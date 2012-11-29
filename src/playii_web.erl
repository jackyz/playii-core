%% mochiweb 的回调模块
%% 职责 : 映射用户请求，提供文件服务，或调用相关业务函数
%% 提供 RESTful API :

-module(playii_web).

%% API
-export([start/1, stop/0, loop/2]).

-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).

do_test_() ->
    [
     %% {timeout, 60, fun full/0}
     {timeout, 60, fun half/0}
    ].

full() ->
    Scene = "test_full",
    {ok, Scene} = playii_scene:open(Scene, ["args"]),
    
    timer:sleep(500),

    Id = "user1",
    Param = "param",
    Player = "p_"++Id,

    O0 = open(Id, Param),
    ?debugFmt("open0 | ~p", [O0]),
    ?assert({ok, Player} =:= O0),

    timer:sleep(500),

    O1 = open(Player),
    ?debugFmt("open1 | ~p", [O1]),
    ?assert({ok, Player} =:= O1),

    timer:sleep(500),

    R0 = recv(Player),
    ?debugFmt("recv0 | ~p", [R0]),
    ?assert(timeout =:= R0),
    
    timer:sleep(500),

    S0 = send(Player, Scene, {"enter", []}),
    ?debugFmt("send0 | ~p", [S0]),
    %% ?assert({error, {error, scene_not_found}} =:= S0),
    ?assert(ok =:= S0),

    timer:sleep(500),

    R1 = recv(Player),
    ?debugFmt("recv1 | ~p", [R1]),
    %% ?assert({ok, "[]"} =:= R1),
    
    timer:sleep(?PlayerIdle),
    timer:sleep(500),
    
    O2 = open(Player),
    ?debugFmt("open2 | ~p", [O2]),
    ?assert({error, player_not_found} =:= O2),
    
    ok.

half() ->
    App = "test_half",
    {ok, App} = playii_scene:open(App, ["args"]),
    
    timer:sleep(500),

    Role = "user1",
    Party = "url1",

    P0 = poll(App, Role, Party),
    ?debugFmt("poll0 | ~p", [P0]),
    ?assert(timeout =:= P0),

    timer:sleep(500),

    Func = "test",
    Args = "param",
    S0 = push(App, Party, Func, Args),
    ?debugFmt("push0 | ~p", [S0]),
    ?assert(ok =:= S0),
    
    timer:sleep(500),

    P1 = poll(App, Role, Party),
    ?debugFmt("poll1 | ~p", [P1]),
    ?assert({ok, [{App, "test", ["param"]}]} =:= P1),
    {ok, Data} = P1,
    Json = enc_data(Data),
    ?debugFmt("~p -> ~s",[Data, Json]),

    ok.
    

-endif.

%%====================================================================
%% API
%%====================================================================

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun(Req) ->
 		   ?MODULE:loop(DocRoot, Req)
 	   end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(DocRoot, Req) ->
    try
        Res = serve_game(Req, Req:get(method), Req:get(path)),
	%% ?debugFmt("~p,~p -> ~p", [Req:get(method), Req:get(path), Res]),
	response(Req, Res)
    catch
	%% _:function_clause ->
	%%    serve_file(DocRoot, Req);
	_:E -> 
	    ?debugVal(E),
	    response(Req, {error, E})
    end.

%%====================================================================
%% internal
%%====================================================================

%%   /_open 用户连接 : '["ok", t]' | 404,error %% 后续 send/recv 请求需带 t
%%     ?id=str&param=str|jsonstr
%%       用户发起新连接，标识为 id 带 param 信息
%%     ?t=str
%%       用户 t 发起重连接
%%   /_recv 用户接收 : '[json,...]' | 404,error
%%     ?t=str
%%       用户 t 接收发送给他的事件 %% LONG POLL
%%   /_send 用户发送 : '[]' | 404,error
%%     ?t=str&s=str&f=str&a=str|jsonstr
%%       用户 t 发事件给服务端 s 场景的 f 函数带 a 参数
%%
%%  参考认证方案
%%     客户端调用 : &_ck=str&_cs=str 客户端可见 _CK
%%       c -> _key, _ck=md5(_key+u), _cs=md5(_ck+s)
%%     服务端调用 : &_sk=str&_ss=str 
%%       c -> _key, _sk=md5(_key+s), _ss=md5(_sk+e)

%% @spec serve_game(Reg, 'GET', "/_open") -> {ok, Data} | {error, Error}
serve_game(Req, 'GET', "/_open") ->
    Player = get_param(Req, "t"),
    if
	%% has token, means a refresh, try to reuse it
	Player =/= undefined ->
	    open(Player);
	%% no token, means a new open request, process it
	true ->
	    Id = get_param(Req, "id"),
	    Param = get_param(Req, "param"),
	    open(Id, Param)
    end;
%% @spec serve_game(Reg, 'GET', "/_recv") -> {ok, Data} | {error, Error}
serve_game(Req, 'GET', "/_recv") ->
    Player = get_param(Req, "t"),
    inet:setopts(Req:get(socket), [{active, once}]),
    recv(Player);
%% @spec serve_game(Reg, 'GET', "/_send") -> {ok, Data} | {error, Error}
serve_game(Req, 'GET', "/_send") ->
    Player = get_param(Req, "t"),
    Scene = get_param(Req, "s"),
    Func = get_param(Req, "f"),
    Args = get_params(Req, "a"),
    send(Player, Scene, {Func, Args});

%%   /_poll 用户轮询 : '[json,...]' | 404,error
%%     ?x=str&r=str&p=str
%%       应用 x 的用户 r 收取 p 场景的消息
%%
%%   /_push 服务下发 : '[]' | 404,error
%%     ?x=str&p=str&f=str&a=str|jsonstr 
%%       应用 x 的服务端通知 p 场景更新( f 函数带 a 参数)
%%
%%  参考认证方案
%%     客户端调用 : &_ck=str&_cs=str 客户端可见 _CK
%%       x -> _key, _ck=md5(_key+r), _cs=md5(_ck+p)
%%     服务端调用 : &_sk=str&_ss=str 
%%       x -> _key, _sk=md5(_key+p), _ss=md5(_sk+f+a)

%% @spec serve_game(Reg, 'GET', "/_poll") -> {ok, Data} | {error, Error}
serve_game(Req, 'GET', "/_poll") ->
    App = get_param(Req, "x"),
    Role = get_param(Req, "r"),
    Party = get_param(Req, "p"),
    %% TODO verify request aginst signature
    inet:setopts(Req:get(socket), [{active, once}]),
    poll(App, Role, Party);
%% @spec serve_game(Reg, 'GET', "/_push") -> {ok, Data} | {error, Error}
serve_game(Req, 'GET', "/_push") ->
    App = get_param(Req, "x"),
    Party = get_param(Req, "p"),
    Func = get_param(Req, "f"),
    Args = get_param(Req, "a"),
    %% TODO verify request aginst signature
    push(App, Party, Func, Args);

%% @spec serve_game(Reg, _, _) -> {ok, Data} | {error, Error}
serve_game(_, _, _) ->
    {error, "File Not Found"}.

%% 服务文件请求
serve_file(DocRoot, Req) ->
    "/"++Path = Req:get(path),
    Method = Req:get(method),
    if
	%% 常规 HTTP 文件服务
	Method =:= 'GET' ->
	    Req:serve_file(Path, DocRoot, [{"Pragma", "no-cache"},
					   {"Cache-Control", "no-cache"}]);
	%% 默认处理 返回 501 错误
	true ->
	    ?debugVal({unknow_req, Req:dump()}),
	    %% Req:not_found()
	    %% Req:serve_file(Path, DocRoot);
	    response(Req, {error, not_found})
    end.

%%====================================================================
%% utils
%%====================================================================

%% @spec get_option(Option, Options) ->
%%     {value, Options2} | {undefined, Options}
%% @doc Retrieve options from options list [{option1, value1}, {option2, value2}]
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%% -------- request

%% get_cookie(Req) ->
%%     Req:get_cookie_value(?WebCookie).

%% mk_cookie(Player) ->
%%     mochiweb_cookies:cookie(?WebCookie, Player, [{path, "/"}]).

get_param(Req, Field) ->
    case Req:get(method) of
	'GET' ->
	    proplists:get_value(Field, Req:parse_qs());
	'POST' ->
	    proplists:get_value(Field, Req:parse_post());
	_ ->
	    undefined
    end.
    
get_params(Req, Field) ->
    case Req:get(method) of
	'GET' ->
	    proplists:get_all_values(Field, Req:parse_qs());
	'POST' ->
	    proplists:get_all_values(Field, Req:parse_post());
	_ ->
	    []
    end.

%% -------- response

response(Req, {error, {error, Error}}) ->
    response(Req, {error, Error});
response(Req, {error, Error}) ->
    Req:respond({404, [], io_lib:format("~s", [Error])});
response(_Req, closed) ->
    ok;
response(Req, timeout) ->
    response(Req, "[]");
response(Req, ok) ->
    response(Req, "[]");
response(Req, {ok, [{_,_,_}|_]=Data}) ->
    response(Req, enc_data(Data));
response(Req, {ok, Player}) ->
    response(Req, "[\"ok\",\""++Player++"\"]");
response(Req, Result) ->
    %% ?debugVal(Result),
    C = get_param(Req, "c"),
    D = case C of
	    undefined -> Result;
	    _ -> C++"("++Result++");"
	end,
    Req:respond({200, [{"Content-Type", "text/javascript"}], D}).


enc_data(Data) ->
    enc_data(Data, []).

enc_data([], Acc) ->
    Acc2 = lists:reverse(Acc),
    mochijson2:encode(Acc2);
enc_data([{Scene,Func,Args}|T], Acc) ->
    enc_data(T, [[enc_scene(Scene), enc_func(Func), enc_args(Args)]|Acc]).

enc_list(List) when is_list(List) -> list_to_binary(List);
enc_list(Any) -> Any.

enc_scene(Scene) -> enc_list(Scene).

enc_func(Func) -> enc_list(Func).

enc_args(Args) -> [enc_list(A) || A <- Args].

%%====================================================================
%% Full Client Mode
%%====================================================================
%% @doc <code>
%%
%%   全客户端风格 : 对 realtime 而言 user 是全功能的逻辑单元，能收，能发
%%   user 在 app 验证，此后 user 根据逻辑，直接向 realtime 发消息
%%
%%     [user]<--------+
%%       ^            | open, recv, send
%%       |            |
%%       |            V
%%      auth      [realtime]
%%       |
%%       |
%%       |
%%       V
%%     [app]
%%
%% </code>

%% 客户端重连
%% @spec open(Player) -> {ok, Player} | {error, player_not_found}
open(Player) ->
    R = playii_player:is_open(Player),
    case R of
	true -> {ok, Player};
	false ->  {error, player_not_found}
    end.

%% 客户端连接
%% @spec open(Id, Param) -> {ok, Player} | {error, E}
open(undefined, Param) ->
    {error, identify_first};
open(Id, Param) ->
    R = playii_player:open(Id, Param),
    case R of
	{ok, Player} -> {ok, Player};
	E ->            {error, E}
    end.

%% 客户端接收
%% @spec recv(Player) -> closed | timeout | {ok, Data} | {error, E}
recv(Player) ->
    case playii_player:take(Player) of
	{error, tcp_closed} -> closed;
	{error, Error} -> {error, Error};
	{ok, timeout} -> timeout;
	{ok, Data} -> {ok, Data};
	Else -> {error, Else}
    end.

%% 客户端发送
%% @spec send(Player, Scene, Event) -> ok | {error, E}
send(Player, Scene, Event) ->
    R = playii_player:fire(Player, Scene, Event),
    case R of
	ok -> ok;
	E -> {error, E}
    end.

%%====================================================================
%% Half Client Mode
%%====================================================================
%% @doc <code>
%%
%%   半客户端风格 : 对 realtime 而言 user 是半功能的逻辑单元，只能收，不能发
%%   user 对 app 请求，app 根据逻辑，可以让 realtime 向 user 发消息
%%
%%     [user]<--------+
%%       |            |
%%       |           poll
%%       |            |
%%     action     [realtime]
%%       |            ^
%%       |            |
%%       |           push
%%       V            |
%%     [app]----------+
%%
%% </code>

%% 客户端 poll
%% @spec poll(App, Role, Party) ->  closed | timeout | {ok, Data} | {error, E}
poll(App, Role, Party) ->
    Id = App ++ "_" ++ Role,
    Player = "p_" ++ Id,
    c([
       %% TODO 确保scene建立 
       %% 确保player建立
       fun() ->
	       case open(Player) of
		   %% 已建，继续
		   {ok, Player} -> ok;
		   %% 未建，新建
		   {error, player_not_found} ->
		       case open(Id, "") of
			   {error, E} -> {error, E};
			   %% 刚建，player向scene发送enter
			   {ok, Player} ->
			       send(Player, App, {"enter", []})
		       end
	       end
       end,
       %% player向scene发送poll消息
       fun() ->
	       send(Player, App, {"poll", [Party]})
       end,
       %% long poll
       fun() ->
	       recv(Player)
       end
      ]).

%% 服务端 push
%% @spec push(App, Party, Func, Args) -> ok
push(App, Party, Func, Args) ->
    c([
       %% TODO 确保scene建立 
       %% 向scene发送push消息
       fun() ->
	       playii_scene:fire(App, {"push", [Party, Func, Args]})
       end
      ]).

%%====================================================================
%% internal
%%====================================================================

%% 链式调用 [fun1, fun2, ...]
c([]) -> ok;
c([H|T]) ->
    case H() of
	ok -> c(T);
	Any -> Any
    end.

