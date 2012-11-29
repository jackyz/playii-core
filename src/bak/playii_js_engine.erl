-module(playii_js_engine).
%% 职责
%% engine 服务进程(每个 js port 一个进程)
%%   提供脚本服务(load, exec, unload)用以实现 scene 的逻辑定制
%%   保持 js port 的状态(port)
%%   保持脚本代码的更新状态(map[{code, lastmodified}])，
%%   实现脚本代码更新自动重载的机制

%% API
%% for scene
-export([load/1, exec/4, unload/1, encode/1, decode/1]).
%% for supervisor
-export([start/0, start_link/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {port, map=[]}).

%%====================================================================
%% Test
%%====================================================================
-include("playii.hrl").
-ifdef(TEST).
-compile(export_all).

do_test() ->
    start_link(),
    
    Code = "/chat/room/scene.js",
    {ok, S0, _, _} = load(Code),
    {ok, S1, _, _} = exec(Code, S0, "enter", ["test0"]),
    {ok, S2, _, _} = exec(Code, S1, "enter", ["test1"]),
    {ok, S3, _, _} = exec(Code, S2, "enter", ["test2"]),
    {ok, S4, _, _} = exec(Code, S3, "enter", ["test3"]),
    {ok, S5, _, _} = exec(Code, S4, "enter", ["test4"]),

    TA1 = now(),
    lists:map(fun(_) ->
                  {ok, S6, Cmd, Log} = exec(Code, S5, "say", ["test0", "hello!"]),
                  ?debugVal(Cmd)
              end, lists:seq(1, 1)),
    TA2 = now(),
    TAX = timer:now_diff(TA2, TA1),
    
    ?debugVal(TAX),

    stop(),
    ok.

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

-endif.

%%====================================================================
%% API
%%====================================================================

%% --------- for scene

%% @spec load(Code) ->
%%     {ok, Data0, Response0} | {error, error()}
%% @doc load code(Code) into engine
%%     if anything error, {error, error()}
%%     else {ok, Data0, Response0={Cmd0, Log0}}
load(Code) ->
    gen_server:call(?MODULE, {load, Code}).

%% @spec exec(Code, Data, Func, Args) ->
%%     {ok, Data2, {Result, Log}} | {error, error()}
%% @doc exec code(Code) where data(Data) with request{Event, Player}
%%     if anything error, {error, error()}
%%     else {ok, Data2, Response={Cmd, Log}}
exec(Code, Data, Func, Args) ->
    gen_server:call(?MODULE, {exec, {Code, Data, Func, Args}}).

%% @spec unload(Code) ->
%%     {ok, {Cmd, Log}} | {error, error()}
%% @doc unload code(Code) from engine
%%     if anything error, {error, error()}
%%     else {ok, Response={Cmd, Log}}
unload(Code) ->
    gen_server:call(?MODULE, {unload, Code}).

encode(P) ->
    playii_js_json:encode(P).

decode(P) ->
    playii_js_json:decode(P).

%% --------- for supervisor

%% @spec start_link() ->
%%   {ok, Pid}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Port = playii_js_port:open(),
    State = #state{port=Port},
    %% process_flag(trap_exit, true),
    {ok, State}.

handle_call({load, Code}, _From, State) ->
    {Reply, State2} = case load_code(Code, State) of
			  {ok, {Data0, Cmds0, Logs0}, S2} ->
			      R = {ok, Data0, Cmds0, Logs0},
			      {R, S2};
			  {error, Err} ->
			      {{error, Err}, State}
		      end,
    {reply, Reply, State2};
handle_call({exec, {Code, Data, Func, Args}}, From, State) ->
    self() ! {self_exec, {Code, Data, Func, Args}, From},
    {noreply, State};
handle_call({unload, _Code}, _From, State) ->
    %% TODO 
    Reply = {ok, ""},
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({self_exec, {Code, Data, Func, Args}, From}, State) ->
    {Reply, State2} = case load_code(Code, State) of
			  {ok, {_Data0, _Cmds0, _Logs0}, S2} ->
			      R2 = playii_js_port:exec(S2#state.port, Code, Data, Func, Args),
			      {R2, S2};
			  {error, Err} ->
			      {{error, Err}, State}
		      end,
    gen_server:reply(From, Reply),
    {noreply, State2};
handle_info(_Info, State) ->
    %% ?debugFmt("!!!! engine shutdown !!!!~n~p~p~n", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% ?debugFmt("!!!! engine shutdown !!!!~n~p~p~n", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

filename(Code) ->
    playii_deps:local_path(["priv", "root"])++Code.

%% load_code(Code, State) ->
%%     {ok, State2} | {error, error()}
load_code(Code, State) ->
    FX = filename(Code),
    case filelib:last_modified(FX) of
	0 ->
	    ?debugVal({file_not_exist, Code, FX}),
	    {error, file_not_exist};
	FD ->
	    FS = calendar:datetime_to_gregorian_seconds(FD),
            R = lists:keysearch(Code, 1, State#state.map),
	    {CFS, Data0} = case R of
		              {value, {Code, Value}} -> Value;
		              _ -> {0, "{}"}
		           end,
	    case (FS > CFS) of
		false ->
		    {ok, {Data0, "", ""}, State};
		_ ->
		    load_code(Code, FS, State)
	    end
    end.

load_code(Code, Fs, State) ->
    {ok, Bin} = file:read_file(filename(Code)),
    case playii_js_port:init(State#state.port, Code, Bin) of
	{ok, Data0, Cmds0, Logs0} ->
	    %% TODO dispatch Log0
	    %% ?debugVal({init_js, Code, ok, Data0, {Cmd0, Log0}}),
	    Map2 = [{Code, {Fs, Data0}} | lists:keydelete(Code, 1, State#state.map)],
	    {ok, {Data0, Cmds0, Logs0}, State#state{map=Map2}};
	{error, Err} ->
	    ?debugVal({init_js, Code, error, Err}),
	    {error, Err}
    end.
