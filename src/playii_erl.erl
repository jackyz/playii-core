%%%-------------------------------------------------------------------
%%% File    : playii_erl.erl
%%% Author  : jacky zhao <>
%%% Description : playii_erl the erlang evaluator
%%% Created : 14 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------

-module(playii_erl).

%% API
-export([open/0, load/1, init/2, exec/4, unload/1, close/0]).

-define(Name, erl_node).

-define(Node, erl_node).

-define(beam(Code), ("beam_"++Code)).
-define(last(Code), ("last_"++Code)).
%% .last
%%   scene脚本文件的最后更新时间(无保存价值)

-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test_() ->
    [
     {timeout, 60, fun t2/0}
    ].

t2() ->    
    Scene = "test_half",
    ok = open(),
    {ok, _, _} = load(Scene),
    {ok, S0, _, _} = init(Scene, ["god", "/test/half"]),
    {ok, S1, _, _} = exec(Scene, S0, "enter", ["u0", "p0", "d0"]),
    {ok, S2, _, _} = exec(Scene, S1, "enter", ["u1", "p1", "d0"]),
    {ok, S3, _, _} = exec(Scene, S2, "poll", ["u0", "url1"]),
    {ok, S4, _, _} = exec(Scene, S3, "poll", ["u1", "url1"]),
    {ok, S5, _, _} = exec(Scene, S4, "push", ["url1", "f", "a"]),
    ok = unload(Scene),
    close().

-endif.

%%====================================================================
%% API
%%====================================================================

%% @spec open() -> ok | throw error()
open() ->
    Host = list_to_atom(net_adm:localhost()),
    {ok, Node} = slave:start_link(Host, ?Name),
    put(?Node, Node),
    ok.

%% @spec load(Scene) -> {ok, Cmds, Logs} | {error, error()}
load(Scene) ->
    r_load(code_name(Scene)).
    %% rpc:call(get(?Node), ?Module, load, [code_name(Scene)]).
    
%% @spec init(Scene, Args) -> {ok, Data, Cmds, Logs} | {error, error()}
init(Scene, Args) ->
    r_init(code_name(Scene), Args).
    %% rpc:call(get(?Node), ?Module, init, [code_name(Scene), Args]).

%% @spec exec(Scene, Data, Func, Args) -> {ok, Data2, Cmds, Logs} | {error, error()}
exec(Scene, Data, Func, Args) ->
    ?mark(fire),
    R = r_exec(code_name(Scene), Data, Func, Args),
    %% R = rpc:call(get(?Node), ?Module, exec, [code_name(Scene), Data, Func, Args]),
    ?pass(fire),
    R.

%% @spec unload(Scene) -> ok
unload(Scene) ->
    ?debugFmt("total exec time :~p", [?time(fire)]),
    r_unload(code_name(Scene)).
    %% rpc:call(get(?Node), ?Module, unload, [code_name(Scene)]).

%% @spec close() -> ok
close() ->
    slave:stop(get(?Node)).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%---- remote

%% @spec load(Code) -> {ok, Cmds, Logs} | {error, error()}
r_load(Code) ->
    %%?debugFmt("erl_load(~p)", [Code]),
    %%CR = compile:file(file_name(Code), [binary]),
    CR = rpc:call(get(?Node), compile, file, [file_name(Code), [binary]]),
    %%?debugVal(CR),
    case CR of
	{ok, ModuleName, Binary} ->
	    %%LR = code:load_binary(ModuleName, Code, Binary),
	    LR = rpc:call(get(?Node), code, load_binary, [ModuleName, Code, Binary]),
	    %%?debugVal(LR),
	    {module, ModuleName} = LR,
	    put(?beam(Code), ModuleName),
	    ?log_info("erl_load(~p) -> ~p", [Code, ModuleName]),
	    {ok, [], []};
	E ->
	    ?log_error("erl_load(~p) fail:~p", [Code, E]),
	    {error, E}
    end.
    
    
%% @spec init(Code, Args) -> {ok, Data, Cmds, Logs} | {error, error()}
r_init(Code, Args) ->
    %%?debugFmt("erl_init(~p, ~p)", [Code, Args]),
    ModuleName = get(?beam(Code)),
    try
	%%IR = apply(ModuleName, init, [Args]),
	IR = rpc:call(get(?Node), ModuleName, init, [Args]),
	%%?debugVal(IR),
	{Data, Cmds, Logs} = IR,
	{ok, Data, Cmds, Logs}
    catch
	_:E -> 
	    ?log_error("erl_init(~p) fail:~p", [Code, E]),
	    {error, E}
    end.

%% @spec exec(Code, Data, Func, Args) -> {ok, Data2, Cmds, Logs} | {error, error()}
r_exec(Code, Data, Func, Args) ->
    %%?debugFmt("erl_exec(~p, ~p, ~p, ~p)", [Code, Data, Func, Args]),
    ModuleName = get(?beam(Code)),
    try
	%%ER = apply(ModuleName, exec, [Data, Func, Args]),
	ER = rpc:call(get(?Node), ModuleName, exec, [Data, Func, Args]),
	%%?debugVal(ER),
	{Data2, Cmds, Logs} = ER,
	{ok, Data2, Cmds, Logs}
    catch
	_:E -> 
	    ?log_error("erl_exec(~p) fail:~p", [Code, E]),
	    {error, E}
    end.

%% @spec unload(Code) -> ok
r_unload(Code) ->
    %%?debugFmt("erl_unload(~p)", [Code]),
    ModuleName = get(?beam(Code)),
    %%code:purge(ModuleName),
    rpc:call(?Node, code, purge, [ModuleName]),
    %%code:delete(ModuleName),
    rpc:call(get(?Node), code, delete, [ModuleName]),
    erase(Code),
    ok.

%% ---- file

file_name(Code) ->
    playii_deps:local_path(["priv/root"++Code]).

file_date(Code) ->
    filelib:last_modified(file_name(Code)++".erl").

%%---- code mapping

code_name(Scene) ->
    %% TODO test here
    A = string:tokens(Scene, "_"),
    if
	length(A) >= 2 ->
	    "/"++lists:nth(1,A)++"/"++lists:nth(2,A);
	true ->
	    "/"++Scene++"/main"
    end.

