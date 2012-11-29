-module(playii_js_port).
%% 职责
%% 包装操纵 js port 的代码(并非独立的进程)
%%   提供 open, init, exec, close 通讯的实现

%% API
-export([open/0, load/2, init/3, exec/5, close/1]).

-define(JsServerCmd, "./js script/playii.js").
-define(JsRootDir, "./priv/").
-define(JsTimeout, 5000).

-define(last(Code), ("last_"++Code)).
%%  脚本文件的最后更新时间暂存在 process dict 

%%====================================================================
%% Test
%%====================================================================

-include("playii.hrl").
-ifdef(TEST).

do_test() ->
    Code = "/test/full.js",
    Port = open(),
    {ok, _, _} = load(Port, Code),
    {ok, S0, _, _} = init(Port, Code, ["args"]),
    {ok, S1, _, _} = exec(Port, Code, S0, "enter", ["test0"]),
    {ok, S2, _, _} = exec(Port, Code, S1, "enter", ["test1"]),
    {ok, S3, _, _} = exec(Port, Code, S2, "enter", ["test2"]),
    {ok, S4, _, _} = exec(Port, Code, S3, "enter", ["test3"]),
    {ok, S5, _, _} = exec(Port, Code, S4, "enter", ["test4"]),
    %% simple bench mark
    N = 20,
    TA1 = now(),
    lists:map(fun(_) ->
		      exec(Port, Code, S5, "say", ["test0","hi!"])
 	      end, lists:seq(1, N)),
    TA2 = now(),
    TAX = timer:now_diff(TA2, TA1),
    ?debugFmt("~7.2fms/~p => ~4.2fms once", [TAX/1000, N, TAX/N/1000]),
    close(Port).

-endif.

%%====================================================================
%% API
%%====================================================================

open() ->
    i_open(?JsRootDir, ?JsServerCmd).

load(Port, Code) ->
    F = file_name(Code),
    %% ?debugVal(F),
    case file:read_file(F) of
	{ok, Source} ->
	    R = i_load(Port, Code, Source),
	    case R of
		{ok, _, _} ->
		    put(?last(Code), file_date(Code)),
		    R;
		_ ->
		    R
	    end;
	{error, Reason} ->
	    ?debugVal(Reason),
	    {error, Reason}
    end.

init(Port, Code, Args) ->
    FT = file_date(Code),
    LT = get(?last(Code)),
    if
	FT =/= LT ->
	    ?debugFmt("reload ~p", [Code]),
	    %% reload if changed
	    R = load(Port, Code),
	    case R of
		{ok, _, _} = R ->
		    put(?last(Code), FT),
		    i_init(Port, Code, Args);
		true ->
		    R
	    end;
	true ->
	    i_init(Port, Code, Args)
    end.
    
exec(Port, Code, Data, Func, Args) ->
    FT = file_date(Code),
    LT = get(?last(Code)),
    if
	FT =/= LT ->
	    ?debugFmt("reload ~p", [Code]),
	    %% reload if changed
	    R = load(Port, Code),
	    case R of
		{ok, _, _} = R ->
		    put(?last(Code), FT),
		    i_exec(Port, Code, Data, Func, Args);
		true ->
		    R
	    end;
	true ->
	    i_exec(Port, Code, Data, Func, Args)
    end.

close(Port) ->
    i_close(Port).


%% ---- js command layer

%%  port protocl
%%  load
%%    -->
%%      "[load, code, source]\n"
%%    <--
%%      "[load, 1, command, log]\n"
%%      "[load, 0, error]\n"
%%  init
%%    -->
%%      "[init, code, args]\n"
%%    <--
%%      "[init, 1, state0, command, log]\n"
%%      "[init, 0, error]\n"
%%  exec
%%    -->
%%      "[exec, code, state1, func, args]\n"
%%    <--
%%      "[exec, 1, state2, command, log]\n"
%%      "[exec, 0, error]\n"
%%  any
%%    -->
%%      "...\n"
%%    <--
%%      "[any, 0, error]\n"

%% ---- port operate layer

i_open(Root, Cmd) ->
    open_port({spawn, Cmd},
	      [stream, 
	       {line, 1000},
	       {cd, Root},
	       exit_status, 
	       hide]).

i_load(Port, Code, Source) ->
    S1 = mochijson2:encode([<<"load">>, enc_code(Code), enc_source(Source)]),
    S2 = command(Port, S1),
    R = mochijson2:decode(S2),
    case R of
	[_, 0, JsError] -> %% throw({js_error, Error});
	    {error, dec_error(JsError)};
	[<<"load">>, 1, JsCmds, JsLogs] ->
	    {ok, dec_cmds(JsCmds), dec_logs(JsLogs)}
    end.

i_init(Port, Code, Args) ->
    S1 = mochijson2:encode([<<"init">>, enc_code(Code), enc_args(Args)]),
    S2 = command(Port, S1),
    R = mochijson2:decode(S2),
    case R of
	[_, 0, JsError] -> %% throw({js_error, Error});
	    {error, dec_error(JsError)};
	[<<"init">>, 1, Data, JsCmds, JsLogs] ->
	    {ok, dec_data(Data), dec_cmds(JsCmds), dec_logs(JsLogs)}
    end.

i_exec(Port, Code, Data, Func, Args) ->
    S1 = mochijson2:encode([<<"exec">>, enc_code(Code), enc_data(Data), enc_func(Func), enc_args(Args)]),
    S2 = command(Port, S1),
    R = mochijson2:decode(S2),
    case R of
	[_, 0, JsError] -> %% throw({js_error, Error});
	    {error, dec_error(JsError)};
	[<<"exec">>, 1, Data2, JsCmds, JsLogs] ->
	    {ok, dec_data(Data2), dec_cmds(JsCmds), dec_logs(JsLogs)}
    end.

i_close(Port) ->
    port_close(Port).

%%--------------------------------------------------------------------
%%% encode/decode
%%--------------------------------------------------------------------

enc_list(List) when is_list(List) -> list_to_binary(List);
enc_list(Any) -> Any.

enc_code(Code) -> enc_list(Code).

enc_source(Source) -> enc_list(Source).

enc_data(Data) -> enc_list(Data).

enc_func(Func) -> enc_list(Func).
    
enc_args(Args) -> [enc_list(Arg) || Arg <- Args].

%% ----

dec_bin(Bin) when is_binary(Bin) -> binary_to_list(Bin);
dec_bin(Any) -> Any.

dec_error(JsError) -> dec_bin(JsError).

dec_data(JsData) -> JsData. %% dec_bin(JsData).

dec_cmds(JsCmds) -> [dec_cmd(JsCmd) || JsCmd <- mochijson2:decode(JsCmds)].

dec_cmd(JsCmd) -> [dec_field(F) || F <- JsCmd].

dec_field(F) when is_list(F) -> [dec_bin(X) || X <- F];
dec_field(Any) -> dec_bin(Any).
    
dec_logs(JsLogs) -> [dec_bin(JsLog) || JsLog <- mochijson2:decode(JsLogs)].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

file_name(Code) ->
    playii_deps:local_path(["priv/root"++Code]).

file_date(Code) ->
    filelib:last_modified(file_name(Code)).

%% ---- port readline/writeline proctool layer

command(Port, Req) ->
    writeline(Port, Req),
    Res = readline(Port),
    %%?debugFmt("~n--> ~s~n<-- ~s", [Req, Res]),
    Res.

writeline(Port, String) ->
    true = port_command(Port, String ++ "\n").

readline(Port) ->
    readline(Port, []).
	
readline(Port, Acc) ->
    Timer = erlang:send_after(?JsTimeout, self(), timeout),
    Result =
	receive
	    {Port, {data, {noeol, Data}}} ->
		readline(Port, [Data|Acc]);
	    {Port, {data, {eol, Data}}} ->
		lists:flatten(lists:reverse(Acc, Data));
                %% lists:reverse(Acc, Data);
	    {Port, Err} ->
		?debugFmt("!!! js error ~p", [Err]),
		catch port_close(Port),
		erlang:cancel_timer(Timer),
		throw({js_error, Err});
	    timeout ->
		?debugFmt("!!! js timeout", []),
		catch port_close(Port),
		throw({js_error, "timed out"})
	end,
    case erlang:cancel_timer(Timer) of
	false ->
	    %% message already sent. clear it
	    receive timeout -> ok end;
	_ -> 
	    ok
    end,
    Result.
