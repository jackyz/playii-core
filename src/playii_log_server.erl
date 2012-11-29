%%%-------------------------------------------------------------------
%%% File    : playii_log_server.erl
%%% Author  : jacky zhao <>
%%% Description : 
%%%
%%% Created : 18 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------
-module(playii_log_server).

-export([start_link/0]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% Test
%%====================================================================
-include("playii.hrl").

-ifdef(TEST).

do_test() ->
    {ok, Pid} = gen_server:start(playii_log_server, [], []),
    gen_server:cast(Pid, {log, ?MODULE, 'DEBUG', "message"}),
    %% ?log_debug("hello log", []),
    ok.

-endif.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?Logger}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({log, M, L, S}, State) ->
    T = now(),
    {{YY,MO,DD},{HH,MI,SS}} = calendar:now_to_local_time(T),
    io:format(
      user,
      "~4..0w.~2..0w.~2..0w-~2..0w:~2..0w:~2..0w[~5s][~s]:~s~n",
      [YY, MO, DD, HH, MI, SS, L, M, S]),
    %% playii_db:save(#log{time=T, module=M, level=L, message=S}),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
