-module(playii_rand).

%% API
-export([start_link/0]).
-export([gen_int/1, gen_str/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% Test
%%====================================================================

-include("playii.hrl").

-ifdef(TEST).

-export([start/0, stop/0]).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

do_test_() ->
    {setup,
     fun() -> start() end,
     fun(_) -> stop() end,
     [{"test1",
       ?_test(begin
		  I1 = gen_int(10),
		  ?debugVal(I1),
		  I2 = gen_int(10),
		  ?debugVal(I2),
		  ?assert(I1 /= I2),
		  S1 = gen_str(10),
		  ?debugVal(S1),
		  S2 = gen_str(10),
		  ?debugVal(S2),
		  ?assert(S1 /= S2)
	      end)}
     ]}.

-endif.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

gen_int(N) ->
    gen_server:call(?MODULE, {gen_int, N}).

gen_str(N) ->
    gen_server:call(?MODULE, {gen_str, N}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    State = now(), %random:seed0(),
    {ok, State}.

handle_call({gen_int, N}, _From, State) ->
    {Reply, State1} = i_gen_int(N, State),
    {reply, Reply, State1};
handle_call({gen_str, N}, _From, State) ->
    {Reply, State1} = i_gen_str(N, State),
    {reply, Reply, State1}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% {int([1~Max]), State1}
i_gen_int(Max, State) ->
    random:uniform_s(Max, State).

%% {string(Len), State1}
i_gen_str(Len, State) ->
    i_gen_str([], State, Len).
i_gen_str(L, State, 0) ->
    {L, State};
i_gen_str(L, State, Len) ->
    {Char, State1} = i_gen_int(26, State),
    i_gen_str(L ++ [Char+$a-1], State1, Len-1).

%% %% {list(ShuffledList), State1}
%% shuffle(List) ->
%%   randomize(round(math:log(length(List)) + 0.5), List).

%% randomize(1, List) ->
%%   randomize(List);
%% randomize(T, List) ->
%%   lists:foldl(fun(_E, Acc) -> randomize(Acc) end, randomize(List), lists:seq(1, (T - 1))).

%% randomize(List) ->
%%   D = lists:map(fun(A) -> {random:uniform(), A} end, List),
%%   {_, D1} = lists:unzip(lists:keysort(1, D)), 
%%   D1.
