%%%-------------------------------------------------------------------
%%% File    : playii_state_mock.erl
%%% Author  : jacky zhao <>
%%% Description : state the mock
%%% Created : 14 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------

-module(playii_state_mock).

%% API
-export([load/2, save/3]).

-include("playii.hrl").

%%====================================================================
%% API
%%====================================================================

%% @spec load(UserId, SceneName) -> State | undefined
load(_Id, _Scene) ->
    % TODO
    <<"{}">>.

%% @spec save(UserId, SceneName, Data) -> ok
save(_Id, _Scene, _Data) ->
    % TODO
    ok.

