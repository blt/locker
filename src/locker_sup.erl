-module(locker_sup).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-behaviour(supervisor).
-export([init/1]).

-define(SCOPE, global).

%% ===================================================================
%%  API
%% ===================================================================

start_link() ->
    supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []).

%% ===================================================================
%%  Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sanity_test_() ->
    [
     ?_assertMatch(global, ?SCOPE)
    ].

callback_test_() ->
    [
     ?_assertMatch({ok, {{one_for_one, _, _}, []}}, init([]))
    ].

-endif.
