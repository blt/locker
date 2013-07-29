-module(locker_sup).

%% API
-export([
         start_link/0,
         lk/1
        ]).

%% Supervisor callbacks
-behaviour(supervisor).
-export([
         init/1
        ]).

-define(SCOPE, global).

%% ===================================================================
%%  API
%% ===================================================================

start_link() ->
    supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []).

-spec lk(Name :: any()) -> {ok, pid()}.
lk(Name) ->
    ChildSpec = {Name, {gen_server, start_link,
                        [{?SCOPE, Name}, lk_proc, [], []]}, permanent,
                 timer:seconds(5), worker, [lk_proc]},
    case supervisor:start_child({?SCOPE, ?MODULE}, ChildSpec) of
        {ok, Child} -> {ok, Child};
        {error, {already_started, Child}} -> {ok, Child}
    end.

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
