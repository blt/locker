-module(locker_app).

%% Application callbacks
-behaviour(application).
-export([
         start/2,
         stop/1
        ]).

%% ===================================================================
%%  Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    locker_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

callback_test_() ->
    [
     ?_assertMatch(ok, stop(any_term)),
     ?_assertMatch({ok, _Pid}, start(any_start, any_args))
    ].

-endif.
