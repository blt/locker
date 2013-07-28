-module(lk).

-export([
         set/1,
         set/2,
         del/1,
         set_size/2,
         get_size/1
        ]).

-type lk_opts() :: any().

-export_type([
              lk_opts/0
             ]).

%% ===================================================================
%%  API
%% ===================================================================

-spec set(Name :: any()) -> undefined.
set(Name) ->
    set(Name, []).

-spec set(Name :: any(), Options :: [lk_opts()]) -> undefined.
set(_Name, _Options) ->
    undefined.

-spec del(Name :: any()) -> undefined.
del(_Name) ->
    undefined.

-spec set_size(Name :: any(), Size :: pos_integer()) -> undefined.
set_size(_Name, Size) when Size > 0 ->
    undefined.

-spec get_size(Name :: any()) -> 0.
get_size(_Name) ->
    0.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

set_test_() ->
    [
     ?_assertMatch(undefined, set(name)),
     ?_assertMatch(undefined, set(name, []))
    ].

del_test_() ->
    [
     ?_assertMatch(undefined, del(name))
    ].

size_test_() ->
    [
     ?_assertMatch(undefined, set_size(name, 1)),
     ?_assertMatch(0,         get_size(name))
    ].

-endif.
