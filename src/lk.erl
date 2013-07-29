-module(lk).

-export([
         set/1,
         del/1,
         set_size/2,
         get_size/1
        ]).

-type lk_opts() :: any().

%% ===================================================================
%%  API
%% ===================================================================

-spec set(Name :: any()) -> undefined.
set(Name) ->
    set(Name, []).

-spec del(Name :: any()) -> undefined.
del(Name) ->
    {ok, Pid} = locker_sup:lk(Name),
    gen_server:call(Pid, del_lock).

-spec set_size(Name :: any(), Size :: pos_integer()) -> ok.
set_size(Name, Size) when Size > 0 ->
    {ok, Pid} = locker_sup:lk(Name),
    gen_server:call(Pid, {set_size, Size}).

-spec get_size(Name :: any()) -> {ok, non_neg_integer()}.
get_size(Name) ->
    {ok, Pid} = locker_sup:lk(Name),
    gen_server:call(Pid, get_size).

%% ===================================================================
%%  Internal Functions
%% ===================================================================

-spec set(Name :: any(), Options :: [lk_opts()]) -> undefined.
set(Name, _Options) ->
    {ok, Pid} = locker_sup:lk(Name),
    gen_server:call(Pid, set_lock).

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integration_test_() ->
    {setup,
     fun() -> _ = application:start(locker) end,
     fun(_) -> _ = application:stop(locker) end,
     [
      { "supervisor lock retrieval",
        [
         ?_assertMatch({ok, _Pid}, locker_sup:lk(locklock)),
         ?_assertMatch({ok, _Pid}, locker_sup:lk(locklock))
        ]
      },
      { "set / del tests",
        [
         ?_assertMatch(ok, set(name)),
         ?_assertMatch({error, already_held}, set(name, [])),
         ?_assertMatch(ok, del(name)),
         ?_assertMatch({error, not_held}, del(name)),
         ?_assertMatch(ok, set(name)),
         ?_assertMatch({error, already_held}, set(name))
        ]
      },
      { "size tests",
        [
         ?_assertMatch({ok, 3}, get_size(name)),
         ?_assertMatch(ok,      set_size(name, 1)),
         ?_assertMatch({ok, 1}, get_size(name))
        ]
      }
     ]
    }.

-endif.
