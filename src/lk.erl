-module(lk).

-export([
         set/1,
         set/2,
         del/1,
         del/2,
         set_size/2,
         get_size/1
        ]).

-type lk_set_opts() :: {timeout, timer:time() | infinity}.
-type lk_del_opts() :: {cooloff_timeout, timer:time()}.

-export_type([lk_set_opts/0, lk_del_opts/0]).

%% ===================================================================
%%  API
%% ===================================================================

-spec set(Name :: any()) -> ok.
set(Name) ->
    set(Name, []).

-spec set(Name :: any(), Options :: [lk_set_opts()]) -> ok.
set(Name, Options) ->
    {ok, Pid} = locker_sup:lk(Name),
    {timeout, Timeout} = timeout(Options),
    gen_server:call(Pid, {set_lock, Timeout}).

-spec del(Name :: any()) -> ok.
del(Name) ->
    del(Name, []).

-spec del(Name :: any(), Options :: [lk_del_opts()]) -> ok.
del(Name, Options) ->
    {ok, Pid} = locker_sup:lk(Name),
    {timeout, Cooldown} = cooldown(Options),
    gen_server:call(Pid, {del_lock, Cooldown}).

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

-spec timeout([lk_set_opts()]) -> {timeout, pos_integer() | infinity}.
timeout(Options) ->
    case proplists:get_value(timeout, Options, infinity) of
        infinity ->
            {timeout, infinity};
        I when is_integer(I), I > 0 ->
            {timeout, I}
    end.

-spec cooldown([lk_del_opts()]) -> {timeout, pos_integer() | 0}.
cooldown(Options) ->
    case proplists:get_value(cooldown, Options, 0) of
        0 ->
            {timeout, 0};
        I when is_integer(I), I > 0 ->
            {timeout, I}
    end.

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
      { "size tests",
        [
         ?_assertMatch({ok, 3}, get_size(size_test)),
         ?_assertMatch(ok,      set_size(size_test, 1)),
         ?_assertMatch({ok, 1}, get_size(size_test))
        ]
      },
      { "set / del tests",
        [
         ?_assertMatch(ok, set(name)),
         ?_assertMatch({error, already_held}, set(name, [])),
         ?_assertMatch(ok, del(name)),
         ?_assertMatch({error, not_held}, del(name)),
         ?_assertMatch(ok, set(name)),
         ?_assertMatch({error, already_held}, set(name)),
         ?_assertMatch(ok, del(name)),

         ?_assertMatch(ok, set_size(long_delay, 1)),
         ?_assertMatch(ok, set(long_delay)),
         ?_assertMatch(ok, del(long_delay, [{cooldown, timer:seconds(60)}])),
         ?_assertMatch({error, overburdened}, set(long_delay)),

         ?_assertMatch(ok, set(name, [{timeout, timer:seconds(60)}])),
         ?_assertMatch({error, already_held}, set(name)),
         ?_assertMatch(ok, del(name)),
         ?_assertMatch(ok, set(name, [{timeout, 1}])),
         ?_assertMatch(ok, set(name)),
         ?_assertMatch(ok, del(name))
        ]
      }
     ]
    }.

-endif.
