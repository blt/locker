-module(locker_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
         all/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         groups/0, fsm_test/1, loop_fsm/2
        ]).

-define(RESOURCES, 10).
-define(TESTTOTS, 1000).
-define(SLEEPWAIT, 5).

all() -> [
          {group, distributed}
         ].

init_per_suite(Config) ->
    ok = application:start(locker),
    Config.

init_per_group(distributed, Config) ->
    Resources = lists:seq(1, ?RESOURCES),
    [{names, Resources} | Config].

end_per_group(_Group, _Config) ->
    ok.

end_per_suite(Config) ->
    ok = application:stop(locker),
    Config.

groups() ->
    [
     {distributed,
      [parallel, {repeat_until_any_fail, 5}],
      lists:duplicate(?TESTTOTS, fsm_test)
     }
    ].

fsm_test(Config) ->
    ok = loop_fsm(init, Config).

%% ===================================================================
%%  Test Helpers
%% ===================================================================

loop_fsm(init, Config) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    Resource = choose(?config(names, Config), ?RESOURCES),
    NextStates = [take_lock, take_lock_timeout, del_unheld],
    loop_fsm(choose(NextStates, 3), Resource);
loop_fsm(take_lock_timeout, Resource) ->
    case lk:set(Resource, [{timeout, random:uniform(?SLEEPWAIT)}]) of
        ok ->
            NextStates = [timed_del_lock, timed_del_lock_cooldown, timed_sit_on_lock],
            loop_fsm(choose(NextStates, 2), Resource);
        {error, overburdened} ->
            loop_fsm({wait_for_resource, 10}, Resource)
    end;
loop_fsm(take_lock, Resource) ->
    case lk:set(Resource) of
        ok ->
            NextStates = [del_lock, del_lock_cooldown, sit_on_lock],
            loop_fsm(choose(NextStates, 2), Resource);
        {error, overburdened} ->
            loop_fsm({wait_for_resource, 10}, Resource)
    end;
loop_fsm({wait_for_resource, 0}, Resource) ->
    _ = wait(),
    loop_fsm(take_lock, Resource);
loop_fsm({wait_for_resource, N}, Resource) ->
    _ = wait(),
    loop_fsm({wait_for_resource, N-1}, Resource);
loop_fsm(timed_sit_on_lock, Resource) ->
    _ = wait(),
    NextStates = [timed_del_lock, timed_del_lock_cooldown, timed_sit_on_lock],
    loop_fsm(choose(NextStates, 2), Resource);
loop_fsm(sit_on_lock, Resource) ->
    _ = wait(),
    NextStates = [del_lock, del_lock_cooldown, sit_on_lock],
    loop_fsm(choose(NextStates, 2), Resource);
loop_fsm(del_unheld, Resource) ->
    {error, not_held} = lk:del(Resource),
    ok;
loop_fsm(timed_del_lock, Resource) ->
    case lk:del(Resource) of
        ok ->
            ok;
        {error, not_held} ->
            ok
    end;
loop_fsm(timed_del_lock_cooldown, Resource) ->
    case lk:del(Resource, [{cooldown, ?SLEEPWAIT}]) of
        ok ->
            ok;
        {error, not_held} ->
            ok
    end;
loop_fsm(del_lock, Resource) ->
    ok = lk:del(Resource);
loop_fsm(del_lock_cooldown, Resource) ->
    ok = lk:del(Resource, [{cooldown, ?SLEEPWAIT}]).

%% ===================================================================
%%  Internal Functions
%% ===================================================================

choose(L, Len) ->
    lists:nth(random:uniform(Len), L).

wait() -> timer:sleep(random:uniform(?SLEEPWAIT)).
