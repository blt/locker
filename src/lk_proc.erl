-module(lk_proc).

-behaviour(gen_server).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-type tref() :: timer:tref() | infinity.

-record(state, {
          abs_size = 3 :: pos_integer(),
          consumed_locks = 0 :: non_neg_integer(),
          lock_holders = [] :: [{pid(), {reference(), tref()}}]
         }).

%% ===================================================================
%%  API
%% ===================================================================

%% ===================================================================
%%  gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{}}.

%% lock setting
handle_call({set_lock, _Timeout}, _From, #state{abs_size=N, consumed_locks=M}=S)
  when N =< M ->
    {reply, {error, overburdened}, S};
handle_call({set_lock, Timeout}, {Pid, _Tag}=_From,
            #state{abs_size=N, consumed_locks=M}=S)
  when N > M, Timeout > 0 ->
    case proplists:get_value(Pid, S#state.lock_holders) of
        undefined ->
            MonRef = erlang:monitor(process, Pid),
            TRef = create_timer(Timeout, Pid),
            NewState = S#state{consumed_locks = M+1,
                               lock_holders = [{Pid, {MonRef, TRef}} |
                                               S#state.lock_holders]},
            {reply, ok, NewState};
        _MonRef ->
            {reply, {error, already_held}, S}
    end;

%% lock deleting
handle_call({del_lock, _}, _From, #state{consumed_locks=0, lock_holders=[]}=S) ->
    {reply, {error, not_held}, S};
handle_call({del_lock, N}, {Pid, _Tag}=_From, #state{consumed_locks=M}=S)
  when M > 0 ->
    case proplists:get_value(Pid, S#state.lock_holders) of
        undefined ->
            {reply, {error, not_held}, S};
        {MonRef, TRef} ->
            ok = cancel_timer(TRef),
            _DidFlush = erlang:demonitor(MonRef, [flush]),
            Holders = proplists:delete(Pid, S#state.lock_holders),
            case N of
                0 ->
                    {reply, ok, S#state{consumed_locks=M-1, lock_holders=Holders}};
                I when is_integer(I), I > 0 ->
                    CoolTRef = create_timer(I, self()),
                    NewHolders = [{self(), {cooldown, CoolTRef}} | Holders],
                    {reply, ok, S#state{lock_holders=NewHolders}}
            end
    end;

%% size queries and modification
handle_call(get_size, _From, #state{abs_size=Int}=S) ->
    {reply, {ok, Int}, S};
handle_call({set_size, Int}, _From, #state{}=S) when Int > 0 ->
    {reply, ok, S#state{abs_size=Int}}.

%% timeouts
handle_cast({timeout, _Pid}, #state{consumed_locks=0}=S) ->
    {noreply, S};
handle_cast({timeout, Pid}, #state{consumed_locks=M}=S) when M > 0 ->
    case proplists:get_value(Pid, S#state.lock_holders) of
        undefined ->
            {noreply, S};
        {cooldown, _TRef} ->
            Holders = proplists:delete(Pid, S#state.lock_holders),
            {noreply, S#state{consumed_locks=M-1, lock_holders=Holders}};
        {MonRef, _TRef} ->
            _DidFlush = erlang:demonitor(MonRef, [flush]),
            Holders = proplists:delete(Pid, S#state.lock_holders),
            {noreply, S#state{consumed_locks=M-1, lock_holders=Holders}}
    end;

handle_cast(_Request, #state{}=S) ->
    {noreply, S}.

handle_info({'DOWN', _MonRef, process, _Pid, _Info},
            #state{consumed_locks=0, lock_holders=[]}=S) ->
    {noreply, S};
handle_info({'DOWN', MonRef, process, Pid, _Info}, #state{consumed_locks=M}=S)
  when M > 0->
    case proplists:get_value(Pid, S#state.lock_holders) of
        undefined ->
            {noreply, S};
        {MonRef, TRef} ->
            ok = cancel_timer(TRef),
            true = erlang:demonitor(MonRef, []),
            Holders = proplists:delete(Pid, S#state.lock_holders),
            {noreply, S#state{consumed_locks=M-1, lock_holders=Holders}}
    end.

terminate(_Reason, #state{}=_S) ->
    ok.

code_change(_OldVsn, #state{}=_S, _Extra) ->
    {error, not_implemented}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

-spec create_timer(pos_integer(), pid()) -> tref().
create_timer(infinity, _Pid) ->
    infinity;
create_timer(Timeout, Pid) when is_integer(Timeout), is_pid(Pid) ->
    {ok, TRef} = timer:apply_after(Timeout, gen_server, cast,
                                   [self(), {timeout, Pid}]),
    TRef.

-spec cancel_timer(tref()) -> ok.
cancel_timer(infinity) ->
    ok;
cancel_timer(TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.

%% ===================================================================
%%  test
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

timer_test_() ->
    Self = self(),
    OneSecond = timer:seconds(1),

    [
     ?_assertMatch(ok, cancel_timer(create_timer(infinity, Self))),
     ?_assertMatch(ok, cancel_timer(create_timer(OneSecond, Self)))
    ].

protocol_test_() ->
    Self = self(),
    Tag = tag,
    From = {Self, Tag},
    TRef = tref,

    S = #state{},
    DefaultSize = S#state.abs_size,

    Overburdened = S#state{abs_size=0, consumed_locks=1},
    MonRef = erlang:monitor(process, Self),
    LockHolder   = S#state{consumed_locks=1, lock_holders=[{Self, {MonRef, infinity}}]},
    Cooldown     = LockHolder#state{lock_holders=[{Self, {cooldown, TRef}}]},

    [
     { "setting locks",
       [
        ?_assertMatch({reply, {error, overburdened}, Overburdened},
                      handle_call({set_lock, infinity}, from, Overburdened)),
        ?_assertMatch({reply, {error, already_held}, LockHolder},
                      handle_call({set_lock, infinity}, From, LockHolder)),
        ?_assertMatch({reply, ok, #state{consumed_locks=1, lock_holders=[{Self, _}]}},
                      handle_call({set_lock, infinity}, From, S))
       ]
     },
     { "deleting locks",
       [
        ?_assertMatch({reply, {error, not_held}, S},
                      handle_call({del_lock, 0}, From, S)),
        ?_assertMatch({reply, {error, not_held}, LockHolder},
                      handle_call({del_lock, 0}, {not_holder, Tag}, LockHolder)),
        ?_assertMatch({reply, ok, S},
                      handle_call({del_lock, 0}, From, LockHolder)),

        ?_assertMatch({noreply, S},
                      handle_cast({timeout, Self}, LockHolder)),
        ?_assertMatch({noreply, S},
                      handle_cast({timeout, Self}, S)),
        ?_assertMatch({noreply, Overburdened},
                      handle_cast({timeout, Self}, Overburdened)),
        ?_assertMatch({noreply, S},
                      handle_cast({timeout, Self}, Cooldown)),

        ?_assertMatch({noreply, S},
                      handle_info({'DOWN', MonRef, process, Self, info}, S)),
        ?_assertMatch({noreply, S},
                      handle_info({'DOWN', MonRef, process, Self, info}, LockHolder)),
        ?_assertMatch({noreply, LockHolder},
                      handle_info({'DOWN', MonRef, process, selfie, info}, LockHolder))
       ]
     },
     { "size changes",
       [
        ?_assertMatch({reply, ok, #state{abs_size=2}},
                      handle_call({set_size, 2}, from, S)),
        ?_assertMatch({reply, {ok, DefaultSize}, S},
                      handle_call(get_size, from, S)),
        ?_assertError(function_clause,
                      handle_call({set_size, -1}, from, S))
       ]
     }
    ].

callback_test_() ->
    S = #state{},

    [
     ?_assertMatch({ok, S},                  init([])),
     ?_assertMatch({noreply, S},             handle_cast(request, S)),
     ?_assertMatch(ok,                       terminate(reason, S)),
     ?_assertMatch({error, not_implemented}, code_change(old_vsn, S, extra))
    ].

-endif.
