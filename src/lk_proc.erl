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

-record(state, {
          abs_size = 3 :: pos_integer(),
          consumed_locks = 0 :: pos_integer(),
          lock_holders = [] :: [{pid(), reference()}]
         }).

%% ===================================================================
%%  API
%% ===================================================================

init([]) ->
    {ok, #state{}}.

%% lock setting
handle_call(set_lock, _From, #state{abs_size=N, consumed_locks=M}=S)
  when N =< M ->
    {reply, {error, overburdened}, S};
handle_call(set_lock, From, #state{abs_size=N, consumed_locks=M}=S)
  when N > M ->
    case proplists:get_value(From, S#state.lock_holders) of
        undefined ->
            MonRef = erlang:monitor(process, From),
            NewState = S#state{consumed_locks = M+1,
                               lock_holders = [{From, MonRef} | S#state.lock_holders]},
            {reply, ok, NewState};
        _MonRef ->
            {reply, {error, already_held}, S}
    end;

%% lock deleting
handle_call(del_lock, _From, #state{consumed_locks=0}=S) ->
    {reply, {error, not_held}, S};
handle_call(del_lock, From, #state{consumed_locks=M}=S)
  when M > 0 ->
    case proplists:get_value(From, S#state.lock_holders) of
        undefined ->
            {reply, {error, not_held}, S};
        MonRef ->
            _DidFlush = erlang:demonitor(MonRef, [flush]),
            Holders = proplists:delete(From, S#state.lock_holders),
            {reply, ok, S#state{consumed_locks=M-1, lock_holders=Holders}}
    end;

%% size queries and modification
handle_call(get_size, _From, #state{abs_size=Int}=S) ->
    {reply, {ok, Int}, S};
handle_call({set_size, Int}, _From, #state{}=S) when Int > 0 ->
    {reply, ok, S#state{abs_size=Int}}.

handle_cast(_Request, #state{}=S) ->
    {noreply, S}.

handle_info({'DOWN', _MonRef, process, _Pid, _Info},
            #state{consumed_locks=0, lock_holders=[]}=S) ->
    {noreply, S};
handle_info({'DOWN', MonRef, process, Pid, _Info}, #state{consumed_locks=M}=S) ->
    case proplists:get_value(Pid, S#state.lock_holders) of
        undefined ->
            {noreply, S};
        MonRef ->
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

%% ===================================================================
%%  test
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

protocol_test_() ->
    Self = self(),
    S = #state{},
    DefaultSize = S#state.abs_size,

    Overburdened = S#state{abs_size=0, consumed_locks=1},
    MonRef = erlang:monitor(process, Self),
    LockHolder   = S#state{consumed_locks=1, lock_holders=[{Self, MonRef}]},

    [
     { "setting locks",
       [
        ?_assertMatch({reply, {error, overburdened}, Overburdened},
                      handle_call(set_lock, from, Overburdened)),
        ?_assertMatch({reply, {error, already_held}, LockHolder},
                      handle_call(set_lock, Self, LockHolder)),
        ?_assertMatch({reply, ok, #state{consumed_locks=1, lock_holders=[{Self, _}]}},
                      handle_call(set_lock, Self, S))
       ]
     },
     { "deleting locks",
       [
        ?_assertMatch({reply, {error, not_held}, S},
                      handle_call(del_lock, Self, S)),
        ?_assertMatch({reply, {error, not_held}, LockHolder},
                      handle_call(del_lock, not_holder, LockHolder)),
        ?_assertMatch({reply, ok, S},
                      handle_call(del_lock, Self, LockHolder)),

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
