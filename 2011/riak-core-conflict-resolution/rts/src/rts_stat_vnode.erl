%% @doc A vnode to handle get & put commands for stat data.  The vnode
%% requests will be hashed on Client and StatName and will use a
%% coordinator to enforce N/R/W values.
-module(rts_stat_vnode).
-behaviour(riak_core_vnode).
-include("rts.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_exit/3]).

-export([
         get/3,
         set/4,
         repair/3,
         incr/3,
         incrby/4,
         append/4,
         sadd/4
        ]).

-record(state, {partition, stats, node}).

-define(MASTER, rts_stat_vnode_master).
-define(sync(PrefList, Command, Master),
        riak_core_vnode_master:sync_command(PrefList, Command, Master)).

%%%===================================================================
%%% API
%%%===================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

get(Preflist, ReqID, StatName) ->
    riak_core_vnode_master:command(Preflist,
                                   {get, ReqID, StatName},
                                   {fsm, undefined, self()},
                                   ?MASTER).

set(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {set, Identity, StatName, Val},
                                   ?MASTER).

%% @doc Attempt to repair -- fire and forget.
repair(IdxNode, StatName, Obj) ->
    riak_core_vnode_master:command(IdxNode,
                                   {repair, undefined, StatName, Obj},
                                   ignore,
                                   ?MASTER).

%% TODO: I have to look at the Sender stuff more closely again
incr(Preflist, Identity, StatName) ->
    riak_core_vnode_master:command(Preflist,
                                   {incr, Identity, StatName},
                                   {fsm, undefined, self()},
                                   ?MASTER).

incrby(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {incrby, Identity, StatName, Val},
                                   {fsm, undefined, self()},
                                   ?MASTER).

append(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {append, Identity, StatName, Val},
                                   ?MASTER).

sadd(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {sadd, Identity, StatName, Val},
                                   ?MASTER).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Partition]) ->
    {ok, #state { partition=Partition, stats=dict:new(), node=node() }}.

handle_command({get, ReqID, StatName}, _Sender,
               #state{stats=Stats, partition=Partition, node=Node}=State) ->
    Reply =
        case dict:find(StatName, Stats) of
            error ->
                not_found;
            {ok, Found} ->
                Found
        end,
    {reply, {ok, ReqID, {Partition,Node}, Reply}, State};

handle_command({set, {ReqID, _}, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
    Stats = dict:store(StatName, Val, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({repair, undefined, StatName, Obj}, _Sender, #state{stats=Stats0}=State) ->
    error_logger:error_msg("repair performed ~p~n", [Obj]),
    Stats = dict:store(StatName, Obj, Stats0),
    {noreply, State#state{stats=Stats}};

handle_command({incr, {ReqID, Cordinator}, StatName}, _Sender,
               #state{stats=Stats0}=State) ->
    Obj =
        case dict:find(StatName, Stats0) of
            {ok, #rts_vclock{val=Val0, vclock=VClock0}=Obj0} ->
                Val = Val0 + 1,
                VClock = vclock:increment(Cordinator, VClock0),
                Obj0#rts_vclock{val=Val, vclock=VClock};
            error ->
                Meta = [{rec_mf, rts_get_fsm, incr_reconcile}],
                VC0 = vclock:fresh(),
                VC = vclock:increment(Cordinator, VC0),
                #rts_vclock{meta=Meta, val=1, vclock=VC}
        end,
    Stats = dict:store(StatName, Obj, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({incrby, {ReqID, _}, StatName, IncrBy}, _Sender, #state{stats=Stats0}=State) ->
    Obj =
        case dict:find(StatName, Stats0) of
            {ok, #rts_basic{val=Val0}=Obj0} ->
                Val = Val0 + IncrBy,
                Obj0#rts_basic{val=Val};
            error ->
                Meta = [{rec_mf, rts_get_fsm, incrby_reconcile}],
                #rts_basic{meta=Meta, val=IncrBy}
        end,
    Stats = dict:store(StatName, Obj, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({append, {ReqID, _}, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
    Stats = try dict:append(StatName, Val, Stats0)
            catch _:_ -> dict:store(StatName, [Val], Stats0)
            end,
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({sadd, {ReqID, _}, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
    F = fun(S) ->
                sets:add_element(Val, S)
        end,
    Stats = dict:update(StatName, F, sets:from_list([Val]), Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}}.


handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.stats),
    {reply, Acc, State}.

handoff_starting(_TargetNode, _State) ->
    {true, _State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, #state{stats=Stats0}=State) ->
    {StatName, Val} = binary_to_term(Data),
    Stats = dict:store(StatName, Val, Stats0),
    {reply, ok, State#state{stats=Stats}}.

encode_handoff_item(StatName, Val) ->
    term_to_binary({StatName,Val}).

is_empty(State) ->
    case dict:size(State#state.stats) of
        0 -> {true, State};
        _ -> {false, State}
    end.

delete(State) ->
    {ok, State}.

handle_exit(_Pid, _Reason, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    ok.
