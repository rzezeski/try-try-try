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
         incr/3,
         incrby/4,
         append/4,
         sadd/4
        ]).

-record(state, {partition, stats}).

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

set(Preflist, ReqID, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {set, ReqID, StatName, Val},
                                   ?MASTER).

%% TODO: I have to look at the Sender stuff more closely again
incr(Preflist, ReqID, StatName) ->
    riak_core_vnode_master:command(Preflist,
                                   {incr, ReqID, StatName},
                                   {fsm, undefined, self()},
                                   ?MASTER).

incrby(Preflist, ReqID, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {incrby, ReqID, StatName, Val},
                                   {fsm, undefined, self()},
                                   ?MASTER).

append(Preflist, ReqID, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {append, ReqID, StatName, Val},
                                   ?MASTER).

sadd(Preflist, ReqID, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {sadd, ReqID, StatName, Val},
                                   ?MASTER).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Partition]) ->
    {ok, #state { partition=Partition, stats=dict:new() }}.

handle_command({get, ReqID, StatName}, _Sender, #state{stats=Stats}=State) ->
    Reply =
        case dict:find(StatName, Stats) of
            error ->
                not_found;
            {ok, Found} ->
                Found
        end,
    {reply, {ok, ReqID, Reply}, State};

handle_command({set, ReqID, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
    Stats = dict:store(StatName, Val, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({incr, ReqID, StatName}, _Sender, #state{stats=Stats0}=State) ->
    Stats = dict:update_counter(StatName, 1, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({incrby, ReqID, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
    Stats = dict:update_counter(StatName, Val, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({append, ReqID, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
    Stats = try dict:append(StatName, Val, Stats0)
            catch _:_ -> dict:store(StatName, [Val], Stats0)
            end,
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({sadd, ReqID, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
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
