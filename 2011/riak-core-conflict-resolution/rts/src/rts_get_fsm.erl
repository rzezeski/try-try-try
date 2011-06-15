%% @doc The coordinator for stat get operations.  The key here is to
%% generate the preflist just like in wrtie_fsm and then query each
%% replica and wait until a quorum is met.
-module(rts_get_fsm).
-behavior(gen_fsm).
-include("rts.hrl").

%% API
-export([start_link/5, get/3]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2, wait_for_n/2, finalize/2]).

-export([reconcile/1]).

-record(state, {req_id,
                from,
                client,
                stat_name,
                preflist,
                num_r=0,
                replies=[],
                r=?R,
                timeout=?DEFAULT_TIMEOUT}).

-type idx_node() :: {integer(), node()}.
-type vnode_reply() :: {idx_node(), rts_obj() | not_found}.

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqID, From, Client, StatName, Opts) ->
    gen_fsm:start_link(?MODULE, [ReqID, From, Client, StatName, Opts], []).

get(Client, StatName, Opts) ->
    ReqID = mk_reqid(),
    rts_get_fsm_sup:start_get_fsm([ReqID, self(), Client, StatName, Opts]),
    {ok, ReqID}.

%%%===================================================================
%%% States
%%%===================================================================

%% Intiailize state data.
init([ReqId, From, Client, StatName, Opts]) ->
    R = proplists:get_value(r, Opts, ?R),
    SD = #state{req_id=ReqId,
                from=From,
                client=Client,
                stat_name=StatName,
                r=R},
    {ok, prepare, SD, 0}.

%% @doc Calculate the Preflist.
prepare(timeout, SD0=#state{client=Client,
                            stat_name=StatName}) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                       list_to_binary(StatName)}),
    Prelist = riak_core_apl:get_apl(DocIdx, ?N, rts_stat),
    SD = SD0#state{preflist=Prelist},
    {next_state, execute, SD, 0}.

%% @doc Execute the get reqs.
execute(timeout, SD0=#state{req_id=ReqId,
                            stat_name=StatName,
                            preflist=Prelist}) ->
    rts_stat_vnode:get(Prelist, ReqId, StatName),
    {next_state, waiting, SD0}.

%% @doc Wait for R replies and then respond to From (original client
%% that called `rts:get/2').
waiting({ok, ReqID, IdxNode, Obj},
        SD0=#state{from=From, num_r=NumR0, replies=Replies0,
                   r=R, timeout=Timeout}) ->
    NumR = NumR0 + 1,
    Replies = [{IdxNode, Obj}|Replies0],
    SD = SD0#state{num_r=NumR,replies=Replies},

    if
        NumR =:= R ->
            Reply = rts_obj:val(merge(Replies)),
            From ! {ReqID, ok, Reply},

            if NumR =:= ?N -> {next_state, finalize, SD, 0};
               true -> {next_state, wait_for_n, SD, Timeout}
            end;
        true -> {next_state, waiting, SD}
    end.

wait_for_n({ok, _ReqID, IdxNode, Obj},
             SD0=#state{num_r=?N - 1, replies=Replies0, stat_name=_StatName}) ->
    Replies = [{IdxNode, Obj}|Replies0],
    {next_state, finalize, SD0#state{num_r=?N, replies=Replies}, 0};

wait_for_n({ok, _ReqID, IdxNode, Obj},
             SD0=#state{num_r=NumR0, replies=Replies0,
                        stat_name=_StatName, timeout=Timeout}) ->
    NumR = NumR0 + 1,
    Replies = [{IdxNode, Obj}|Replies0],
    {next_state, wait_for_n, SD0#state{num_r=NumR, replies=Replies}, Timeout};

%% TODO partial repair?
wait_for_n(timeout, SD) ->
    {stop, timeout, SD}.

finalize(timeout, SD=#state{replies=Replies, stat_name=StatName}) ->
    MObj = merge(Replies),
    case needs_repair(MObj, Replies) of
        true ->
            repair(StatName, MObj, Replies),
            {stop, normal, SD};
        false ->
            {stop, normal, SD}
    end.

handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @impure
mk_reqid() -> erlang:phash2(erlang:now()).

%% @pure
%%
%% @doc Given a list of `Replies' return the merged value.
-spec merge([vnode_reply()]) -> rts_obj() | not_found.
merge(Replies) ->
    Objs = [Obj || {_,Obj} <- Replies],
    rts_obj:merge(Objs).

%% @pure
%%
%% @doc Reconcile conflicts among conflicting values.
-spec reconcile([A::any()]) -> A::any().
reconcile([#incr{}|_]=Vals) ->
    Get = fun(K, L) -> proplists:get_value(K, L, 0) end,
    Counts = [dict:to_list(V#incr.counts) || V <- Vals],
    Nodes = unique(lists:flatten([[Node || {Node,_} <- C] || C <- Counts])),
    MaxCounts = [{Node, lists:max([Get(Node, C) || C <- Counts])}
                 || Node <- Nodes],
    Total = lists:sum([lists:max([Get(Node, C) || C <- Counts])
                       || Node <- Nodes]),
    #incr{total=Total, counts=dict:from_list(MaxCounts)};

reconcile([V|_]=Vals) when element(1, V) == statebox -> statebox:merge(Vals).


%% @pure
%%
%% @doc Given the merged object `MObj' and a list of `Replies'
%% determine if repair is needed.
-spec needs_repair(any(), [vnode_reply()]) -> boolean().
needs_repair(MObj, Replies) ->
    Objs = [Obj || {_,Obj} <- Replies],
    lists:any(different(MObj), Objs).

%% @pure
different(A) -> fun(B) -> not rts_obj:equal(A,B) end.

%% @impure
%%
%% @doc Repair any vnodes that do not have the correct object.
-spec repair(string(), rts_obj(), [vnode_reply()]) -> io.
repair(_, _, []) -> io;

repair(StatName, MObj, [{IdxNode,Obj}|T]) ->
    case rts_obj:equal(MObj, Obj) of
        true -> repair(StatName, MObj, T);
        false ->
            rts_stat_vnode:repair(IdxNode, StatName, MObj),
            repair(StatName, MObj, T)
    end.

%% pure
%%
%% @doc Given a list return the set of unique values.
-spec unique([A::any()]) -> [A::any()].
unique(L) ->
    sets:to_list(sets:from_list(L)).
