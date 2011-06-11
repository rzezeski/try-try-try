%% @doc Interface into the Real Time Statistics application.
-module(rts).
-include("rts.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         entry/2,
         get/2,
         get/3,
         set/3,
         append/3,
         incr/2,
         incrby/3,
         sadd/3,
         srem/3
        ]).

-export([get_dbg_preflist/2,
         get_dbg_preflist/3,
         dbg_incr/4]).
-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rts),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, rts_vnode_master).

%% @doc Process an entry.
entry(Client, Entry) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                       term_to_binary(now())}),
    PrefList = riak_core_apl:get_apl(DocIdx, 1, rts_entry),
    [IdxNode] = PrefList,
    rts_entry_vnode:entry(IdxNode, Client, Entry).

%% @doc Get a stat's value.
get(Client, StatName) ->
    {ok, ReqID} = rts_get_fsm:get(Client, StatName, []),
    wait_for_reqid(ReqID, ?TIMEOUT).

get(Client, StatName, Opts) ->
    {ok, ReqID} = rts_get_fsm:get(Client, StatName, Opts),
    wait_for_reqid(ReqID, ?TIMEOUT).

get_dbg_preflist(Client, StatName) ->
    [get_dbg_preflist(Client, StatName, N) || N <- lists:seq(1,3)].

get_dbg_preflist(Client, StatName, N) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                       list_to_binary(StatName)}),
    Preflist = riak_core_apl:get_apl(DocIdx, ?N, rts_stat),
    IdxNode = lists:nth(N, Preflist),
    {ok, req_id, _, Val} =
        riak_core_vnode_master:sync_command(IdxNode,
                                            {get, req_id, StatName},
                                            rts_stat_vnode_master),
    {IdxNode, Val}.

%% @doc Set a stat's value, replacing the current value.
set(Client, StatName, Val) ->
    do_write(Client, StatName, set, Val).

%% @doc Append to a stat's value.
append(Client, StatName, Val) ->
    do_write(Client, StatName, append, Val).

%% @doc Increment the stat's value by 1.
incr(Client, StatName) ->
    do_write(Client, StatName, incr).

%% @doc Increment the stat's value by Val.
incrby(Client, StatName, Val) ->
    do_write(Client, StatName, incrby, Val).

%% @doc Add a memeber to the stat's set.
sadd(Client, StatName, Val) ->
    do_write(Client, StatName, sadd, Val).

srem(Client, StatName, Val) ->
    do_write(Client, StatName, srem, Val).

%% @doc Fake a partitioned write to the given `Nodes' from the given
%% `Coordinator'.  That is, this write will act as if the given nodes
%% are partitioned from the rest of the cluster.  Let the replies fall
%% on the caller's mailbox.
dbg_incr(Coordinator, Nodes, Client, StatName) ->
    ReqID = mk_reqid(),
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                       list_to_binary(StatName)}),
    Preflist = riak_core_apl:get_apl(DocIdx, ?N, rts_stat),
    P = fun({_Idx,Node}) ->
                lists:member(Node, Nodes)
        end,
    Targets = lists:filter(P, Preflist),
    rts_stat_vnode:incr(Targets, {ReqID, Coordinator}, StatName).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
do_write(Client, StatName, Op) ->
    {ok, ReqID} = rts_write_fsm:write(Client, StatName, Op),
    wait_for_reqid(ReqID, ?TIMEOUT).

do_write(Client, StatName, Op, Val) ->
    {ok, ReqID} = rts_write_fsm:write(Client, StatName, Op, Val),
    wait_for_reqid(ReqID, ?TIMEOUT).

wait_for_reqid(ReqID, Timeout) ->
    receive
	{ReqID, ok} -> ok;
        {ReqID, ok, Val} -> {ok, Val}
    after Timeout ->
	    {error, timeout}
    end.

mk_reqid() -> erlang:phash2(erlang:now()).
