%% @doc Interface into the Real Time Statistics application.
-module(rts).
-include("rts.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         entry/2,
         get/2,
         set/3,
         append/3,
         incr/2,
         incrby/3,
         sadd/3
        ]).

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
%%
%% TODO: Coordinator to provide N/R/W
entry(Client, Entry) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                       term_to_binary(now())}),
    PrefList = riak_core_apl:get_apl(DocIdx, 1, rts_entry),
    [IdxNode] = PrefList,
    rts_entry_vnode:entry(IdxNode, Client, Entry).

%% @doc Get a stat's value.
get(Client, StatName) ->
    rts_stat_vnode:get(get_idxnode(Client, StatName), StatName).

%% @doc Set a stat's value, replacing the current value.
set(Client, StatName, Val) ->
    rts_stat_vnode:set(get_idxnode(Client, StatName), StatName, Val).

%% @doc Append to a stat's value.
append(Client, StatName, Val) ->
    rts_state_vnode:append(get_idxnode(Client, StatName), StatName, Val).

%% @doc Increment the stat's value by 1.
incr(Client, StatName) ->
    rts_stat_vnode:incr(get_idxnode(Client, StatName), StatName).

%% @doc Increment the stat's value by Val.
incrby(Client, StatName, Val) ->
    rts_stat_vnode:incrby(get_idxnode(Client, StatName), StatName, Val).

%% @doc Add a memeber to the stat's set.
sadd(Client, StatName, Val) ->
    rts_stat_vnode:sadd(get_idxnode(Client, StatName), StatName, Val).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

get_idxnode(Client, StatName) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client), list_to_binary(StatName)}),
    hd(riak_core_apl:get_apl(DocIdx, 1, rts_stat)).
