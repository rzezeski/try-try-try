%% @doc Interface for rts-admin commands.
-module(rts_console).
-export([join/1,
         leave/1,
         remove/1,
         ringready/1]).

join([NodeStr]) ->
    case do_join(NodeStr) of
        ok ->
            io:format("Sent join request to ~s
", [NodeStr]),
            ok;
        {error, not_reachable} ->
            io:format("Node ~s is not reachable!
", [NodeStr]),
            error;
        {error, different_ring_sizes} ->
            io:format("Failed: ~s has a different ring_creation_size~n",
                      [NodeStr]),
            error
    end;
join(_) ->
    io:format("Join requires a node to join with.
"),
    error.

do_join(NodeStr) when is_list(NodeStr) ->
    do_join(riak_core_util:str_to_node(NodeStr));
do_join(Node) when is_atom(Node) ->
    {ok, OurRingSize} = application:get_env(riak_core, ring_creation_size),
    case net_adm:ping(Node) of
        pong ->
            case rpc:call(Node,
                          application,
                          get_env, 
                          [riak_core, ring_creation_size]) of
                {ok, OurRingSize} ->
                    riak_core_gossip:send_ring(Node, node());
                _ -> 
                    {error, different_ring_sizes}
            end;
        pang ->
            {error, not_reachable}
    end.

leave([]) ->
    remove_node(node()).

remove([Node]) ->
    remove_node(list_to_atom(Node)).

remove_node(Node) when is_atom(Node) ->
    Res = riak_core_gossip:remove_from_cluster(Node),
    io:format("~p
", [Res]).

-spec(ringready([]) -> ok | error).
ringready([]) ->
    case ringready() of
        {ok, Nodes} ->
            io:format("TRUE All nodes agree on the ring ~p
", [Nodes]);
        {error, {different_owners, N1, N2}} ->
            io:format("FALSE Node ~p and ~p list different partition owners
", [N1, N2]),
            error;
        {error, {nodes_down, Down}} ->
            io:format("FALSE ~p down.  All nodes need to be up to check.
", [Down]),
            error
    end.

-spec(ringready() -> {ok, [atom()]} | {error, any()}).
ringready() ->
    case get_rings() of
        {[], Rings} ->
            {N1,R1}=hd(Rings),
            case rings_match(hash_ring(R1), tl(Rings)) of
                true ->
                    Nodes = [N || {N,_} <- Rings],
                    {ok, Nodes};

                {false, N2} ->
                    {error, {different_owners, N1, N2}}
            end;

        {Down, _Rings} ->
            {error, {nodes_down, Down}}
    end.

%% Retrieve the rings for all other nodes by RPC
get_rings() ->
    {RawRings, Down} = riak_core_util:rpc_every_member(
                         riak_core_ring_manager, get_my_ring, [], 30000),
    Rings = orddict:from_list([{riak_core_ring:owner_node(R), R} || {ok, R} <- RawRings]),
    {lists:sort(Down), Rings}.

%% Produce a hash of the 'chash' portion of the ring
hash_ring(R) ->
    erlang:phash2(riak_core_ring:all_owners(R)).

%% Check if all rings match given a hash and a list of [{N,P}] to check
rings_match(_, []) ->
    true;
rings_match(R1hash, [{N2, R2} | Rest]) ->
    case hash_ring(R2) of
        R1hash ->
            rings_match(R1hash, Rest);
        _ ->
            {false, N2}
    end.
