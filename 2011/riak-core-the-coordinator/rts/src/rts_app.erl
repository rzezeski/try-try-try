-module(rts_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case rts_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register_vnode_module(rts_vnode),
            ok = riak_core_node_watcher:service_up(rts, self()),

            ok = riak_core:register_vnode_module(rts_entry_vnode),
            ok = riak_core_node_watcher:service_up(rts_entry, self()),

            ok = riak_core:register_vnode_module(rts_stat_vnode),
            ok = riak_core_node_watcher:service_up(rts_stat, self()),

            EntryRoute = {["rts", "entry", client], rts_wm_entry, []},
            webmachine_router:add_route(EntryRoute),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
