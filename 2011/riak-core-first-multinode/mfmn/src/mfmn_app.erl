-module(mfmn_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case mfmn_sup:start_link() of
        {ok, Pid} ->

            {ok, App} = application:get_application(self()),
            ok = riak_core:register(App, [{vnode_module, mfmn_vnode}])
            ok = riak_core_node_watcher:service_up(mfmn, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
