-module(rts_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { rts_vnode_master,
                  {riak_core_vnode_master, start_link, [rts_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    Entry = {rts_entry_vnode_master,
             {riak_core_vnode_master, start_link, [rts_entry_vnode]},
             permanent, 5000, worker, [riak_core_vnode_master]},

    Stat = {rts_stat_vnode_master,
            {riak_core_vnode_master, start_link, [rts_stat_vnode]},
            permanent, 5000, worker, [riak_core_vnode_master]},

    WriteFSMs = {rts_write_fsm_sup,
                 {rts_write_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [rts_write_fsm_sup]},

    GetFSMs = {rts_get_fsm_sup,
               {rts_get_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [rts_get_fsm_sup]},

    {ok,
     {{one_for_one, 5, 10},
      [VMaster, Entry, Stat, WriteFSMs, GetFSMs]}}.
