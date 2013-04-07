%% @doc A vnode to crunch incoming log entries.  Attempt to match each
%% log entry against a registry of regexps.  If a regexp matches then
%% execute its corresponding trigger function passing it the {Client,
%% Entry, Regexp} as well as the resulting match.  The trigger
%% function can then choose to take an action such as update a
%% statistic via the `rts_stat_vnode'.
%%
%% Since this vnode is purely for computation there is no need to
%% worry about handoff.
-module(rts_entry_vnode).
-behaviour(riak_core_vnode).
-include("rts.hrl").

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
         handle_coverage/4,
         handle_exit/3]).

%% match handlers
-export([
         combined_lf/2
        ]).

%% API
-export([entry/3]).

-record(state, {
          partition,
          reg            %% registry [regexp => fun]
         }).

-define(MASTER, rts_entry_vnode_master).
-define(COMBINED_LF, "(\\d+\\.\\d+\\.\\d+\\.\\d+) (.*) (.*) (\\[.*\\]) \"(.*)\" (\\d+) (.*) \"(.*)\" \"(.*)\"").

%%%===================================================================
%%% API
%%%===================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

entry(IdxNode, Client, Entry) ->
    riak_core_vnode_master:command(IdxNode,
                                   {entry, Client, Entry},
                                   ?MASTER).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([Partition]) ->
    Reg = [
           {?COMBINED_LF, fun ?MODULE:combined_lf/2}
          ],
    {ok, #state { partition=Partition, reg=Reg }}.

handle_command({entry, Client, Entry}, _Sender, #state{reg=Reg}=State) ->
    io:format("~p~n", [{entry, State#state.partition}]),
    lists:foreach(match(Client, Entry), Reg),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, _State) ->
    {true, _State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

match(Client, Entry) ->
    fun({Regexp, Fun}) ->
            case re:run(Entry, Regexp, [{capture, all, list}]) of
                nomatch -> ignore;
                {match, Match} -> Fun({Client, Entry, Regexp}, Match)
            end
    end.

%%%===================================================================
%%% Match Handlers
%%%===================================================================

combined_lf({Client, _Entry, _Regexp}, [_Entry, _Host, _, _User, _Time, Req, Code, BodySize, _Referer, Agent]) ->
    rts:sadd(Client, "agents", Agent),
    rts:incrby(Client, "total_sent", list_to_integer(BodySize)),
    [Method, _Resource, _Protocol] = string:tokens(Req, " "),
    rts:incr(Client, Method),
    case Code of
        [$2, _, _] ->
            rts:incr(Client, "200");
        [$3, _, _] ->
            rts:incr(Client, "300");
        [$4, _, _] ->
            rts:incr(Client, "400");
        [$5, _, _] ->
            rts:incr(Client, "500")
    end,
    rts:incr(Client, "total_reqs").
