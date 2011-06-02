%% @doc Webmachine resource to handle incoming log entries.
%%
%% == PUT /rts/entry/Client ==
%%
%% Write an entry for the given Client.
%%
%% Content-type must be 'text/plain' and the body should be _one_
%% line.
-module(rts_wm_entry).
-export([
         init/1,
         allowed_methods/2,
         process_post/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init(_Props) ->
    {ok, none}.

allowed_methods(_RD, _Ctx) ->
    {['POST'], _RD, _Ctx}.

process_post(RD, _Ctx) ->
    case wrq:get_req_header("content-type", RD) of
        "text/plain" ->
            Client = wrq:path_info(client, RD),
            Entry = binary_to_list(wrq:req_body(RD)),
            ok = rts:entry(Client, Entry),
            {true, RD, _Ctx};
        _ ->
            {{halt, 415}, RD, _Ctx}
    end.
