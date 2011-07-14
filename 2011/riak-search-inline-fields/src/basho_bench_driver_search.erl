-module(basho_bench_driver_search).
-compile(export_all).
-record(state, {cmd, q, index, filter, expected}).
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

new(_Id) ->
    Path = basho_bench_config:get(search_path),
    Cmd = Path ++ "/bin/search-cmd",
    case filelib:is_file(Cmd) of
        true -> ok;
        false -> throw("Failed to find search-cmd~n")
    end,
    Index = basho_bench_config:get(search_index),
    Query = basho_bench_config:get(search_q),
    Filter = basho_bench_config:get(search_filter, ""),
    Expected = basho_bench_config:get(search_expected, undefined),
    {ok, #state{cmd=Cmd,q=Query, index=Index, filter=Filter, expected=Expected}}.

run(get, _KeyGen, _ValGen, S=#state{cmd=Cmd, q=Q, index=Index, filter=Filter,
                                    expected=Expected}) ->
    Str0 = "~s search ~s \"~s\" \"~s\" | tail -1 | cut -d ' ' -f4 | tr -d '\n'",
    Str = lists:flatten(io_lib:format(Str0, [Cmd, Index, Q, Filter])),
    Res = os:cmd(Str),
    N = list_to_integer(Res),
    case Expected of
        undefined -> {ok, S};
        _ ->
            if N == Expected -> {ok, S};
               true ->
                    {error,
                     ?FMT("Expected ~p but received ~p~n", [Expected, N])}
            end
    end.
