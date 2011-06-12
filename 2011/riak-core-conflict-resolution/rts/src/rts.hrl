-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-define(N, 3).
-define(R, 2).
-define(W, 2).

-define(DEFAULT_TIMEOUT, 10000).
-define(STATEBOX_EXPIRE, 60000).

-type val()             :: any().
-type proplist()        :: [proplists:property()].

-record(rts_vclock,     {val    :: val(),
                         vclock :: vclock:vclock()}).

-record(incr,           {total  :: pos_integer(),
                         counts :: dict()}).

-type rts_vclock()      :: #rts_vclock{}.
-type rts_obj()         :: rts_vclock() | not_found.
-type reconcile_fun()   :: fun(([rts_obj()]) -> rts_obj()).
