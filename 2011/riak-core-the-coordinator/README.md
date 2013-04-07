Riak Core, The Coordinator
==========

At the end of my [vnode](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-the-vnode) post I asked the question "Where's the redundancy?"  Currently there is none in RTS.  Riak Core isn't magic.  It won't do everything for you.  Instead it's a suite of tools for building distributed systems.  If you want redundancy you'll have to build it yourself.  In this post I'll do just that by implementing a  _coordinator_ for RTS.


What is a Coordinator?
----------

A _coordinator_ is just what it sounds like.  It's job is to coordinate incoming requests.  It enforces the consistency semantics of [N, R and W](http://wiki.basho.com/Riak-Glossary.html#Quorum) and performs anti-entropy services like [read repair](http://wiki.basho.com/Riak-Glossary.html#Read-Repair).  In simpler terms, it's responsible for distributing data across the cluster and syncing data when it finds conflicts.  You could think of vnodes as the things that Get Shit Done (TM) and the coordinators as the other things telling them what to do and overseeing the work.  They work in tandem to make sure your request is being handled as best as it can.

Techincally speaking, a coordinator is a [gen_fsm](http://www.erlang.org/doc/man/gen_fsm.html).  Each request is handled in its own Erlang process.  A coordinator communicates with the vnode instances to fulfill requests.

To wrap up, a coordinator

* coordinates requests

* enforces the consistency requirements

* performs anti-entropy

* is an Erlang process that implements the `gen_fsm` behavior

* and communicates with the vnode instances to execute the request


Implementing a Coordinator
----------

Unlike the vnode, Riak Core doesn't define a coordinator behavior.  You have to roll your own each time.  I used Riak's [get](https://github.com/basho/riak_kv/blob/1.0/src/riak_kv_get_fsm.erl) and [put](https://github.com/basho/riak_kv/blob/1.0/src/riak_kv_put_fsm.erl) coordinators for guidance.  You'll notice they both have a similar structure.  I'm going to propose a general structure here that you can use as your guide, but remember that there's nothing set in stone on how to write a coordinator.

Before moving forward it's worth mentioning that you'll want to instantiate these coordinators under a [simple_one_for_one](http://www.erlang.org/doc/design_principles/sup_princ.html#id69831) supervisor.  If you've never heard of `simple_one_for_one` before then think of it as a factory for Erlang processes of the same type.  An incoming request will at some point call `supervisor:start_child/2` to instantiate a new FSM dedicated to handling this specific request.

### init(Args) -> {ok, InitialState, SD, Timeout} ###

    Args                :: term()
    InitialState        :: atom()
    SD                  :: term()
    Timeout             :: integer()

This is actually part of the `gen_fsm` behavior.  It's a callback you must implement to specify the `InitialState` name and its data (`SD`).  In this case you'll also want to specify a `Timeout` value of `0` in order to immediately go to the `InitialState`, `prepare`.

A get coordinator for RTS is passed four arguments.

1. `ReqId`: A unique id for this request.

2. `From`: Who to send the reply to.

3. `Client`: The name of the client entity -- the entity that is writing log events to RTS.

4. `StatName`: The name of the statistic the requester is interested in.

This data will be passed as a list to init at which point it will build the initial state record and tell the FSM to proceed to the `prepare` state.

    init([ReqId, From, Client, StatName]) ->
        SD = #state{req_id=ReqId,
                    from=From,
                    client=Client,
                    stat_name=StatName},
        {ok, prepare, SD, 0}.

The write coordinator for RTS is very similar but has two additional arguments.

1. `Op`: The operation to be performed, one of `set`, `append`, `incr`,
`incrby` or `sadd`.

2. `Val`: The value of the operation.  For the `incr` op this is `undefined`.

Here is the code.

    init([ReqID, From, Client, StatName, Op, Val]) ->
        SD = #state{req_id=ReqID,
                    from=From,
                    client=Client,
                    stat_name=StatName,
                    op=Op,
                    val=Val},
        {ok, prepare, SD, 0}.


### prepare(timeout, SD0) -> {next_state, NextState, SD, Timeout} ###

    SD0 = SD            :: term()
    NextState           :: atom()
    Timeout             :: integer()

The job of `prepare` is to build the _preference list_.  The preference list is the preferred set of vnodes that should participate in this request.  Most of the work is actually done by `riak_core_util:chash_key/1` and `riak_core_apl:get_apl/3`.  Both the get and write coordinators do the same thing here.

1. Calculate the index in the ring that this request falls on.

2. From this index determine the `N` preferred partitions that should handle the request.

Here is the code.

    prepare(timeout, SD0=#state{client=Client,
                                stat_name=StatName}) ->
        DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                           list_to_binary(StatName)}),
        Prelist = riak_core_apl:get_apl(DocIdx, ?N, rts_stat),
        SD = SD0#state{preflist=Prelist},
        {next_state, execute, SD, 0}.

The fact that the key is a two-tuple is simply a consequence of the fact that Riak Core was extracted from Riak and some of its key-value semantics crossed during the extraction.  In the future things like this may change.

### execute(timeout, SD0) -> {next_state, NextState, SD} ###

    SD0 = SD            :: term()
    NextState           :: atom()

The `execute` state executes the request by sending commands to the vnodes in the preflist and then putting the coordinator into a waiting state.  The code to do this in RTS is really simple; call the vnode command passing it the preference list.  Under the covers the vnode uses `riak_core_vnode_master:command/4` which will distribute the commands across the `Preflist` for you.  I'll talk about this later in the post.

Here's the code for the get coordinator.

    execute(timeout, SD0=#state{req_id=ReqId,
                                stat_name=StatName,
                                preflist=Prelist}) ->
        rts_stat_vnode:get(Prelist, ReqId, StatName),
        {next_state, waiting, SD0}.

The code for the write coordinator is almost identical except it's parameterized on `Op`.

    execute(timeout, SD0=#state{req_id=ReqID,
                            stat_name=StatName,
                            op=Op,
                            val=undefined,
                            preflist=Preflist}) ->
        rts_stat_vnode:Op(Preflist, ReqID, StatName),
        {next_state, waiting, SD0}.


### waiting(Reply, SD0) -> Result ###

    Reply               :: {ok, ReqID}
    Result              :: {next_state, NextState, SD}
                         | {stop, normal, SD}
    NextState           :: atom()
    SD0 = SD            :: term()

This is probably the most interesting state in the coordinator as its job is to enforce the consistency requirements and possibly perform anti-entropy in the case of a get.  The coordinator waits for replies from the various vnode instances it called in `execute` and stops once its requirements have been met.  The typical shape of this function is to pattern match on the `Reply`, check the state data `SD0`, and then either continue waiting or stop depending on the current state data.

The get coordinator waits for replies with the correct `ReqId`, increments the reply count and adds the `Val` to the list of `Replies`.  If the quorum `R` has been met then return the `Val` to the requester and stop the coordinator.  If the vnodes didn't agree on the value then return all observed values.  In this post I am punting on the conflict resolution and anti-entropy part of the coordinator and exposing the inconsistent state to the client application.  I'll implement conflict resolution in my next post.  If the quorum hasn't been met then continue waiting for more replies.

    waiting({ok, ReqID, Val}, SD0=#state{from=From, num_r=NumR0, replies=Replies0}) ->
        NumR = NumR0 + 1,
        Replies = [Val|Replies0],
        SD = SD0#state{num_r=NumR,replies=Replies},
        if
            NumR =:= ?R ->
                Reply =
                    case lists:any(different(Val), Replies) of
                        true ->
                            Replies;
                        false ->
                            Val
                    end,
                From ! {ReqID, ok, Reply},
                {stop, normal, SD};
            true -> {next_state, waiting, SD}
        end.

The write coordinator has things a little easier here because it only cares that `W` vnodes executed its write request.

    waiting({ok, ReqID}, SD0=#state{from=From, num_w=NumW0}) ->
        NumW = NumW0 + 1,
        SD = SD0#state{num_w=NumW},
        if
        NumW =:= ?W ->
                From ! {ReqID, ok},
                {stop, normal, SD};
        true -> {next_state, waiting, SD}
        end.


What About the Entry Coordinator?
----------

Some of you may be wondering why I didn't write a coordinator for the [entry vnode](https://github.com/rzezeski/try-try-try/blob/master/2011/riak-core-the-coordinator/rts/src/rts_entry_vnode.erl)?  If you don't remember this is responsible for matching an incoming log entry and then executing its trigger function.  For example, any incoming log entry from an access log in combined logging format will cause the `total_reqs` stat to be incremented by one.  I only want this action to occur at maximum once per entry.  There is no notion of `N`.  I could write a coordinator that tries to make some guarentees about its execution but for now I'm ok with possibly dropping data occasionally.


Changes to rts.erl and rts_stat_vnode
----------

Now that I've written a coordinator to handle requests to RTS I need to refactor the old [rts.erl](https://github.com/rzezeski/try-try-try/blob/master/2011/riak-core-the-vnode/rts/src/rts.erl) and [rts_stat_vnode](https://github.com/rzezeski/try-try-try/blob/master/2011/riak-core-the-vnode/rts/src/rts_stat_vnode.erl).  The model has changed from calling the vnode directly to delegating the work to [rts_get_fsm](https://github.com/rzezeski/try-try-try/blob/master/2011/riak-core-the-coordinator/rts/src/rts_get_fsm.erl) which will call the various vnodes and collect responses.

    rts:get ----> rts_stat_vnode:get (local)

                                                              /--> stat_vnode@rts1
    rts:get ----> rts_get_fsm:get ----> rts_stat_vnode:get --|---> stat_vnode@rts2
                                                              \--> stat_vnode@rts3


Instead of performing a synchronous request the `rts:get/2` function now calls the get coordinator and then waits for a response.

    get(Client, StatName) ->
        {ok, ReqID} = rts_get_fsm:get(Client, StatName),
        wait_for_reqid(ReqID, ?TIMEOUT).

The write requests underwent a similar refactoring.

    do_write(Client, StatName, Op) ->
        {ok, ReqID} = rts_write_fsm:write(Client, StatName, Op),
        wait_for_reqid(ReqID, ?TIMEOUT).

    do_write(Client, StatName, Op, Val) ->
        {ok, ReqID} = rts_write_fsm:write(Client, StatName, Op, Val),
        wait_for_reqid(ReqID, ?TIMEOUT).

The `rts_stat_vnode` was refactored to use `riak_core_vnode_master:command/4` which takes a `Preflist`, `Msg`, `Sender` and `VMaster` as argument.

* `Preflist`: The list of vnodes to send the command to.

* `Msg`: The command to send.

* `Sender`: A value describing who sent the request, in this case the coordinator.  This is used by the vnode to correctly address the reply message.

* `VMaster`: The name of the vnode master for the vnode type to send this command to.

    get(Preflist, ReqID, StatName) ->
        riak_core_vnode_master:command(Preflist,
                                       {get, ReqID, StatName},
                                       {fsm, undefined, self()},
                                       ?MASTER).


Coordinators in Action
----------

Talk is cheap, lets see it in action.  Towards the end of the vnode post I made the following statement.

> If you start taking down nodes you'll find that stats start to disappear.

One of the main objectives of the coordinator is to fix this problem.  Lets see if it worked.

### Build the devrel ###

    make
    make devrel

### Start the Cluster ###

    for d in dev/dev*; do $d/bin/rts start; done
    for d in dev/dev{2,3}; do $d/bin/rts-admin join rts1@127.0.0.1; done

### Feed in Some Data ###

    gunzip -c progski.access.log.gz | head -100 | ./replay --devrel progski

### Get Some Stats ###

    ./dev/dev1/bin/rts attach
    (rts1@127.0.0.1)1> rts:get("progski", "total_reqs").
    {ok,97}
    (rts1@127.0.0.1)2> rts:get("progski", "GET").       
    {ok,91}
    (rts1@127.0.0.1)3> rts:get("progski", "total_sent").
    {ok,445972}
    (rts1@127.0.0.1)4> rts:get("progski", "HEAD").      
    {ok,6}
    (rts1@127.0.0.1)5> rts:get("progski", "PUT").  
    {ok,not_found}
    (rts1@127.0.0.1)6> rts:get_dbg_preflist("progski", "total_reqs"). 
    [{730750818665451459101842416358141509827966271488,
      'rts3@127.0.0.1'},
     {753586781748746817198774991869333432010090217472,
      'rts1@127.0.0.1'},
     {776422744832042175295707567380525354192214163456,
      'rts2@127.0.0.1'}]
    (rts1@127.0.0.1)7> rts:get_dbg_preflist("progski", "GET").       
    [{274031556999544297163190906134303066185487351808,
      'rts1@127.0.0.1'},
     {296867520082839655260123481645494988367611297792,
      'rts2@127.0.0.1'},
     {319703483166135013357056057156686910549735243776,
      'rts3@127.0.0.1'}]

Don't worry about what I did on lines 6 and 7 yet, I'll explain in a second.

### Kill a Node ###

    (rts1@127.0.0.1)8> os:getpid().
    "91461"
    Ctrl^D
    kill -9 91461

### Verify it's Down ###

    $ ./dev/dev1/bin/rts ping
    Node 'rts1@127.0.0.1' not responding to pings.

### Get Stats on rts2 ###

You're results my not exactly match mine as it depends on which vnode instances responded first.  The coordinator only cares about getting `R` responses.

    ./dev/dev2/bin/rts attach
    (rts2@127.0.0.1)1> rts:get("progski", "total_reqs").
    {ok,97}
    (rts2@127.0.0.1)2> rts:get("progski", "GET").       
    {ok,[not_found,91]}
    (rts2@127.0.0.1)3> rts:get("progski", "total_sent").
    {ok,445972}
    (rts2@127.0.0.1)4> rts:get("progski", "HEAD").      
    {ok,[not_found,6]}
    (rts2@127.0.0.1)5> rts:get("progski", "PUT"). 
    {ok,not_found}

### Let's Compare the Before and After Preflist ###

Notice that some gets on `rts2` return a single value as before whereas others return a list of values.  The reason for this is because the `Preflist` calculation is now including _fallback_ vnodes.  A fallback vnode is one that is not on its appropriate physical node.  As a consequence of killing `rts1` its vnode requests must be routed to one of the other nodes.  Since the request-reply model between the coordinator and vnode is asynchronous our reply value will depend on which vnode instances reply first.  If the instances with values reply first then you get a single value, otherwise you get a list of values.

    (rts2@127.0.0.1)6> rts:get_dbg_preflist("progski", "total_reqs"). 
    [{730750818665451459101842416358141509827966271488,
      'rts3@127.0.0.1'},
     {776422744832042175295707567380525354192214163456,
      'rts2@127.0.0.1'},
     {753586781748746817198774991869333432010090217472,
      'rts3@127.0.0.1'}]
    (rts2@127.0.0.1)7> rts:get_dbg_preflist("progski", "GET").       
    [{296867520082839655260123481645494988367611297792,
      'rts2@127.0.0.1'},
     {319703483166135013357056057156686910549735243776,
      'rts3@127.0.0.1'},
     {274031556999544297163190906134303066185487351808,
      'rts2@127.0.0.1'}]

In both cases either `rts2` or `rts3` stepped in for the missing `rts1`.  Also, in each case, one of these vnodes is going to return `not_found` since it's a fallback.  I added another debug function to determine which one.

    (rts2@127.0.0.1)8> rts:get_dbg_preflist("progski", "total_reqs", 1).
    [{730750818665451459101842416358141509827966271488,
      'rts3@127.0.0.1'},
     97]
    (rts2@127.0.0.1)9> rts:get_dbg_preflist("progski", "total_reqs", 2). 
    [{776422744832042175295707567380525354192214163456,
      'rts2@127.0.0.1'},
     97]
    (rts2@127.0.0.1)10> rts:get_dbg_preflist("progski", "total_reqs", 3).
    [{753586781748746817198774991869333432010090217472,
      'rts3@127.0.0.1'},
     not_found]
    (rts2@127.0.0.1)11> rts:get_dbg_preflist("progski", "GET", 1).       
    [{296867520082839655260123481645494988367611297792,
      'rts2@127.0.0.1'},
     91]
    (rts2@127.0.0.1)12> rts:get_dbg_preflist("progski", "GET", 2).
    [{319703483166135013357056057156686910549735243776,
      'rts3@127.0.0.1'},
     91]
    (rts2@127.0.0.1)13> rts:get_dbg_preflist("progski", "GET", 3).
    [{274031556999544297163190906134303066185487351808,
      'rts2@127.0.0.1'},
     not_found]

Notice the fallbacks are at the end of each list.  Also notice that since we're on `rts2` that `total_reqs` will almost always return a single value because its fallback is on another node whereas `GET` has a local fallback and will be more likely to return multiple values.


Conflict Resolution & Read Repair
----------

In the [next post](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-conflict-resolution) I'll go over how to implement conflict resolution and read repair in the coordinator.
