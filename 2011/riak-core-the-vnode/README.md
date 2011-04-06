Riak Core, The vnode
==========

In this post I want to discuss the _virtual node_ or _vnode_ for short.  It is the fundamental unit of both work and storage in Riak Core and without understanding it you can't successfully build an application atop Riak Core.

First, Why Riak Core
----------

Before digging into the vnode I want to spend a little time on answering _why_ you may want to use Riak Core.  Specifically, I want to attempt to address Wilson MacGyver's question on the riak-users mailing list.

> The topic I'd really like coverage is, what can riak-core do that other stack including erlang+otp can't do.

This is a great question, and while I hestitate to make any direct comparsions (for fear of selling something else short) I'd like to talk about what I think makes Riak Core an attractive solution.

The key thing to understand is that Riak Core is essentially an implementation of [Dynamo](http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html).  In fact, from what I understand, Riak Core started it's life on [Andy Gross's](https://github.com/argv0) laptop on a plane ride recently after he had read the Dynamo paper.  In the Dynamo paper there are several data structures and techniques laid out that Amazon used to build _highly available_, robust systems that also met certain _SLAs_ 99.9% of the time.  In the paper the authors framed these technologies around a key-value store but they are more general than that.

The two main data structures are:

* The _ring_: The ring (which is really a combination of consistent hashing and vnodes) is at the heart of Riak Core's distribution.  It's what routes requests and data across the cluster.

* _vector clocks_: How do you order events in realtion to each other if you can't trust your time source or don't have one?  You use vector clocks.  In short, Riak Core's vector clock mechanism is almost like having a built-in version control system that will automatically merge forks that it can and alert you to conflicts otherwise.

There are also various processes that are alive in a Riak Core cluster; two important ones being _handoff_ and _coordination_.

* _handoff_: Riak Core implements something called _hinted handoff_.  The "hinted" part means that each parition carries a hint with it that indicates the phsyical node it should be on.  Periodically, the system checks this hint and if the partition is determined to belong to another node it will begin the handoff process.  Which is to say the current node that hosts the parition will start sending data for that partition to the _target_ node now responsible for it.  Handoff occurs when you add a node to the cluster or if a node has restarted after crashing.  Handoff does **NOT** occur when the node crashes.

* _coordination_: The coordinator is responsible for routing requests and satisfying the [CAP](http://www.julianbrowne.com/article/viewer/brewers-cap-theorem) properties _N_, _R_, and _W_.

So these are just some of the things that Riak Core _helps_ you with.  You must keep in mind that Riak Core doesn't necessairly just magically do everything for you.  Much like a _behavior_ Erlang behavior it's providing a _container_ for your application to run in, but you have to provide the funcionality.  In some cases, like coordination, Riak Core gives you some functions to aid in the process but you have to write most of the logic.  My point is that Riak Core still requires some work on your part to get all thoose cool Dynamo features.  That's why I'm writing this series of blog posts.  To help both myself and others understand just what it takes to build a system on Riak Core.


An Interview Question
----------

I was recently posed with an interview question that went something like this:

> You have N machines writing syslog events to a server somewhere.  Each incoming entry should be compared to a list of regular expressions on the server.  Each regexp has a corresponding event that should be triggered if a match occurs.  Said event will write/udpate a value somewhere that can be queried by interested parties.  How do you implement it?

I thought this could make a good fit for solving with Riak Core because it's not completely trivial but it's not bloated with complexity either (it could be, but I'm going to keep it simple here).


What's a vnode, Anyways?
----------

* A _vnode_ is a _virtual node_, as opposed to physical _node_

* Each vnode is responsible for **one** partition on the [ring](http://wiki.basho.com/An-Introduction-to-Riak.html#Clustering)

* A vnode is an [Erlang process](http://www.erlang.org/doc/reference_manual/processes.html)

* A vnode is a [behavior](http://www.erlang.org/doc/design_principles/fsm.html) written atop of the _gen\_fsm_  behavior

* A vnode handles incoming requests

* A vnode potentially stores data to be retrieved later

* A vnode is _the_ unit of concurrency, replication, and fault tolerance

* Typically many vnodes will run on each phsyical node

* Each machine has a _vnode master_ who's purpose is to keep track of all active vnodes on it's node

As you can see a vnode takes on a lot of responsibility.  If none of the above is sinking in then think of the ring like a honeycomb.  A partition is equivalent to a cell in the comb and a vnode is a  worker bee which, at any one time, is reponsible for exactly one of those cells.  When a worker bee dies another springs in it's place to take care of that cell.  If too many bees die then the comb deteriorates.  BTW, if you're wondering where the queen bee is then I'd be tempted to call her the _vnode master_ but that's a stretch since there is a master for each machine in the cluster.  Analogies only go so far.


Lifecycle Callbacks
----------

The `init/1` and `terminate/2` callbacks are called at the edges of the vnode lifecycle.

### init([Index]) -> Result ###

    Index = int() >= 0
    Result = {ok, State}
    State = term()

This callback initializes the state of the vnode.  The entry vnode needs to store the regexp to trigger fun mapping so that the command callback can access it later.

    init([_Partition]) ->
        %% registry consist of regexps mapped to funs.  each fun must take
        %% _two_ arguments: 1) an {Entry, Regexp} tuple and 2) the result
        %% of running the re:run on the incoming Entry using the regexp
        %% keyed to this fun.
        Reg = [
               {?COMBINED_LF, fun ?MODULE:combined_lf/2}
              ],
        {ok, #state { reg=Reg }}.

The entry vnode needs to track the stat updates as they are sent in.

    init([Partition]) ->
        {ok, #state { partition=Partition, stats=dict:new() }}.

### terminate(Reason, State) -> Result ###

    Reason = normal | shutdown | {shutdown, term()} | term()
    State = Result = term()

Used to cleanup any resources held by the vnode.  The `Reason` will depend on how the vnode was stopped.  Since the vnode container is simply a _gen\_fsm_ underneath you can read more about the `Reason` [here](http://erldocs.com/R14B/stdlib/gen_fsm.html).  The `State` is the final state of the vnode and `Result` can be anything but will be ignored by the container.

Since both the entry and stat vnodes keep everything in memory Erlang will handle cleanup implicitly and there is nothing explicit to be done in terminate.

    terminate(_Reason, _State) ->
        ok.

Commands
----------

All incoming requests become commands on your vnode.  For example, a _GET_ on Riak KV will end up in the [handle_command](https://github.com/basho/riak_kv/blob/riak_kv-0.14.0/src/riak_kv_vnode.erl#L171) defined in it's vnode.  For this reason, it might be easiest to start with the question:

> What commands will I need to implement?

To implement a command you add a new `handle_command/3` [function clause](http://www.erlang.org/doc/reference_manual/functions.html) that matches against the incoming request.  For example, to get a stat the request will be `{get, StatName}` which would require a function head with the form `handle_command({get, StatName}, ...)`.

### handle_command(Request, Sender, State) -> Result ###

    Request = term()
    Sender = sender()
    State = NewState = term()
    Result = {reply, Reply, NewState}
             | {noreply, NewState}
             | {stop, Reason, NewState}

The `Request` can be anything (i.e. any term) but is typically a _tagged tuple_.  The `Sender` is a representation of the client process but is typically used as an opaque value that you would use with a utility function such as `riak_core_vnode:reply/2`.  The `State` is much like state in a _gen\_server_ and is there to store data persistent across callback invocations.

There are three choices of reply.  In all cases the 3rd element of the tuple is the potentially modified state.

1) _reply_: Send `Reply` back to the client.

2) _noreply_: Don't send a reply.  This doesn't necessairly indicate that no reply was made, it just means you don't want the vnode container to send a reply.  For example, this command could cause a long-running action to occur and you might spawn it on another process passing the `Sender`.

3) _stop_: For whatever `Reason` you want this vnode to terminate.

The entry vnode needs to compare each incoming log entry with all registered regular expressions and possibly execute a corresponding trigger fun.

    handle_command({entry, Client, Entry}, _Sender, #state{reg=Reg}=State) ->
        lists:foreach(match(Client, Entry), Reg),
        {noreply, State};

With the `match/2` HOF defined as so.

    match(Client, Entry) ->
        fun({Regexp, Fun}) ->
                case re:run(Entry, Regexp, [{capture, all, list}]) of
                    nomatch -> ignore;
                    {match, Match} -> Fun({Client, Entry, Regexp}, Match)
                end
        end.

The stat vnode is like a mini [redis](http://redis.io/) in that it offers in-place updates.  Notice that all mutations cause a new state to be created and returned.

    handle_command({get, StatName}, _Sender, #state{stats=Stats}=State) ->
        Reply =
            case dict:find(StatName, Stats) of
                error ->
                    not_found;
                Found ->
                    Found
            end,
        {reply, Reply, State};
    handle_command({put, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
        Stats = dict:store(StatName, Val, Stats0),
        {reply, ok, State#state{stats=Stats}};
    handle_command({incr, StatName}, _Sender, #state{stats=Stats0}=State) ->
        Stats = dict:update_counter(StatName, 1, Stats0),
        {reply, ok, State#state{stats=Stats}};
    handle_command({incrby, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
        Stats = dict:update_counter(StatName, Val, Stats0),
        {reply, ok, State#state{stats=Stats}};
    handle_command({decr, StatName}, _Sender, #state{stats=Stats0}=State) ->
        Stats = dict:update_counter(StatName, -1, Stats0),
        {reply, ok, State#state{stats=Stats}};
    handle_command({append, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
        Stats = try dict:append(StatName, Val, Stats0)
                catch _:_ -> dict:store(StatName, [Val], Stats0)
                end,
        {reply, ok, State#state{stats=Stats}};
    handle_command({sadd, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
        F = fun(S) ->
                    sets:add_element(Val, S)
            end,
        Stats = dict:update(StatName, F, sets:from_list([Val]), Stats0),
        {reply, ok, State#state{stats=Stats}}.


Handoff
----------

A _handoff_ occurs when a vnode realizes it's not on the proper node.  This could happen for two reasons:

1) A new node is added to the cluster causing some partitions to get shuffled around.

2) A node, that's already a member of the cluster, comes back online causing partitions to be given back to it.

You might have heard the term _hinted handoff_; this is was Riak Core implements.  The "hint" is a piece of data that tells the partition where it's proper home is.  Periodically a "home check" is done which uses the hint to determine if the vnode is on the correct physical host.

Implementing handoff seems hard on the surface but it's nothing to be afraid of.  The key thing to remember about handoff is it's purpose is to transfer data from one vnode to another.  Data transfer, that's it.  This means that you don't have to implement handoff if your vnode is purely computational.  Well, you should write the callbacks but they won't have to do anything.

The players in handoff are `is_empty/1`, `delete/1`, `handoff_starting/2`, `handoff_cancelled/1`, `encode_handoff_item/2`, `handle_handoff_data/2`, and `handle_handoff_command/3`.

### is_empty(State) -> Result ###

    State = NewState = term()
    Result = {true, NewState} | {false, NewState}

Once the container has determined a vnode is out of place it's first action is determine if there is any data to be transfered.  If there is then return _true_ otherwise return _false_.  When a vnode is deemed empty the next `delete/3` callback will be invoked.

### delete(State) -> Result ###

    State = NewState = term()
    Result = {ok, NewState}

The container will invoke this callback when it's determined there is no more data to be transfered.  That is, when `is_empty/1` returns _true_.  Use this time to perform any premptive cleanup of vnode resources.  On return the vnode will be terminated with a `Reason` of `normal` and the `terminate/2` callback will have a chance to make any final cleanup.

### handoff_starting(TargetNode, State) -> Result ###

    TargetNode = node()
    Result = {true, NewState} | {false, NewState}
    State = NewState = term()

Invoked by the container when it's determined a handoff must occur.  The vnode has the final say in whether or not the handoff will occur.  Return _true_ to continue and _false_ to cancel.  A vnode might have some heuristic that determines it's load and choose not to participate in handoff if overloaded at the moment.  The `TargetNode` is the node to transfer the data to.

### handoff_cancelled(State) -> Result ###

    State = NewState = term()
    Result = {ok, NewState}

The _handoff manager_ allows a set number of concurrent handoff operations.  By default it's 4 but this can be adjusted.  If it's determined that the maximum concurrency has been reached then the container will invoke this callback.  You could use this to undo anything you might have done in `handoff_starting/2`.

### encode_handoff_item(K, V) -> Result ###

    K = {Bucket, Key}
    Bucket = riak_object:bucket()
    Key = riak_object:key()
    V = term()
    Result -> binary()

It seems there are still remnants of Riak KV leftover in Riak Core.  Notice the notion of bucket/key and their types.  I think the more general contract is that `K` should be a two-tuple and `V` can be anything.  That said, this callback is used by the container to encode data before crossing the wire.  I.e., it serializes the data.  To get this right you have to know three things:

1) Encode both `K` and `V` together, i.e. they need to go across together.

2) This function must return a binary.

3) This works in concert with `handle_handoff_data/2`.

### handle_handoff_data(BinObj, State) -> Result ###

    BinObj = binary()
    State = NewState = term()
    Result = {reply, ok, NewState}
           | {reply, {error, Error}, NewState}

This callback deserializes handoff data as it comes across.  It's job is to reconstruct the vnode state from the `BinObj` binaries.  If there is a problem decoding the data then reply with `{error, Error}` that describes the failure.

### handle_handoff_command(Request, Sender, State) -> Result ###

    Request = term()
    Sender = sender()
    State = NewState = term()
    Result = {reply, Reply, NewState}
           | {noreply, NewState}
           | {forward, NewState}
           | {drop, NewState}
           | {stop, Reason, NewState}

This callback is very similar to `handle_command/3` but is instead invoked when a request is recieved **during** handoff.  It has two additional possible return types as well: _forward_ and _drop_.  The _forward_ reply will send the request to the target node.  The _drop_ reply will exibit the same behavior as noreply but is used to signify that you are "dropping" this request on the floor.  That is, you won't even attempt to fulfill it.


External State
----------

The word _state_ is already used in the Erlang community to represent the data that is threaded through the various callbacks in behaviors such as `gen_server`.  It's even more confusing when you talk about a `gen_fsm` because the state is actually the state machine's current state, and what is normally called state is often referred to as context.  To confuse matters worse a vnode has to keep state that relates specifically to the vnode such as the parition it's bound to as well as _external state_ that is relevant to your application.  In this case the external state will be various statistics gathered from the incoming entries.  I choose the adjective _external_ because it clearly delineates it from state that is useful only to the vnode and has a scope beyond the vnode that currently holds it.  In the case of Riak KV the external state would be the bitcask container in which your key-values are stored.

If your vnode needs to keep track of external state then you have to code callbacks to handle a _handoff_ situation.  A handoff is when a vnode determines it is no longer on the correct physical node (because a new node was added or a previously downed node just restarted) and it should "handoff" it's external state to the new vnode on the other machine.

In this case there are a few more callbacks you need to implement:

* `is_empty/1`: Called by the vnode container to determine if there is any external state to be transfered

* `encode_handoff_item/2`: Used by the handoff sender to encode an item before sending it to the new target vnode

* `handle_handoff_data/2`: Used by the target vnode to deserialize the incoming data and add it to it's external state

* `handle_handoff_command/3`: Called by the container when a command comes in **while** the vnode is in the middle of a handoff.

* `handoff_starting/2`, `handoff_cancelled/1`, `handoff_finished/2`: Various lifecycle callbacks that we won't need in this example.


Other Examples
----------

Current examples of [Riak Core](https://github.com/basho/riak_core)  in action include [Riak KV](https://github.com/basho/riak_kv) (also just called Riak), [Riak Search](https://github.com/basho/riak_search) and [BashoBanjo](https://github.com/rklophaus/BashoBanjo).  The first two are actual products created and supported by Basho and the third is just something really cool that Rusty Klophaus did.

