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


Commands
----------

The first thing to wrap your head around is that an incoming request to a Riak Core cluster ends up being translated to a _command_ on your vnode implementation.  For example, when you perform a _GET_ on Riak KV via `curl` it will eventually wind up being handled by the [handle_command](https://github.com/basho/riak_kv/blob/riak_kv-0.14.0/src/riak_kv_vnode.erl#L171) callback in `riak_kv_vnode`.  This means that the first thing to think about when writing your vnode is:

> What commands will I need to implement?

If your vnode simply needs to perform some computation and has no need for external state then congratulations, you're done!  Seriously, besides the `init` callback, `handle_command` is the only thing you need to implement in Riak Core to distribute work.  In fact, to distribute the regexp matching I'll write a vnode that does just this.

Along with matching the entries I also need to potentially update statistics when a match occurs and for that I'll need a vnode that does a little more than just handle commands.

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

