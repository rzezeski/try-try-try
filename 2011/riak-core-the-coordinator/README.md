Riak Core, The Coordinator
==========

What is a Coordinator?
----------

Logically speaking, a _coordinator_ is just what it sounds like.  It's job is to coordinate incoming requests.  It enforces the consistency semantics of [N, R and W](http://wiki.basho.com/Riak-Glossary.html#Quorum) and performs anti-entropy services like [read repair](http://wiki.basho.com/Riak-Glossary.html#Read-Repair).  In simpler terms, it's responsible for distributing data across the cluster and re-syncing data when it finds conflicts.  You could think of vnodes as the things that Get Shit Done (TM) and the coordinators as the other things telling them what to do and overseeing the work.  They work in tandem to make sure your request is behind handled as best as it can.

To be more concrete a coordinator is a [gen_fsm](http://www.erlang.org/doc/man/gen_fsm.html).  Each request is handled in it's own Erlang process.  A coordinator communicates with the vnode instances to fulfill requests.

To wrap up, a coordinator

* coordinates requests

* enforces the consistency requirements

* performs anti-entropy

* is an Erlang process that implements the `gen_fsm` behavior

* communicates with the vnode instances to execute the request

WARNING
----------

**READ THE FOLLOWING AT YOUR OWN RISK...STILL A WORK IN PROGRESS AND MAY COMPLETELY CHANGE**

At the end of my [vnode](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-the-vnode) post I asked the question _Where's the redundancy?_  There is none in RTS, thus far.  Riak Core isn't magic but rather a suite of tools for building distirbuted, highly available systems.  You have to build your own redundancy, but Riak Core offers several tools to help.  In this post I'll show you how to build soething called a _coordinator_ with the help of Riak Core.

What is a Coordinator?
----------

A coordinator in Riak Core is much like it's name suggests.  It's role is to coordinate the incoming request.  In the [Dynamo](http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html) paper the authors specifically discuss get & put coordinators.  In general, the coordinator is responsible for enforcing the consistency model of the objects your system stores.  I.e. put and get are two types of coordinators that relate to the model of a key-value store.

* A coordinator coordinates requests.

* A coordinator enforces your object's consistency model.

* A coordinator is a [gen_fsm](http://www.erlang.org/doc/man/gen_fsm.html).

* A coordinator is an Erlang process under a [simple_one_for_one](http://www.erlang.org/doc/design_principles/sup_princ.html#id69831) supervisor.  For the uninitiated this means each incoming request is concurrent with respect to all others and has it's own dedicated coordinator.

* A coordinator calls [vnode](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-the-vnode) APIs to service requests.


What's a "Consistency Model?"
----------

The consistency model is the guarentees (or lack thereof) that your system makes to it's clients about the objects it stores.  E.g. in Dynamo/Riak one talks about things like `N`, `R` and `W` to describe consistency requirements.  In Riak a put coordinator will enforce the client's expectations when it makes a write request with `W=3`.  The point is that your applications consistency model may be simpler or more advanced than that found in Riak and it's the job of your coordinator to enforce it.

Sidenote:  I wonder, however, if there are general forms of coordinators that would be amenable to a suite of Erlang behaviors?  If you look at the [Riak code](https://github.com/basho/riak_kv) you'll see there is a fair amount of shared structure among the coordinators.
