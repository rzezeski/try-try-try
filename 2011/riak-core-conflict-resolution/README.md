Riak Core, Conflict Resolution
==========

In the [previous post](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-the-coordinator) I explained the role of a coordinator and showed how to implement one.  In this post I want to discuss the idea of _eventual consistency_ and how a system based on [Riak Core](https://github.com/basho/riak_core) goes about enforcing consistency in the face of chaos.

When I think about _consistency_ I like to think about it in terms of entropy or chaos.  That is, entropy is the lack of consistency and consistency is the removal of entropy.  It's much like Socrates's _Theory of Opposites_ in that one must arise from the other and there is a process which transforms one to the other.  In the case of distributed systems _eventual consistency_ is the process of taking a system from entropy to consistency.

That's all well and good, and if you like philosophy you might even find my definition cute, but it doesn't necessarily help you, the reader, understand the situation any better.  I'll start with something most of us understand, a single-machine RDBMS, and move towards something like Riak by comparing the two in terms of consistency.  I think it's safe to say most of us have at least a basic understanding of how a single-machine RDBMS works.  You build a schema, maybe write some constraints, and then insert and query data via SQL.  More importantly, there is a notion of something called a _transaction_ and it enforces a set of rules ubiquitously referred to by the acronym _ACID_ (Atomic Consistent Isolated Durable).  Notice the word _Consistent_ is baked right into the acronym but that's a different consistency than I'm talking about here.  No, I'm more interested in the _Atomic_ and _Isolated_ parts of the acronym.  It's these parts of ACID that give a RDBMS it's predictable behavior even in the face of concurrency.  One transaction cannot affect another concurrent transaction (i.e. one can't see the others modifications) and the **entire** transaction must succeed or fail (i.e. there is no chance of partial completion).  You can take the pessimistic approach and lock the data while a transaction is taking place or you can be optimistic and perform transactions concurrently with the potential that some might have to be retried.  In any case, the RDBMS will ensure the transaction has completed in full and all other transactions will see the new state before they may complete.  This means that modifications are serialized, i.e. applied in-full one after another always working with the "latest" state of the database.  Thought of another way, a RDBMS system enforces consistency up-front, before the data is written.  However, this up-front consistency doesn't come free.  It requires coordination in order to enforce ACID.  In the case of a single-machine deployment this could mean rising latency on a highly contended piece of data.  In the case of a multi-machine deployment we are talking about not only rising latencies but also the lose of availability if one of the nodes goes down or a network partition occurs.  After all, how can you enforce consistency across all nodes if all nodes cannot talk to each other?

Riak takes a different approach.  It embraces potential inconsistency in return for lower latency and availability.  If a node goes down or the network partitions the data is still available for read and write, it just might become inconsistent causing multiple concurrent versions to exist.  Does that mean Riak just throws up it's hands and returns crap data?  Absolutely not, it pushes the enforcement of consistency to read time and furthermore only considers consistency in terms of the nodes it can see.  That is, if the cluster splits into two then a read won't fail but instead will use the data it has access to determine a response.  When the cluster repairs and another read occurs the system will then take that time to perform _conflict resolution_ in order to reconcile any inconsistencies that were created by the cluster split.  That is, Riak and systems like it build atop Riak Core, are lazy in their enforcement of consistency.  Choosing to delay it until absolutely needed.

Notice I am not saying one is better than the other.  Both have their place and the ultimate system would allow you to choose the semantics of your data storage on an as-needed basis.  To a certain extent Riak allows you to do this via it's quorum parameters but even something like `W = M` (that is, all writes must finish before call returns) doesn't get you the consistency found in RDBMS.

Unfortunately, Riak Core doesn't do a whole lot to guide you in this department.  In it's defense, until recently, Core was just a bunch of code hard-wired into Riak and it's covering new ground in the sense that it attempts to be a general framework or foundation to build distributed, scalable, highly-available applications.  Furthermore, the idea of _consistency_ and how to enforce it seem to be very application dependent and it will take time for us to discover the generalities and encode them in Core.  There is the [riak_object](https://github.com/basho/riak_kv/blob/master/src/riak_object.erl) which contains a basic wrapper for storing data in a Riak Core app utilizing vector clocks to detect entropy but unfortunately it's relevant bits haven't been pulled down to Core yet.  There is also work being done by others in this area such as Mochi Media's [statebox](https://github.com/mochi/statebox) abstraction which can be stored as a value inside a `riak_object` and allows you to resolve conflicts specific to certain types of data structures such as sets (which I'll cover later in this post).

Previous Episode
----------

At the end of the last post I left you with a system that could tolerate node failures but wasn't very smart about how it acted afterward.  That is, if one of the first two nodes returned `not_found` then the caller would get back a list of `[not_found, Val]`.  Lets verify that before moving on.

    make
    make devrel
    
    for d in dev/dev*; do $d/bin/rts start; done
    for d in dev/dev{2,3}; do $d/bin/rts-admin join rts1@127.0.0.1; done
    
    gunzip -c progski.access.log.gz | head -20 | ./replay --devrel progski
    ./dev/dev1/bin/rts attach
    
    (rts1@127.0.0.1)1> rts:get("progski", "total_reqs").
    {ok,19}
    (rts1@127.0.0.1)2> os:getpid().
    "33431"
    Ctrl^D
    kill -9 33431
    
    ./dev/dev2/bin/rts attach
    (rts2@127.0.0.1)1> rts:get("progski", "total_reqs").
    {ok,19}
    Ctrl^D
    
    ./dev/dev3/bin/rts attach
    (rts3@127.0.0.1)1> rts:get("progski", "total_reqs").
    {ok,[not_found,19]}
