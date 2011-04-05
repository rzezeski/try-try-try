Riak Core, The vnode
==========

In this post I want to discuss the _virtual node_ or _vnode_ for short.  It is the fundamental unit of both work and storage in a Riak Core system and without understanding it you can't successfully build an application with Riak Core.

First, Why Riak Core
----------

Before digging into the vnode I want to spend a little time on answering _why_ you may want to use Riak Core.  Specifically, I want to attempt to address Wilson MacGyver's question on the riak-users mailing list.

> The topic I'd really like coverage is, what can riak-core do that other stack including erlang+otp can't do.

This is a great question, and while I hestitate to make any direct comparsions (for fear of selling something else short) I'd like to talk about what I think makes Riak Core an attractive solution.

The key thing to understand is that Riak Core is essentially an implementation of [Dynamo](http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html).  In fact, from what I understand, Riak Core started it's life on [Andy Gross's](https://github.com/argv0) laptop on a plane ride recently after he had read the Dynamo paper.  In the Dynamo paper there are several data structures and techniques laid out that Amazon used to build _highly available_, robust systems that also met certain _SLAs_ 99.9% of the time.  In the paper they framed these technologies around a key-value store but they are more general than that.

The two main data structures are:

* The _ring_: The ring (which is really a combination of consistent hashing and vnodes) is at the heart of Riak Core's distribution.  It's what routes requests and data across the cluster.

* _vector clocks_: How do you order events in realtion to each other if you can't trust your time source or don't have one?  You use vector clocks.  In short, vector clocks can be used to detect parallel versions of your data that don't have a common ancestor much like a version control system can tell you when you have a merge conflict.



I feel like there has been a growing interest in riak_core recently and I thought I'd share an example that I coded up as a reaction to an interview question posed to me recently.  Current examples of [Riak Core](https://github.com/basho/riak_core)  in action include [Riak KV](https://github.com/basho/riak_kv) (also just called Riak), [Riak Search](https://github.com/basho/riak_search) and [BashoBanjo](https://github.com/rklophaus/BashoBanjo).  The first two are actual products created and supported by Basho and the third is just something really cool that Rusty Klophaus did.  None of these examples seem to illustrate exactly how to use Riak Core.  I'm hoping this example can help fill that void.

An Interview Question
----------

I was recently posed with an interview question, over skype, that went something like this:

> You have N machines writing syslog events to a server somewhere.  Each incoming entry should be compared to a list of regular expressions on the server.  Each regexp has a corresponding event that should be triggered if a match occurs.  Said event will write/udpate a value somewhere that can be queried by interested parties.  How do you implement it?

Did I already say that this was completely over skype?  I had to talk out my entire solution.  I probably asked to hear the problem over again about seven different times.  It's amazing how much you take a whiteboard for granted until you can't use one.  Luckily, my interviewers were calm and patient and I ended up giving a half decent solution to the problem I thought.  They hired me, at least :)

During the interview I realized this could actually be a great fit for Riak Core.  I didn't get to share this idea with the interviewers that day but that's okay because today I'll share it  with all you wounderful people that are curious about Riak Core.  Let's get to it, then.

What's a vnode, Anyways?
----------

The name _vnode_ is short for _virtual node_.  This is in contrast to a regular _node_ which is just another name for a physical machine.  For all intents and purposes a vnode is simply a virtual machine that runs on a real machine.  No, not a virutal machine like the JVM, but instead more like a virtual host in a webserver.  The vnode acts as a unit of work or storage (or both) which makes it the fundamental measure of concurrency in Riak Core.  A vnode's job is to handle requests and Riak Core provides the tools for distributing those requests across the vnodes.  You code to the interface provided by the vnode behavior and Riak Core does most of the rest of the work.  I'll try to clarify all these points as I go along.

A vnode has two primary objectives in life:

1) Handle incoming requests, called _commands_

2) Keep track of any _external state_

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
