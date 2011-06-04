Riak Core, Conflict Resolution
==========

* TODO: Intro


Previous Episode
----------

At the end of the last post I left you with a system that could tolerate node failures but wasn't very smart about what it returned.  That is, if one of the first two nodes returned `not_found` then the caller would get back a list of `[not_found, Val]` or something similar.  Lets verify that before moving on.

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

Right, so the returned value on `rts3` is not optimal.  Intuitively one would expect `{ok,19}` just like on `rts2`.  In the case of RTS a `not_found` value is really the ancestor of all other values.  Said another way, it's always the first value.  Furthermore, I know that if `not_found` is returned by the stat vnode then more than likely it just started because it's a fallback vnode.  In that case there are two things I want to do.

1. Count `not_found` as a successful response but don't include it in the return value.

2. Update the vnode that returned `not_found` with the value returned by the other vnode(s), this is typically called _read repair_.


The first thing is really talking about a specific instance of _conflict resolution_ and the second is a specific instance of the idea of _anti-entropy_.

Basic Conflict Resolution
----------

Before diving into _vector clocks_ and abstractions built atop of them such as [statebox](https://github.com/mochi/statebox) I want to demonstrate a very basic conflict resolution mechanism you could put in place.  This will set the stage for the more complete mechanisms.  There are pretty much three things that could happen at the coordinator level in regards to the replies it recieves.

1. "Gravy train" (for you non-English folk that means "easy scenario") - all replies are the same value, ergo return the value

    [27, 27, 27] => 27

2. "Partly cloudy" (yea, I'm mixing metaphors) - a `not_found` or two mixed in with the value, ergo return the value

    [not_found, 27, 27] => 27
    [not_found, not_found, 27] => 27

3. "Oh snap!" - the replies vary in value and a `not_found` may even be mixed in, ergo merge the values into one as best as possible

    [27, 31, 31] => 27
    [27, 31, not_found] => 27


The first case should need no explination, duh!  The second case might seem weird but let me try explaining it another way.  In Dynamo, in relation to the coordinator, the authors state "...it returns all versions it deems to be causally unrelated."  In this cause I'm saying that `not_found` and any value are causally related in that the former is always an ancestor of the later.  In the third case there are a few different ways we can go about things.  First off, you should ignore `not_found` given scenario #2.  That means you're left with two values (`27` and `31`) which you need to do something with.  In the case of RTS I always want to return a single value

TODO: ACtually the first thing to ask is how you reconcile...

The first thing to decide is what you want to return to the client.  You could return both values and let the client reconcile them, also called _semantic reconciliation_.  You could perform conflict resolution before and return the resulting value, also called _syntactic reconciliation_.  You could even do both by returning multiple versions to the client to let it know multiple version were encountered but then perform reconciliation automatically in the server.  In any case because this is an eventually consistent system there are going to be cases where there are multiple values for the same key and you will need to reconcile them somehow.

The second thing to ask is how much does you client need to know (which will also affect how much work it potentially has to do)?  Does your client need to know if there are multiple version of the same object?  Can this conflicts always be resolved in a deterministic way on the server or does it require some other knowledge that only exists on the client which perhaps might even ask for input from the actual user of the software to help perform the reconciliation?  Riak allows you to do just this with the `allow_mult` flag.  If you decide to leave all reconciliation on the server but your conflicts are not always amenable to syntactic reconciliation then you can also use some heuristic to pick a version from the set.  In my example above I just pick the first value in the list.  Another option would be to pick the most frequently occuring value.  Riak uses the _last write wins_ policy which picks the version with the latest timestamp, but since my values don't yet have timestamps attached to them that's not an option.


Both these forms of reconciliation try to **merge** the conflicting versions but you could punt on this altogether and use some heuristic to pick one of the versions.  This is what I have done here, by simply picking the first one in the list.  Another method might be to pick the value that occurs most frequently.  In Riak the default is something called _last write wins_ which keeps the version with the latest timestamp.  The point is to pick something that works for your application.

TODO: Something is not quite right about the above.  To be more clear Riak **uses** LWW to prevent multiple version from every possibly occuring b/c it's trying to enforce `allow_mult=false`.

TODO: What does quorum really mean re Dynamo/Distributed Systems?  Is it "this many nodes responsded?"  Or is it more?  Is it "this many nodes responded AND agreed on the value?"

Basic Anti-Entropy
----------




    for d in dev/dev*; do $d/bin/rts stop; done


