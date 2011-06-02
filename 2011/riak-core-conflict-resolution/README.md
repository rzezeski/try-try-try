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

Right, so the returned value on `rts3` is not optimal.  Intuitively one would expect `{ok,19}` just like on `rts2`.  In the case of RTS a `not_found` value is really the ancestor of all other values.  Said another way, it's always the first value.  Furthermore, I know that if `not_found` should be returned by stat vnode then more than likely it just started because it's a fallback vnode.  In that case there are two things I want to do.

1. Count `not_founds` as a successful response but don't include them in the return value.

2. Update the vnode that returned `not_found` with value returned by the other vnode(s), this is typically called _read repair_.



