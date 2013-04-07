Riak Core, Conflict Resolution
==========

In the
[previous post](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-the-coordinator)
I explained the role of a coordinator and showed how to implement one.
In this post I want to discuss the idea of _eventual consistency_ and
how a system based on [Riak Core](https://github.com/basho/riak_core)
goes about enforcing consistency in the face of chaos.

When I think about _consistency_ I like to think about it in terms of
entropy.  Entropy is the lack of consistency and consistency is the
removal of entropy.  It's much like Socrates's _Theory of Opposites_
in that one must arise from the other and there is a process which
transforms one to the other.  In the case of distributed systems,
processes like _conflict resolution_ and _read repair_ constantly work
to remove entropy from a system.  In a non-quiescent system these
processes are working towards _eventual consistency_.

That's all well and good, but what does this look like to reify this
idea?  I'll start with something most of us understand, a RDBMS, and
move towards something like Riak by comparing the two in terms of
consistency.  I think it's safe to say most of us have at least a
basic understanding of how a RDBMS works.  You build a schema, maybe
write some constraints, and then insert and query data via SQL.  More
importantly, there is a notion of something called a _transaction_ and
it enforces a set of rules ubiquitously referred to by the acronym
_ACID_ (Atomic Consistent Isolated Durable).  Notice the word
_Consistent_ is baked right into the acronym but that's a different
consistency than I'm talking about here.  No, I'm more interested in
the _Atomic_ and _Isolated_ parts of the acronym.  It's these parts of
ACID that give a RDBMS its predictable behavior in the face of
concurrency.  One transaction cannot affect another concurrent
transaction (i.e. one can't see the other's modifications) and the
**entire** transaction must succeed or fail (i.e. there is no chance
of partial completion).  This means that modifications are serialized,
i.e. applied in-full one after another always working with the
"latest" state of the database.  Thought of another way, a RDBMS
system enforces consistency up-front, before the data is written.
This up-front consistency doesn't come free.  It requires coordination
in order to enforce ACID.  In the case of a single-node deployment
this could mean rising latency on a highly contended piece of data.
In the case of a multi-node deployment we are talking about not only
rising latencies but also the loss of availability if one of the nodes
goes down or a network partition occurs.  After all, how can you
enforce consistency across all nodes if all nodes cannot talk to each
other?

Riak takes a different approach.  It embraces potential inconsistency
in return for lower latency and availability.  If a node goes down or
the network partitions the data is still available for read and write.
In certain scenarios the data may become inconsistent causing multiple
concurrent versions to exist.  Does that mean Riak just throws up its
hands and returns crap data?  Absolutely not, it pushes the
enforcement of consistency to read time.  Furthermore, it only
considers consistency in terms of the nodes it can see.  That is, if
the cluster splits into two then a read won't fail but instead will
use the data to which it has access to determine a response.  When the cluster
repairs and another read occurs the system will then take that time to
perform _conflict resolution_ in order to reconcile any
inconsistencies that were created by the cluster split.  That is, Riak
and systems like it built atop Riak Core, are lazy in their
enforcement of consistency, choosing to delay it until absolutely
needed.

Notice I am not saying one is better than the other.  Both have their
place and the ultimate system would allow you to choose the semantics
of your data storage on an as-needed basis.  To a certain extent Riak
allows you to do this via its quorum parameters but even something
like `W = N` (that is, all writes must finish before a call returns)
doesn't get you the consistency found in RDBMS.  Said as succinctly as
possible, RDBMS says "all of you [nodes] must agree **right now** or I
will fail the write" whereas something Riak-like says "at some point
in the future all of you [nodes] should agree."  There are cases for
both.

Unfortunately, Riak Core doesn't do a whole lot to guide you in this
department.  The idea of _consistency_ and how to enforce it in a
distributed system seem to be very application dependent and it will
take time for Basho to discover the generalities and encode them in
Core.  There is the
[riak_object](https://github.com/basho/riak_kv/blob/1.0/src/riak_object.erl)
which contains a basic wrapper for storing data in a Riak Core app
utilizing vector clocks to detect entropy but unfortunately its
relevant bits haven't been pulled down to Core yet.  There is also
work being done by others in this area such as Mochi Media's
[statebox](https://github.com/mochi/statebox) abstraction which can be
stored as a value inside a `riak_object` and allows you to resolve
conflicts specific to certain types of data structures such as sets.


Previous Episode
----------

At the end of the last post I left you with a system that could
tolerate node failures but wasn't very smart about how it acted
afterward.  That is, if one of the first two nodes returned
`not_found` then the caller would get back a list of
`[not_found, Val]`.  Lets verify that before moving on.

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

As you can see RTS doesn't do anything in the way of resolving the
conflicting values.  Besides, even if it did that wouldn't help much
since it does nothing to repair these conflicts.  The rest of the post
is about fixing this.

Conflict Resolution
----------

The first step to building an eventually consistent system is
determining how to detect entropy.  With a conflict in hand you then
need to have some sort of plan on how to reconcile it.  I will show
how I implemented these processes in RTS.

### Vector Clocks ###

Like Riak, I decided to use
[vector clocks](http://wiki.basho.com/Vector-Clocks.html) to detect
conflicting versions of the same object.  I created the
[rts_obj](https://github.com/rzezeski/try-try-try/blob/master/2011/riak-core-conflict-resolution/rts/src/rts_obj.erl)
module to wrap the use of vclocks in a nice, contained package.  If
you take a peek at
[riak_object](https://github.com/basho/riak_kv/blob/riak_kv-0.14.2/src/riak_object.erl)
you might notice some similarities.  Hopefully in the future we will
bring some of this down to Core as it seems to be generally useful.
So what exactly are vector clocks?

There are already posts about why vclocks are
[easy](http://blog.basho.com/2010/01/29/why-vector-clocks-are-easy/)
or
[hard](http://blog.basho.com/2010/04/05/why-vector-clocks-are-hard/)
depending on your perspective.  In this post I want to focus on the
fact that vector clocks allow you to give a logical ordering to
multiple versions of the same object.  By assigning a logical timeline
to each version you can then compare them to determine if a split
occurred at some point in time.  If a split has occurred then it means
there is potential that each version has data that the other is
missing.  However, that is dependent on the data your object is
storing.  For example, in the case of a set where elements are only
ever added it hardly matters that two parallel version had the same
element added because that would result in the same set.  Even in the
case where different elements were added resolving their differences
would be a simple matter of performing a union of the sets.  This is
the case for the `agents` stat in RTS.  On the flip side are the
counter stats such as `total_sent` which keeps track of the total
number of bytes sent by a webserver.  If there are parallel versions
then that means each version is missing the byte counts sent by the
other versions.

<table>
  <tr>
    <th>Node A</th>
    <th>Node B</th>
    <th>Node C</th>
  </tr>

  <tr>
    <td align="center" colspan="3">total_sent + 500 on Coordinator A</td>
  </tr>

  <tr>
    <td>500 [{A,1}]</td>
    <td>500 [{A,1}]</td>
    <td>500 [{A,1}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">total_sent + 200 on Coordinator A</td>
  </tr>

  <tr>
    <td>700 [{A,2}]</td>
    <td>700 [{A,2}]</td>
    <td>700 [{A,2}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">total_sent + 350 on Coordinator C</td>
  </tr>

  <tr>
    <td>1050 [{A,2}, {C,1}]</td>
    <td>1050 [{A,2}, {C,1}]</td>
    <td>1050 [{A,2}, {C,1}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">Network Split -- (A,B), (C) </td>
  </tr>

  <tr>
    <td align="center" colspan="3">total_sent + 100 on Coordinator C</td>
  </tr>

  <tr>
    <td>1050 [{A,2}, {C,1}]</td>
    <td>1050 [{A,2}, {C,1}]</td>
    <td>1150 [{A,2}, {C,2}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">total_sent + 500 on Coordinator B</td>
  </tr>

  <tr>
    <td>1550 [{A,2}, {B,1}, {C,1}]</td>
    <td>1550 [{A,2}, {B,1}, {C,1}]</td>
    <td>1150 [{A,2},        {C,2}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">Network Repaired -- (A,B,C)</td>
  </tr>

  <tr>
    <td align="center" colspan="3">total_sent + 50 on Coordinator A</td>
  </tr>

  <tr>
    <td>1600 [{A,3}, {B,1}, {C,1}]</td>
    <td>1600 [{A,3}, {B,1}, {C,1}]</td>
    <td>1200 [{A,3},        {C,2}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">GET total_sent on Coordinator A</td>
  </tr>
</table>

In the case above the object versions on `A` and `B` are missing `100`
bytes that were added on `C` during the split.  If you look at the
vector clocks you can see that `A` and `B` are identical but different
from `C` which has one more operation logged under the `C`
coordinator.  This indicates that these versions are conflicting and
must be resolved.

This conflict detection is handled by the `rts_obj`, specifically the
`merge` function is called by the coordinator (`rts_get_fsm`) to merge
the vnode replies into one.  Typically, the replies will have a
sequential, non-parallel ordering and merge will simply return the
latest object in the logical timeline.  Otherwise, if parallel version
exist, it will perform reconciliation of the values and merge the
vector clocks.

    merge([#rts_obj{}|_]=Objs) ->
        case rts_obj:children(Objs) of
            [] -> not_found;
            [Child] -> Child;
            Chldrn ->
                Val = rts_get_fsm:reconcile(lists:map(fun val/1, Chldrn)),
                MergedVC = vclock:merge(lists:map(fun vclock/1, Chldrn)),
                #rts_obj{val=Val, vclock=MergedVC}
        end.

As you can see `merge` doesn't really detect conflicts itself but
delegates to the `children/1` function.  The only way multiple
children can occur is if conflicts exist.  If only one child exists
than that must mean all objects follow linearly from one to the next.
If you remove all ancestors from the unique list of objects then what
you have left are the children.  Don't just take my word for it.
Think about it and make sure you agree.

    %% @doc Given a list of `rts_obj()' return a list of the children
    %% objects.  Children are the descendants of all others objects.
    children(Objs) ->
        unique(Objs) -- ancestors(Objs).

If you're still not convinced, perhaps looking at the definitions for
`unique/1` and `ancestors/1` will help.  Note that `not_found` is a
special case in that it is considered an ancestor of all values and
therefore is generally filtered out and ignored by most operations.

    %% @doc Given a list of `rts_obj()' return a list of all the
    %% ancestors.  Ancestors are objects that all the other objects in the
    %% list have descent from.
    -spec ancestors([rts_obj()]) -> [rts_obj()].
    ancestors(Objs0) ->
        Objs = [O || O <- Objs0, O /= not_found],
        As = [[O2 || O2 <- Objs,
                     ancestor(O2#rts_obj.vclock,
                              O1#rts_obj.vclock)] || O1 <- Objs],
        unique(lists:flatten(As)).

    %% @doc Predicate to determine if `Va' is ancestor of `Vb'.
    -spec ancestor(vclock:vclock(), vclock:vclock()) -> boolean().
    ancestor(Va, Vb) ->
        vclock:descends(Vb, Va) andalso (vclock:descends(Va, Vb) == false).

    %% @doc Given a list of `Objs' return the list of uniques.
    -spec unique([rts_obj()]) -> [rts_obj()].
    unique(Objs) ->
        F = fun(not_found, Acc) ->
                    Acc;
               (Obj, Acc) ->
                    case lists:any(equal(Obj), Acc) of
                        true -> Acc;
                        false -> [Obj|Acc]
                    end
            end,
        lists:foldl(F, [], Objs).

Detecting conflicts is half the battle.  The other half is determining
how to resolve them.


### Reconciling Conflicts ###

In order to reconcile conflicts in your system you have to know about
the type of data being stored.  For example, in Riak the data is an
opaque blob which means Riak can't really do much on its own to
resolve a conflict because it doesn't understand anything about the
data.  By default Riak will implement a _Last Write Wins_ (LWW)
behavior which will select the latest object version based on wall
clock time.  If this is unacceptable then the user has the option to
disable this feature by setting `allow_mult` to true which will allow
multiple versions to coexist and upon a read all versions will be
returned to the caller.  The caller can then reconcile the versions
with full context.

It's not always enough to have **just** the conflicting objects to
perform reconciliation.  Many times you will need supplementary data
that puts those versions in context.  In the example above the two
versions of the object would be returned with values `1600` and
`1200`.  Now, I hope it's clear to everyone that you can't just add
these two numbers to get the reconciled version, because that would be
too large a number.  Likewise, you can't just pick the largest number
because that would be too small a number.  In order to determine the
correct number more context is needed.  One way to do this (but not
the only way, there was a very recent addition to statebox to do the
same thing) is by a separate count for each coordinator.  In that case
the total is always the sum of the max of all coordinators.  I'll
demonstrate this with an example similar to above but instead using
the `incr` operation which is really just a specific instance of
`incrby`.

<table>
  <tr>
    <th>Node A</th>
    <th>Node B</th>
    <th>Node C</th>
  </tr>

  <tr>
    <td align="center" colspan="3">incr on Coordinator A</td>
  </tr>

  <tr>
    <td>1 [{A,1}]</td>
    <td>1 [{A,1}]</td>
    <td>1 [{A,1}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">incr Coordinator A</td>
  </tr>

  <tr>
    <td>2 [{A,2}]</td>
    <td>2 [{A,2}]</td>
    <td>2 [{A,2}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">incr on Coordinator C</td>
  </tr>

  <tr>
    <td>3 [{A,2}, {C,1}]</td>
    <td>3 [{A,2}, {C,1}]</td>
    <td>3 [{A,2}, {C,1}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">Network Split -- (A,B), (C) </td>
  </tr>

  <tr>
    <td align="center" colspan="3">incr on Coordinator C</td>
  </tr>

  <tr>
    <td>3 [{A,2}, {C,1}]</td>
    <td>3 [{A,2}, {C,1}]</td>
    <td>4 [{A,2}, {C,2}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">incr on Coordinator B</td>
  </tr>

  <tr>
    <td>4 [{A,2}, {B,1}, {C,1}]</td>
    <td>4 [{A,2}, {B,1}, {C,1}]</td>
    <td>4 [{A,2},        {C,2}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">Network Repaired -- (A,B,C)</td>
  </tr>

  <tr>
    <td align="center" colspan="3">incr on Coordinator A</td>
  </tr>

  <tr>
    <td>5 [{A,3}, {B,1}, {C,1}]</td>
    <td>5 [{A,3}, {B,1}, {C,1}]</td>
    <td>5 [{A,3},        {C,2}]</td>
  </tr>

  <tr>
    <td align="center" colspan="3">GET total_sent on Coordinator A</td>
  </tr>
</table>

The keen reader will notice that the context data is isomorphic to a
vector clock so I just use the same structure to represent both.  In
fact, an earlier iteration of the code used the vector clock directly
to resolve conflicts until I realized this was probably poor form so I
added an explicit context structure.  What should the resolved value
be?  Counting all occurrences of `incr` in the table above would
rightly get you the value `6`, but my proposed context doesn't store
every incr op performed, or does it?  Notice that the context stores
the total number of operations performed by each coordinator and we
know each of these is an `incr` operation.  By summing the max count
for each coordinator we get the correct result.

    Reconciled Object = {A,3} + {B,1} + {C,2} => 6 [{A,3}, {B,1}, {C,2}]

Another way to reason about this is that during the split both the
`AB` and `C` cluster missed a count.  Since both have seen `5` counts
up to the read adding `1` to either side will arrive at `6`.

The excerpt below comes from
[rts_stat_vnode](https://github.com/rzezeski/try-try-try/blob/master/2011/riak-core-conflict-resolution/rts/src/rts_stat_vnode.erl)
and is responsible for not only incrementing the value (as it did
before) but now must also keep tract of the context which is stored
under the `#incr` record's element `counts`.  The `counts` element
contains a dict that maps `Coordinator` nodes to their respective
total count.  Notice that the `Coordinator` value is passed down from
the coordinator itself (remember from the last post it is the
coordinator that interacts with the vnodes).  This is important
because the vnode, by nature, is probably running on a different node
from the coordinator and you only want to bump the count of the node
coordinating the operation.

    handle_command({incrby, {ReqID, Coordinator}, StatName, IncrBy}, _Sender, #state{stats=Stats0}=State) ->
        Obj =
            case dict:find(StatName, Stats0) of
                {ok, #rts_obj{val=#incr{total=T0, counts=C0}}=O} ->
                    T = T0 + IncrBy,
                    C = dict:update_counter(Coordinator, IncrBy, C0),
                    Val = #incr{total=T, counts=C},
                    rts_obj:update(Val, Coordinator, O);
                error ->
                    Val = #incr{total=IncrBy,
                                counts=dict:from_list([{Coordinator, IncrBy}])},
                    VC0 = vclock:fresh(),
                    VC = vclock:increment(Coordinator, VC0),
                    #rts_obj{val=Val, vclock=VC}
            end,
        Stats = dict:store(StatName, Obj, Stats0),
        {reply, {ok, ReqID}, State#state{stats=Stats}};


The next excerpt is the code responsible for reconciling divergent
versions of `#incr` values.  I feel like I could probably clean this
up but the main thing to take away is that it uses the context data to
determine the max count for each coordinator and then sums them to
arrive at the correct total.  Make sure to notice that it returns a
new `#incr` value that has both the new `Total` **and** the max count
from each coordinator.

    -spec reconcile([A::any()]) -> A::any().
    reconcile([#incr{}|_]=Vals) ->
        Get = fun(K, L) -> proplists:get_value(K, L, 0) end,
        Counts = [dict:to_list(V#incr.counts) || V <- Vals],
        Nodes = unique(lists:flatten([[Node || {Node,_} <- C] || C <- Counts])),
        MaxCounts = [{Node, lists:max([Get(Node, C) || C <- Counts])}
                     || Node <- Nodes],
        Total = lists:sum([lists:max([Get(Node, C) || C <- Counts])
                           || Node <- Nodes]),
        #incr{total=Total, counts=dict:from_list(MaxCounts)};


That's all well and good for reconciling counters, but what about
other types of data?


### Reconciling Conflicts With Statebox ###

In RTS, along with counters, there are also sets.  Currently sets are
only used to keep track of the various user agents that have hit a
webserver.  This means data is only added to the set and
reconciliation can be as simple as a union.  However, the second you
allow a delete operation to occur this won't work.  Once again I'll
use an example to demonstrate why.

Lets say I add a stat to RTS that tracks user login/logout events and
keep track of all currently logged-in users via a set.

<table>
  <tr>
    <th>Node A</th>
    <th>Node B</th>
  </tr>

  <tr>
    <td align="center" colspan="3">user_login rzezeski on coordinator A</td>
  </tr>

  <tr>
    <td>{rzezeski}</td>
    <td>{rzezeski}</td>
  </tr>

  <tr>
    <td align="center" colspan="3">user_login whilton on coordinator B</td>
  </tr>

  <tr>
    <td>{rzezeski, whilton}</td>
    <td>{rzezeski, whilton}</td>
  </tr>

  <tr>
    <td align="center" colspan="3">Network Split -- (A) (B)</td>
  </tr>

  <tr>
    <td align="center" colspan="3">user_logout rzezeski on coordinator A</td>
  </tr>

  <tr>
    <td>{whilton}</td>
    <td>{rzezeski, whilton}</td>
  </tr>

  <tr>
    <td align="center" colspan="3">user_logout whilton on coordinator B</td>
  </tr>

  <tr>
    <td>{whilton}</td>
    <td>{rzezeski}</td>
  </tr>

  <tr>
    <td align="center" colspan="3">Partition Heal -- (A,B)</td>
  </tr>

  <tr>
    <td align="center" colspan="3">GET online_users</td>
  </tr>
</table>

After the partition heals if RTS were to simply union the sets then it
would appear as if both `rzezeski` and `whilton` are still online when
in fact they are both offline.  Zoinks!  As with counters, we need
context to resolve this correctly.  More specifically, we need to know
the operations that occurred during the network split so that we may
replay them after it has been healed.  This is exactly what
[statebox](https://github.com/mochi/statebox) provides you with.

Essentially, statebox provides a **window** of events that lead up to
the current value.  I emphasize window because it's limited in scope.
You can't remember every event because then it becomes too expensive
both in the space to store it and the time to traverse it (replay
events).  Plus, if your cluster is well connected most of the time
there is no reason to remember older events that have since been
propagated throughout.  The tricky part is when a partition occurs.
While statebox saves you a lot of trouble it isn't fool proof.  The
key is that your statebox window must be larger than the partition
window (it terms of time and number of operations) or else you
**will** lose events and thus lose data.  Whether that's acceptable
depends entirely on your specific application and its data
requirements.

Returning to the example, statebox would determine that even though
the union of the two sets is indeed `{rzezeski, whilton}` that two
deletes have occurred, one for each user, and that the correct value
is actually an empty set, `{}`.  Statebox does this by selecting one
of the values and then applying the union of all **operations** that
have occurred.  This means statebox is limited in the type of values
in can deal with.  Please see this
[post](http://labs.mochimedia.com/archive/2011/05/08/statebox/) for
more details.

The excerpt below shows how `rts_stat_vnode` uses statebox to keep
track of the set operations `sadd` and `srem`.  First off, notice that
I bound my window strictly by time.  That is, each statebox will track
all operations that have occurred within the `?STATEBOX_EXPIRE`
window.  That means the number of operations tracked is unbounded and
could potentially run amuck if there was a sudden write spike.  This
also means that if a partition split lasts for longer than
`?STATEBOX_EXPIRE` then there is potential to lose data.  I say
potential because expiration must be performed explicitly and is only
done during write time in RTS, as seen below.  If the network was to
split but writes only occurred inside the window but the network
wasn't healed until after no data would be lost because those older
events haven't been expired yet.

    handle_command({sadd, {ReqID, Coordinator}, StatName, Val},
                   _Sender, #state{stats=Stats0}=State) ->
        SB = 
            case dict:find(StatName, Stats0) of
                {ok, #rts_obj{val=SB0}=O} ->
                    SB1 = statebox:modify({sets, add_element, [Val]}, SB0),
                    SB2 = statebox:expire(?STATEBOX_EXPIRE, SB1),
                    rts_obj:update(SB2, Coordinator, O);
                error ->
                    SB0 = statebox:new(fun sets:new/0),
                    SB1 = statebox:modify({sets, add_element, [Val]}, SB0),
                    VC0 = vclock:fresh(),
                    VC = vclock:increment(Coordinator, VC0),
                    #rts_obj{val=SB1, vclock=VC}
            end,
        Stats = dict:store(StatName, SB, Stats0),
        {reply, {ok, ReqID}, State#state{stats=Stats}};


If it wasn't clear already, statebox provides you with the means to
reconcile conflicting objects (so long as they are amenable to being
managed by statebox).  This is no more clearly shown than with the
fragment below which shows how one reconciles with statebox.  Pretty
simply, eh?  Thanks Bob!

    reconcile([V|_]=Vals) when element(1, V) == statebox -> statebox:merge(Vals).

Earlier I mentioned how something like the `agents` stat doesn't
really need much in the way of reconciliation because it's an
append-only set.  Even so I find it makes more sense just use statebox
on all set-backed data regardless because it makes the code easier to
reason about.


Read Repair
----------

At this point I've shown you how to detect conflicts and reconcile
them, but that's still not enough.  After the system detects a
conflict it should fix it so that it doesn't have to repeat the
reconciliation process for each subsequent read.  One method is
something called _read repair_ which takes advantage of the fact that
the data has already been accessed so the system might as well go the
last mile and repair any replicas that are in conflict with the
reconciled object.  I like to refer to this type of anti-entropy as
_passive anti-entropy_ because it is secondary to some other primary
action.  This is opposed to _active anti-entropy_ such as background
gossiped _Merkle Trees_ which is preemptive and a primary action in
its own right.

In RTS I added a state for the read coordinator (`rts_get_fsm`) named
`finalize`.  This state will not be entered until all `N` replies
have been accounted for.  At this point it performs another merge.
The reason for the second merge (remember the first one is done after
the `R` quorum is met) and thus not just using the value of the first
merge is because `R` may be less than `N` and in that case not all
objects were accounted for in the first merge.  After the merge is
performed a check is made to determine if and where repairs need to be
made and then performs them.

    finalize(timeout, SD=#state{replies=Replies, stat_name=StatName}) ->
        MObj = merge(Replies),
        case needs_repair(MObj, Replies) of
            true ->
                repair(StatName, MObj, Replies),
                {stop, normal, SD};
            false ->
                {stop, normal, SD}
        end.


Failure Scenarios
----------

Now that I've explained how to detect, resolve, and repair conflicts I
want to discuss various failure scenarios and demonstrate how these
features come together to provide consistency.  I will cover only the
scenarios that jump out to me as trying to cover all the different
variations may lead to madness for both you and me.  I'll also be
quoting shell sessions with RTS so that you can follow along if you
like.

**NOTE: All of these examples assume that data has already been
  written via the following commands.**

    for d in dev/dev*; do $d/bin/rts stop; done
    for d in dev/dev*; do $d/bin/rts start; done
    for d in dev/dev{2,3}; do $d/bin/rts-admin join rts1@127.0.0.1; done
    ./dev/dev1/bin/rts-admin ringready
    gunzip -c progski.access.log.gz | head -20 | ./replay --devrel progski

It's probably also worth explaining two functions I'll be using to
help me test and demonstrate my scenarios.  The first is
`get_dbg_preflist` which returns a list of `{{Index, Node}, Obj}`.
That is it shows the mapping of vnodes to their respective values.
The second function is `dbg_op` which allows me to fake partitioned
writes.  A partitioned write is one that does not reach all its
destination vnodes.

### Node Goes Down ###

I think the first, most obvious, and probably simplest to reason about
is the case where a single node crashes.

    ./dev/dev1/bin/rts attach
    (rts1@127.0.0.1)1> rts:get("progski", "GET").
    19
    (rts1@127.0.0.1)2> rts:get_dbg_preflist("progski", "GET").
    ...
    Ctrl^C Ctrl^C

First I check for sanity on `rts1` and then I run `get_dbg_preflist`
to see the objects stored on each vnode.  In this case they should be
identical.  Then I kill this node and attach to `rts2`.

    ./dev/dev2/bin/rts attach
    (rts2@127.0.0.1)1> rts:get_dbg_preflist("progski", "GET").
    ...
     {{274031556999544297163190906134303066185487351808,
       'rts2@127.0.0.1'},
      not_found}]
    ...
    (rts2@127.0.0.1)2> rts:get("progski", "GET").
    19
    (rts2@127.0.0.1)3>
    =ERROR REPORT==== 16-Jun-2011::22:34:01 ===
    repair performed {rts_obj,{incr,19,...
    (rts2@127.0.0.1)3> rts:get("progski", "GET").
    19

The first thing to do on `rts2` is to verify, via `get_dbg_preflist`,
that only two vnodes contain the correct value while the 3rd has the
value `not_found`.  I extracted part of the return value to
demonstrate this above.  Next, perform a read.  At this point `merge`
will return one child so no reconciliation is needed (remember a
`not_found` is ancestor to all), `needs_repair` will return true since
not all objects are equal, and then a read repair will be performed by
calling `repair`.  Once again, I quoted an excerpt from the shell
demonstrating this.  Notice there is no repair after the second read
because it has already been repaired.

Let's raise the stakes a bit here by taking a node down and then
performing a write before a read.  This should cause the replica
values to conflict.

First bring `rts1` back up and perform read repair to get back to the
initial state.  Then bring it back down :)

    for d in dev/dev*; do $d/bin/rts start; done
    ./dev/dev1/bin/rts attach
    (rts1@127.0.0.1)1> rts:get("progski", "GET").
    ...
    Ctrl^C Ctrl^C

Now attach to `rts2` and confirm one replica reports `not_found`.

    ./dev/dev2/bin/rts attach
    (rts2@127.0.0.1)1> rts:get_dbg_preflist("progski", "GET").
    ...

Increment the `GET` stat, confirm conflicting values, and then perform
a read.  If everything goes well the read should return `20`.

    (rts2@127.0.0.1)3> rts:incr("progski", "GET").
    ok
    (rts2@127.0.0.1)4> rts:get_dbg_preflist("progski", "GET").
    ...
     {{274031556999544297163190906134303066185487351808,
       'rts2@127.0.0.1'},
      {rts_obj,{incr,1,
    ...
    (rts2@127.0.0.1)5> rts:get("progski", "GET").
    20
    (rts2@127.0.0.1)6>
    =ERROR REPORT==== 16-Jun-2011::23:24:07 ===
    repair performed {rts_obj,{incr,20,...
    (rts2@127.0.0.1)6> rts:get_dbg_preflist("progski", "GET").

What if you fail multiple nodes?

    Ctrl^C Ctrl^C
    ./dev/dev3/bin/rts attach
    (rts3@127.0.0.1)1> rts:get_dbg_preflist("progski", "GET").
    ...
     {{274031556999544297163190906134303066185487351808,
       'rts3@127.0.0.1'},
      not_found},
     {{296867520082839655260123481645494988367611297792,
       'rts3@127.0.0.1'},
      not_found}]
    ...
    (rts3@127.0.0.1)2> rts:get("progski", "GET").
    not_found
    (rts3@127.0.0.1)3>
    =ERROR REPORT==== 16-Jun-2011::23:29:48 ===
    repair performed {rts_obj,{incr,20,...
    (rts3@127.0.0.1)3> rts:get("progski", "GET").
    20

You might be surprised that first read returned `not_found`.  After
all, even though two replicas reported `not_found` there is still one
with the correct value.  However, since the default is `R=2` and the
`not_found` values happened to return first this is the value
returned.  In this version of RTS I added an option to pass a
different value of `R` to `get`.  You could have performed the first
read like so and it would return the correct value.

    (rts3@127.0.0.1)4> rts:get("progski", "GET", [{r,3}]).
    20

The key to remember is that multiple node failures, even with writes
occurring, should be no different from a single failure as long as the
number of failures is less than `N`.  Once you lose `N` or more nodes
you **may** start to lose **some** of your data.  This depends on how
many nodes you have in the cluster and what the ring partition
ownership looks like.  Essentially, for data to be lost, its top `N`
partitions need to be owned by failed nodes.

### Partitioned Writes ###

Before I go into partitioned writes I want to say that the closer the
nodes of your cluster are the rarer of an event this is.  For example,
you're much more likely to run into this if your nodes are spread
across data centers as opposed to a rack.  Basho's flagship product,
Riak, is designed to be deployed locally and if you want WAN support
you should use replication.  I'm not sure if the same stance is taken
for applications built on Core.  In any case, the onus is on you, the
developer, to understand your application's needs and this is one of
the things you should take into account.  That is, you should ask
yourself questions like "What's the likelihood that my cluster could
partition?  How often and for how long?  Would such an event have a
deleterious effect on my business or is it not such a big deal?"

A _partition_, in regards to distributed systems, typically refers to
when a cluster breaks apart and forms independent sub-clusters.  This
event is really only dangerous, however, when more than one
sub-cluster can still be reached by the outside world.  Why?  Because
if two or more sub-clusters, or partitions, can receive writes for the
same data but they can't coordinate with each other then you have the
recipe for parallel versions of the same object.  Independently, these
sub-clusters are consistent but once joined conflicts ensue and must
be fixed.  That said, partitions can be more subtle and short lived
than this.  For example, imagine a write request where 2 writes make
it to their destination but the 3rd fails for whatever reason.  This
would cause a logical partition in the replica values until that
replica was repaired.  In fact, if you think hard enough you'll
realize that partitions are **always** occurring in an eventually
consistent system like Riak because many actions are left to be
performed asynchronously and in parallel.  In the case where you
happen to read during a logical partition that's okay because conflict
resolution will occur.  There are also some tricks that you can
perform in these types of systems to garner more predictable
consistency such as making a best effort to serialize all writes to an
object on the same coordinator.  You can find some of these ideas in
Werner Vogel's
[Eventually Consistent - Revisited](http://www.allthingsdistributed.com/2008/12/eventually_consistent.html).

A partition is really no different than a write after a node goes down
in that it causes parallel versions which then causes conflict
resolution to kick in and repair to occur.  I'm just going to continue
the session from above by first restarting `rts1` and `rts2`.  This
time we'll work with the `total_sent` stat to switch it up.

    for d in dev/dev*; do $d/bin/rts start; done
    ./dev/dev1/bin/rts attach
    (rts1@127.0.0.1)1> rts:get("progski", "total_sent").
    ...
    (rts1@127.0.0.1)2> rts:get_dbg_preflist("progski", "total_sent").
    ...

Make sure the value is `95216` for all replicas.  Now let's perform a
partitioned write on the sub-cluster `(rts2, rts3)`.

    (rts1@127.0.0.1)5> rts:dbg_op(incrby, 'rts2@127.0.0.1', ['rts3@127.0.0.1'], "progski", "total_sent", 10000).
    ok
    (rts1@127.0.0.1)6> rts:get_dbg_preflist("progski", "total_sent").
    ...

In the call to `rts:dbg_op` I'm saying that I want to perform a
partitioned write to `rts2` and `rts3` that adds `10000` to the
`total_sent` stat. The 2nd argument is the coordinator node and the
3rd is the other nodes in the pretend sub-cluster.  You should confirm
that two of the replica's now hold the value `105126`.  Performing a
read at this point should cause a read repair.

    (rts1@127.0.0.1)7> rts:get("progski", "total_sent").
    105216
    (rts1@127.0.0.1)8>
    =ERROR REPORT==== 17-Jun-2011::00:39:17 ===
    repair performed {rts_obj,{incr,105216,...

    (rts1@127.0.0.1)8> rts:get_dbg_preflist("progski", "total_sent").
    ...
     {{1233142006497949337234359077604363797834693083136,
       'rts1@127.0.0.1'},
      {rts_obj,{incr,105216,
    ...

As you can see partitions aren't that difficult to reason about.  The
key is to remember that the values stay consistent relative to the
nodes that the coordinator can communicate with.  If 2 nodes split off
to form a sub-cluster then any requests to those 2 nodes will be
consistent in regards to that sub-cluster.  This may be undesirable in
certain circumstances so Riak recently added the query parameters `PW`
and `RW` which say that in order for a write or read to succeed it
must reach a certain number of unique nodes.  For example, given the
sub-cluster of 2 a `PW=3` would fail because only 2 nodes can be
reached.

### Partitioned Writes and Node Down ###

What happens when you combine the two scenarios above?  Similar to
partitioned writes you have the chance for parallel versions.  Similar
to node failure, you have the chance for both parallel versions and
data loss if more than `N` nodes fail.  However, when combined there
is a chance that you can lose data when less than `N` nodes fail.
Think of the case where a sub-cluster of **one** node forms, accepts
some writes, and then fails.  At this point the **only** physical node
that knows about this new data just failed and thus it has been lost.
I'm assuming here, like in the case of RTS, that all storage is in
ephemeral memory.  You could avoid this problem with persistent
storage.  Another approach to avoiding problems like this is to
require that any write must be coordinated by multiple nodes.  That
way a write to a single node will always fail fast and can be retried
by the client, potentially at a different coordinator.

First, I'll demonstrate a partitioned write to a 2-node sub-cluster,
fail one of those nodes, and then perform a read after the partition
has "healed."  Once again, I'll continue from the previous session.

    (rts1@127.0.0.1)12> rts:get("progski", "agents").
    ...
    (rts1@127.0.0.1)14> rts:get_dbg_preflist("progski", "agents").
    ...
    (rts1@127.0.0.1)15> rts:dbg_op(sadd, 'rts2@127.0.0.1', ['rts3@127.0.0.1'], "progski", "agents", "Foo Agent").
    ok
    (rts1@127.0.0.1)16> rts:get_dbg_preflist("progski", "agents").
    ...
    Ctrl^D

At this point a partitioned write has occurred, adding the user agent
"Foo Agent" to the `agents` stat.  Now kill either `rts2` or `rts3`
and then perform a read on one of the remaining nodes.

    ./dev/dev2/bin/rts attach
    Ctrl^C Ctrl^C

    ./dev/dev1/bin/rts attach
    (rts1@127.0.0.1)17> rts:get_dbg_preflist("progski", "agents").
    (rts1@127.0.0.1)18> rts:get("progski", "agents").
    ...
     "Foo Agent",
    ...

Now a write to a sub-cluster of one.

    Ctrl^D
    for d in dev/dev*; do $d/bin/rts start; done
    ./dev/dev1/bin/rts attach
    (rts1@127.0.0.1)19> rts:get("progski", "agents").
    ...
    (rts1@127.0.0.1)20> rts:get_dbg_preflist("progski", "agents").
    ...
    (rts1@127.0.0.1)21> rts:dbg_op(sadd, 'rts1@127.0.0.1', [], "progski", "agents", "Bar Agent").
    ok
    (rts1@127.0.0.1)22> rts:get_dbg_preflist("progski", "agents").
    ...
    Ctrl^C Ctrl^C

    ./dev/dev2/bin/rts attach
    rts:get_dbg_preflist("progski", "agents").
    ...
    (rts2@127.0.0.1)2> rts:get("progski", "agents").
    ...
    (rts2@127.0.0.1)3> rts:get("progski", "agents").
    ...

Notice that "Bar Agent" has been lost.


Hinted Handoff & Conflict Resolution
----------

Conflict resolution is not limited to just read time.  It's also
needed during write time if there is a chance that the objects could
be out of sync.  Think about hinted handoff for a second.  Hinted
handoff occurs when a fallback vnode realizes the primary vnode is
online and its data can be transferred.  However there is a lag
between the time a fallback vnode recognizes the primary is online and
when it starts transferring its data.  During this window writes may
occur on the primary.  If that is the case then the handoff data
cannot simply overwrite the local data or else data would be lost
**locally**.  I emphasize locally because we are talking about a
redundant system and while the data may be lost locally there is a
good chance the data still exist on another vnode and read repair
would reconcile it.  However, it's good to avoid local data loss like
this and avoid relying too heavily on redundancy.

Notice my use of `rts_obj:merge` in the
[rts_stat_vnode](https://github.com/rzezeski/try-try-try/blob/master/2011/riak-core-conflict-resolution/rts/src/rts_stat_vnode.erl)
`handle_handoff_data` callback.  This is invoked when a fallback node
sends a piece of data to the primary.

    handle_handoff_data(Data, #state{stats=Stats0}=State) ->
        {StatName, HObj} = binary_to_term(Data),
        MObj =
            case dict:find(StatName, Stats0) of
                {ok, Obj} -> rts_obj:merge([Obj,HObj]);
                error -> HObj
            end,
        Stats = dict:store(StatName, MObj, Stats0),
        {reply, ok, State#state{stats=Stats}}.

Since this blog post is already grown much too long I'm going to
refrain from showing a console session proving this works.  If you'd
like to try for yourself here are the steps.

1. Take a node down -- this will cause fallback vnodes to be created.

2. Write some data -- this will cause the fallback vnode to be
populated with parallel/conflicting objects relative to the other
vnodes.  It's important that you not perform a `rts:get` or else read
repair will reconcile them.

3. Restart the downed node -- this will cause the primary to come
online with **no** data.

4. Perform a `rts:get` to invoke read repair.  At this point all
primaries have the correct data but you have a fallback that has
conflicting data.  After some time the fallback will realize
the primary is up and will begin handoff.

4. Wait for handoff messages to appear in the console.  Retry the
`rts:get` and make sure the data is still correct and **no** further
read repair was made.  This proves that the data was reconciled
**prior** to writing it.


Next Time
----------

> I made this [letter] very long, because I did not have the leisure
> to make it shorter. ~ Blaise Pascal

I apologize for the length and general cumbersome feel of this blog
post.  It turns out that conflict resolution and the more general
topic of eventual consistency can be quite hard to nail down in a
succinct manner.  This is at least the 5th iteration of this blog post
and I still feel it's wanting in some areas but at some point you have
to say "enough if enough."  Finally, be very critical of everything I
say here.  I'm still an infant in distributed computing and the
purpose of this post is as much to learn as it is to teach.

For my next post I'm thinking of either digging into ways to test
these systems with tools such as QuickCheck or maybe doing an overview
of every module in Riak Core describing its purpose.  If you have an
opinion, on anything here, please ping me on
[twitter](http://twitter.com/#!/rzezeski).
