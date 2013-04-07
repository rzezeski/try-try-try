Riak Core, First Multinode
==========

Questions about Riak Core seem to be occurring with more frequency on the riak-users mailing list.  Recently [jnewlend](https://github.com/jnewland) threw up a [Riak Core template](https://github.com/websterclay/rebar_riak_core) to be used with [Rebar](https://github.com/basho/rebar).  I thought this was excellent work and ran with it.

While I liked what Mr. Newland had started I felt it could use a little more meat on its bones.  He created a template to create a Riak Core application but left it as an exercise to the user to do the rest such as build a "multinode" capable release.  With knowledge of Erlang, Rebar, and peeking at the Riak source code this is certainly doable but it's a real PITA when you just want to get something up and running.  I've started [my own fork](https://github.com/rzezeski/rebar_riak_core/tree/multinode) that allows one to standup a _multinode_ release fairly easily.

Um, Ryan...What The Hell is Multinode?
----------

So I kind of just threw the term _multinode_ at you like it's colloquial language or something; let me try to explain.  In general, when someone says _node_ I generally think of a physical machine...you know a big black box with sharp corners, loud fans, and maybe a blue neon light if you built it yourself, or maybe it's a sleek, quiet, beautiful piece of solid aluminum if you bought it from a company named after a piece of fruit (no laughing, I own both).  I digress.  Back to multinode.

In Erlang, a node refers to an instance of an Erlang Virtual Machine.  You can have multiple Virtual Machines running at once on the same machine just like you can run more than one instance of a JVM on your local machine.  Now, if you've read anything about Erlang you've probably heard it's really great at this whole "distributed" thing.  No?  Well go RTFM and come back, please.  In fact, Erlang has a _distributed_ mode, which is to say that it is started in such a way that it can be joined with other nodes on the same physical machine or on that abused mac mini you have sitting in the corner (if you're counting that's three computers I own...not enough).

Like some famous person said "one is the loneliest number" so Erlang nodes tend to form gangs.  Not the gangs like in West Side Story where they walk in-step snapping their fingers, but still cool nonetheless.  When more than one node makes up a cluster I call it a _multinode_.  That's all I mean.  I guess it might have been easier to just call it a "cluster," but multinode sounds way cooler; and besides I'm too lazy to change it now.

TLDR, Start Here
----------

Enough of me attempting to be witty (is it possible to be witty in written form?).  The first thing you need to do is grab the templates and drop them in `~/.rebar/templates`.

    git clone git://github.com/rzezeski/rebar_riak_core.git
    mkdir -p ~/.rebar/templates
    cp rebar_riak_core/* ~/.rebar/templates
    ls ~/.rebar/templates

Without executing the above steps rebar won't be able to find the _riak\_core\_multinode_ template (can I configure rebar to look in other dirs for templates?).  If you don't have rebar don't worry, we'll get to that.  Next, make a directory to house your new multinode capable Riak Core application.

    mkdir mfmn
    cd mfmn

The mfmn stands for "My First MultiNode".  Original, I know.  Next, you need rebar.

    wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar

Great, now create your new multinode app.

    ./rebar create template=riak_core_multinode appid=mfmn nodeid=mfmn

Here is an excerpt of what the output should look like:

    ==> mfmn (create)
    Writing rel/reltool.config
    Writing rel/files/erl
    Writing rel/files/nodetool
    Writing rel/files/mfmn
    ...

Let me break that last command down a bit:

* `create`: This is a rebar _command_, and it tells rebar that you want to create a skeleton from the template and vars which you pass as argument.

* `template`: The name of the template used to build the skeleton.

* `appid` & `nodeid`: These are a little harder to explain without going into Erlang OTP, applications and releases.  The multinode template not only assumes you want to build a local application (and by local I mean in the same repo) but also that you want to build a release around this application.  If you have no clue what that means then just ignore this for now and specify the same value for both.

Congrats, you have the start of a Riak Core application that can be deployed to multiple nodes and joined together to form a _multinode_ cluster.  Lets start 'er up.

    make rel
    ./rel/mfmn/bin/mfmn console

At this point you have a single node of _mfmn_ running, now for the moment you've been waiting for!

    mfmn:ping().

You should see something like `{pong,365375409332725729550921208179070754913983135744}` returned but the really large number probably won't be the same.  Try running that command a few times back to back.  Notice how the number changes each time?  That's a _partition_ in Riak Core parlance, and the fact that it's changing means your ping request is getting distributed across the various _vnodes_ in the one node cluster.  "Wait..wha?  What the hell is a vnode Ryan?"  Glad you asked.

This example has pretty much sidestepped explaining anything about Riak Core.  Yes, when you call `mfmn:ping()` you are using a Riak Core based application but that doesn't tell you much.  While trying to write a post that explains Riak Core in more depth I realized that it might be easier if I could establish some sort of common ground.  I created this Rebar template for that reason.  In my next post I plan to discuss the idea of a _vnode_ and how to implement one.  In the meantime, I'll discuss some more things you can do with the toy project you just created.

devrel
----------

Above I showed you how to start a single node with the console at the foreground, but this isn't typically how other Riak Core based applications like Riak are tested.  Instead, there is something called a _devrel_ that allows one to easily stand up a local 3-node cluster.  Lucky for you I included this in the multinode template.

    make devrel

This command is very similar to `rel` but instead creates 3 separate instances under the `dev/` dir; check it out.

    ls dev

Now, lets start all the nodes.

    for d in dev/dev*; do $d/bin/mfmn start; done

There's no output so let's make sure they are indeed up.

    for d in dev/dev*; do $d/bin/mfmn ping; done

You should see three `pong` replies.  Now, at this point, it is worth saying that you have three **INDIVIDUAL** mfmn nodes running.  They are **NOT** aware of each other yet and if this were a Riak KV cluster you could store data in one node and the other node will have no idea it's there.  In order to form the cluster you have to _join_ the nodes.  Don't worry, you only have to join them once.  If a node, or the entire cluster, goes down it will remember the other nodes it's joined to.

    for d in dev/dev{2,3}; do $d/bin/mfmn-admin join mfmn1@127.0.0.1; done

Finally, to make sure they really all agree on the shape of the cluster you can ask if the _ring_ is "ready."

To verify you have a 3 node cluster you can run the `member_status` command.

    ./dev/dev1/bin/mfmn-admin member_status

Now you can attach to the shell of one of the nodes and run the `ping` command.

    ./dev/dev2/bin/mfmn attach
    mfmn:ping().

To stop all the nodes just transpose `start` for `stop`.

    for d in dev/dev*; do $d/bin/mfmn stop; done

This is a "Working Blog"
----------

It's my intention that this will be a _working_ blog.  That means that I will strive to place the emphasis on working code over prose.  If you look you'll see a `mfmn` dir that contains everything needed for turnkey operation.  That is, you should be able to go in there and just type `make` to get things going.  This means you can skip the rigmarole of downloading the templates, copying them, downloading rebar, etc.  It also means you can eff up your local copy without worry cause there will always be a pristine one here; as long as I never delete my repo :)
