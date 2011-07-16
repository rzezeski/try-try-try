Run Benchmarks
==========

In keeping tradition with my "working blog" title I've included these
instructions to enable you to run the benchmarks on your own hardware.

### Prereqs ###

Since I already assumed familiarity with Search I'm going to assume
you already have it installed or you know how to build a devrel from
scratch.  Remember, Search is integrated with Riak now.

You'll need the Python [client] [pc] to load the data.

You'll need [basho bench] [bb] to run the benchmarks.

Finally, you'll need a local copy of the Riak Search [code] [rsc]
because it contains the bench driver needed to run the benchmarks.

Then you should edit the Makefile and .config files to correspond to
your environment.

* `BB` - Path to Basho Bench

* `RIAK_PATH` - That path to your Riak "install."  If you used devrel
  then point it to `<riak path>/dev/dev1`.  The key is that the `bin`
  dir exist in this directory.

* `PBC_PORT` - The Protocol Buffers port of any of your nodes.  If
  using devrel then set it to `8081`.

For example...

    BB ?= /Users/rzezeski/work/basho_bench
    RIAK_PATH ?= /Users/rzezeski/work/riak/dev/dev1
    PBC_PORT ?= 8081

And in the .config files...

    {source_dir, "/Users/rzezeski/work/riak_search/src"}.

### Load Corpus ###

To load the data run the following.  Keep in mind this will take
several minutes to load and you will see a few errors that you can
safely ignore.  It would be a good time to step away for a beer, a
smoke, or an apple if you listened to your mother as a child.

    make init


### Run the Benchmarks ###

A total of 4 runs will occur.  The first to prime the system and then
the three queries.  Each run will take 5 minutes to finish.

    make run


### Generate the Pretty Graphs ###

If you want to see pretty graphs run the following command.  It should
create PNG images for each run.

    make results


[pc]: https://github.com/basho/riak-python-client

[bb]: http://wiki.basho.com/Benchmarking-with-Basho-Bench.html

[rsc]: https://github.com/basho/riak_search
