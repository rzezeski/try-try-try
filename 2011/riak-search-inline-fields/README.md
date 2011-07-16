Riak Search, Inline Fields
==========

In this post I want to give a quick overview of _inline fields_; a
recent addition to Riak Search [1] that allows you to tradeoff disk
space for a considerable performance bump in query execution and
throughput.  I'm going to assume the reader is already familair with
Search.  In the future I may do a Search overview.  If you would like
that then ping me on [twitter](http://twitter.com/#!/rzezeski).


The Goal
----------

Recently on the Riak Users mailing list there has been discussion
about improving the performance of Search when executing intersection
(i.e. `AND`) queries where one term has a low frequency and the other
has a high frequency.  This can pose a problem because Search needs to
run through all the results on both sides in order to provide the
correct result.  Therefore, the query is always bounded by the highest
frequency term.  This is exasperated further by the fact that Search
uses a _global index_, or in other words partitions the index by term.
This effectively means that all results for a particular term are
pulled sequentially from **one** node.  This is opposed to a _local
index_, or partitioning by document, which effectively allows you to
parallelize the query across all nodes.  There are tradeoffs for
either method and I don't want to discuss that in this blog post.
However, it's good to keep in mind [2].  My goal with this post is to
show how you can improve the performance of this type of query with
the current version of Search [3].


What's an "Inline" Field, Anyways?
----------

To properly understand inline fields you need to understand the
_inverted index_ data structure [4].  As a quick refresher the gist is
that the index is a map from words to a list of document
reference/weight pairs.  For each word [5] the index tells you in
which documents it occurs and it's "weight" in relation to that
document, e.g. how many times it occurs.  Search adds a little twist
to this data structure by allowing an arbitrary list of properties to
be tacked on to each of these pairs.  For example, Search tracks the
position of each occurance of a term in a document.

Inline fields allow you to take advantage [6] of this fact and store
the terms of a field directly in the inverted index entries [7].
Going back to my hypothetical query you could mark the field with the
frequently occuring term as `inline` and change the `AND` query to a
query and a _filter_.  A filter is simply an extra argument to the
Search API that uses the same syntax as a regular query but makes use
of the inline field.  This has the potential to drop your latency
dramatically as you avoid pulling all the massive _posting_ [7]
altogether.

**WARNING: Inline fields are not free!  Think carefully about what I
 just described and you'll realize that this list of inline terms will
 be added to every single posting for that index.  If your field
 contains many terms or you have many inline fields this could become
 costly in terms of disk space.  As always, benchmarking with real
 hardware on a real production data set is recommended.**


The Corpus
----------

I'll be using a set of ~63K tweets [8] that occured in reaction to the
the devestating earthquake that took place in Haiti during January of
2010.  The reason I choose this dataset is because it's guarenteed to
have frequently occuring terms such as "earhquake" but also has low
occuring terms [9] such as the time the tweets were created.


The Rig
----------

I have performed a crude benchmark of three different queries.  All
benchmarks were run on a 2GHz i7 MBP with an SSD [10].  An initial run
is performed to prime all systems.  Essentially, everything should be
coming from FS cache meaning I'll mostly be testing processing time.
My guess is disk I/O would only amplify the results.  I'll be using
Basho Bench and running it on the same machine as my cluster.  My
cluster consists of a four Riak nodes (obviously, on the same machine)
which I built from master [11].


Naive Query
----------

    "text:earthquake"

The naive query asks for every document id [12] that includes the word
`earthquake`.  This should return `62805` results every time.

![Naive](https://github.com/rzezeski/try-try-try/raw/riak-search-inline-fields/2011/riak-search-inline-fields/results/run-with-inline/naive.png)


Scoped Query
----------

    "text:earthquake AND created_at:[20100113T032200 TO 20100113T032500]"

The scoped query still searches for all documents with the term
`earthquake` but restricts this set further to only those that were
created in the provided three minute time span.

![Scoped](https://github.com/rzezeski/try-try-try/raw/riak-search-inline-fields/2011/riak-search-inline-fields/results/run-with-inline/scoped.png)


Scoped Query With Filtering
----------

    "created_at:[20100113T032200 TO 20100113T032500]" "text:earthquake"

This is the same as the scoped query except `earthquake` is now a
filter, not a query.  Notice, unline the previous two queries, there
are two strings.  The first is the query the second is the filter.
You could read that in English as:

> Execute the query to find all tweets created in this three minute
> range.  Then filter that set using the inline field "text" where it
> contains the term "earthquake."

![Scoped & Filter](https://github.com/rzezeski/try-try-try/raw/riak-search-inline-fields/2011/riak-search-inline-fields/results/run-with-inline/scoped-filter.png)


Conclusions
----------


Run The Benchmark Yourself
----------

In keeping tradition with my "working blog" title I'll show you how to
run these benchmarks on your on hardware.

### Prereqs ###

Since I already assumed familiarity with Search I'm going to assume
you already have it installed or you know how to build a devrel from
scratch.  Remember, Search is integrated with Riak now.

You'll need the Python client to load the data [13].

You'll need basho bench [14].

Finally, you'll need a local copy of the Riak Search code because it
contains the bench driver needed to run the benchmarks.

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


TODO: To be fair I should benchmark query 1 & 2 with the inline field
turned off to avoid overhead of all that extra data.

[1]: http://wiki.basho.com/Riak-Search---Schema.html#Fields-and-Field-Level-Properties

[2]: If you'd like to know more you could start by reading Distributed
Query Processing Using Partitioned Inverted Files.

[3]: Inline fields were added in 14.2, but my benchmarks were run
against master.

[4]: I like the introduction in Effect of Inverted Index Partitioning
Schemes on Performance of Query Processing in Parallel Text Retrieval
Systems.

[5]: In search parlence a word is called a _term_ and the entire list
of terms is called the _vocabulary_.

[6]: Or abuse, depending on your disposition.

[7]: Entries in an inverted index are also called _postings_ by some people.  I know, enough with the fucking terminology already.  Shesh!

[8]: http://www.infochimps.com/datasets/twitter-haiti-earthquake-data

[9]: Or high cardinality, depending on how you want to look at it.

[10]: Just like when dynoing a car it's constant conditions and
relative improvement that matter.  Once you're outta the shop those
absolute numbers don't mean shit.

[11]: The exact commit is `3cd22741bed9b198dc52e4ddda43579266a85017`.

[12]: BTW, in this case "document" is a Riak object indexed by the
Search precommit hook.

[13]: https://github.com/basho/riak-python-client

[14]: http://wiki.basho.com/Benchmarking-with-Basho-Bench.html
