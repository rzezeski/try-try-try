BB ?= /Users/rzezeski/work/basho_bench
RIAK_PATH ?= /Users/rzezeski/work/riak/dev/dev1
PBC_PORT ?= 8081

init:
	./init.sh $(RIAK_PATH) $(PBC_PORT)

# First prime system by running naive, then measure.
run:
	$(BB)/basho_bench naive.config

	$(BB)/basho_bench naive.config
	cp -r tests/current tests/naive

	$(BB)/basho_bench scoped.config
	cp -r tests/current tests/scoped

	$(BB)/basho_bench scoped-filter.config
	cp -r tests/current tests/scoped-filter

run_bench:
	$(BB)/basho_bench $(CONFIG)

# Build the pretty graphs, you need R installed
results:
	$(BB)/priv/summary.r -i tests/naive
	cp tests/naive/summary.png tests/naive/naive.png

	$(BB)/priv/summary.r -i tests/scoped
	cp tests/scoped/summary.png tests/scoped/scoped.png

	$(BB)/priv/summary.r -i tests/scoped-filter
	cp tests/scoped-filter/summary.png tests/scoped-filter/scoped-filter.png