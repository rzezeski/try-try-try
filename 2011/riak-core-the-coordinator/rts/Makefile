.PHONY: deps rel

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean devclean relclean
	./rebar delete-deps

rel: all
	./rebar generate

relclean:
	rm -rf rel/rts

devrel: dev1 dev2 dev3

devclean:
	rm -rf dev

dev1 dev2 dev3:
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@.config)