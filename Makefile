all: 
	rebar compile eunit doc dialyze

clean:
	rebar clean

.PHONY: all clean

