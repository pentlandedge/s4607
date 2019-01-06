all: compile test doc dialyzer

compile:
	rebar3 compile

test:
	rebar3 eunit 

doc:
	rebar3 edoc 

dialyzer:
	rebar3 dialyzer 
    
clean:
	rebar3 clean

.PHONY: all compile test doc dialyzer clean

