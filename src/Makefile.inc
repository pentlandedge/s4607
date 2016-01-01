#######################################################################
# Generic make script for compiling erlang code                       #
# Compiles the code into a ebin dir. relative to the source dir.      #
# (../ebin)                                                           #
####################################################################### 
 
EBIN = ../ebin
ERL = erl
GEN = beam
ERLC_EMULATOR = erl -boot start_clean
TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

CODE = $(SOURCE:%.erl=$(EBIN)/%.beam) 

$(EBIN)/%.beam: %.erl
	erlc  -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<

all: $(TARGETS)
   
clean:
	\rm -f $(CODE) 

