ERLC = erlc
ERL = erl
EBIN_DIR   = ebin
EBIN_DIRS  = $(EBIN_DIR)
ERL_FILES  = $(wildcard *.erl)
BEAM_FILES = $(subst .erl,.beam,$(ERL_FILES))

HALT = -s erlang halt

## Create needed folders (if not exist):
$(shell [ -d "$(EBIN_DIR)/" ] || mkdir $(EBIN_DIR)/)

.PHONY: all

all: $(BEAM_FILES)
	@(cd src && make EBIN_DIR=../$(EBIN_DIR) ERLC=$(ERLC) ERL_COMPILE_FLAGS="$(ERL_COMPILE_FLAGS)" $@)

%.beam: %.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o $(EBIN_DIR) $<

clean:
	@(cd src && make EBIN_DIR=../$(EBIN_DIR) ERLC=$(ERLC) ERL_COMPILE_FLAGS="$(ERL_COMPILE_FLAGS)" $@)

dialyzer:
	../../thesis_source_code/otp/bin/dialyzer --src -r .

open_erl:
	$(ERL) -pa $(EBIN_DIRS)

test:
	$(ERL) -pa $(EBIN_DIRS) -noshell -run app main $(args) $(HALT)
