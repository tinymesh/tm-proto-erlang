.PHONY: deps test

REBAR?=rebar
DIALYZER?=dialyzer
ERL?=erl

all: deps compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	public_key eunit syntax_tools compiler

COMBO_PLT = $(HOME)/.erl/colonel_combo_dialyzer_plt

check_plt: compile
	$(DIALYZER) --check_plt --plt $(COMBO_PLT) --apps $(APPS)
		

build_plt: compile
	$(DIALYZER) --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)

dialyzer: compile
	@$(DIALYZER) --plt $(COMBO_PLT) -Wno_return --src src 


test:
	$(REBAR) skip_deps=true eunit suite=$(suite)

distclean: clean
	$(REBAR) delete-deps
