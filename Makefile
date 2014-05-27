.PHONY: deps test

REBAR?=rebar
DIALYZER?=dialyzer
ERL?=erl

all: deps compile

deps:
	$(REBAR) get-deps

compile: ebin/Elixir.Tinymesh.Config.beam
	$(REBAR) compile

clean:
	$(REBAR) clean

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	public_key eunit syntax_tools compiler

COMBO_PLT = .dialyzer.plt

check_plt: compile
	$(DIALYZER) --check_plt --plt $(COMBO_PLT) --apps $(APPS)
		

build_plt: compile
	$(DIALYZER) --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)

dialyzer: compile
	@$(DIALYZER) --plt $(COMBO_PLT) -Wno_return --src src | \
	    fgrep -v -f ./dialyzer.ignore-warnings


test:
	$(REBAR) skip_deps=true eunit suites=$(suite)

distclean: clean
	$(REBAR) delete-deps

ebin/Elixir.Tinymesh.Config.beam: priv/config priv/generate-erl-config.exs
	priv/generate-erl-config.exs priv/config
