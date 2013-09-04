BASE_DIR := $(shell pwd)
REBAR_REPO := https://github.com/rebar/rebar.git
REBAR ?= $(BASE_DIR)/rebar
DEPS ?= deps

DIALYZER := dialyzer
DIALYZER_APPS := kernel stdlib sasl inets crypto public_key ssl

BASIC_PLT := basic.plt

.PHONY: all app doc deps clean test ct xref docs lock-deps

all: app

app: $(REBAR) deps
	@$(REBAR) compile

deps: $(REBAR) 
	@$(REBAR) get-deps

clean: $(REBAR)
	@$(REBAR) clean

test: $(REBAR)
	@$(REBAR) compile -D TEST
	@$(REBAR) eunit skip_deps=true $(if $(SUITES),suites=$(SUITES),)

ct: $(REBAR) app
	@$(REBAR) ct skip_deps=true

$(BASIC_PLT): build-plt

build-plt: 
	@$(DIALYZER) --build_plt --output_plt $(BASIC_PLT) --apps $(DIALYZER_APPS)

dialyze: $(BASIC_PLT)
	@$(DIALYZER) -r src deps/*/src --no_native --src --plt $(BASIC_PLT) -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

xref: $(REBAR) clean app
	@$(REBAR) xref skip_deps=true

doc: $(REBAR) app
	@$(REBAR) doc skip_deps=true

lock-deps: $(REBAR) app
	@$(REBAR) lock-deps ignore=meck,proper,rebar

$(DEPS)/rebar:
	@mkdir -p $(DEPS)
	git clone $(REBAR_REPO) $(DEPS)/rebar
 
$(REBAR): $(DEPS)/rebar
	$(MAKE) -C $(DEPS)/rebar
	cp $(DEPS)/rebar/rebar .
 
rebar: $(REBAR)
