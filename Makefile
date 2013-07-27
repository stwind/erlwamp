BASE_DIR = $(shell pwd)
REBAR := $(BASE_DIR)/rebar
DIALYZER := dialyzer
DIALYZER_APPS := kernel stdlib sasl inets crypto public_key ssl
DEPS := $(CURDIR)/deps
APP := wamp

BASIC_PLT := $(APP).plt

.PHONY: all deps clean test ct xref docs lock-deps

all: app

app: $(REBAR) deps
	@$(REBAR) compile

deps: $(REBAR) 
	@$(REBAR) get-deps

clean: $(REBAR)
	@$(REBAR) clean

ifndef SUITES
EUNIT_SUITES =
else
EUNIT_SUITES = suites=$(SUITES)
endif
test: $(REBAR) deps
	@$(REBAR) compile -D TEST
	@$(REBAR) eunit skip_deps=true $(EUNIT_SUITES)

ct: $(REBAR) deps
	@CT_COMPILE=true $(REBAR) compile -D TEST
	@$(REBAR) ct skip_deps=true -v

$(BASIC_PLT): build-plt

build-plt: 
	@$(DIALYZER) --build_plt --output_plt $(BASIC_PLT) --apps $(DIALYZER_APPS)

dialyze: $(BASIC_PLT)
	@$(DIALYZER) -r src deps/*/src --no_native --src --plt $(BASIC_PLT) -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

xref: $(REBAR) clean app
	@$(REBAR) xref skip_deps=true

docs: $(REBAR)
	@$(REBAR) doc skip_deps=true

lock-deps: $(REBAR) app
	@$(REBAR) lock-deps ignore=meck,proper,rebar

$(DEPS)/rebar:
	@mkdir -p $(DEPS)
	git clone https://github.com/rebar/rebar.git $(DEPS)/rebar

$(REBAR): $(DEPS)/rebar
	$(MAKE) -C $(DEPS)/rebar
	cp $(DEPS)/rebar/rebar .
