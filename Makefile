.PHONY: deps

all:
	@./rebar compile

clean:
	@./rebar clean

run:
	erl -pa ebin -noshell -s ws_app

