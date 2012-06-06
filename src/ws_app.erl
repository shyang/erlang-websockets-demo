-module(ws_app).

-behaviour(application).

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
    io:format("start ~p~n", [application:start(ws)]).

stop() ->
    application:stop(ws_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    ws_sup:start_link().

stop(_State) ->
    ok.
