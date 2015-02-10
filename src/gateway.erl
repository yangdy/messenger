-module(gateway).
-export([start/0, stop/0]).

start() ->
    application:start(sasl),
    application:start(gateway).

stop() ->
    application:stop(gateway).
