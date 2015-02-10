-module(gw_app).
-behaviour(application).
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 8000).

start(_StartType, _StartArgs) ->
    Port = case application:get_env(gateway, port) of
               {ok, P} -> P;
               _ -> ?DEFAULT_PORT
           end,
    gw_sup:start_link(Port).

stop(_State) ->
    ok.
