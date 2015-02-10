-module(gw_connected).
-include("../gw_client.hrl").
-export([event/2]).

event(tcp_closed, State) ->
    {stop, normal, State};
event(<<"quit", _/binary>>, State) ->
    {stop, normal, State};
event(Data, State) ->
    {reply, Data, connected, State}.
