-module(gw_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, Port),
    cluster:start(gateway, ?SERVER),
    {ok, Pid}.

init(Port) ->
    RestartStrategy = {one_for_one, 10, 10},
    Server = {gw_server, {gw_server, start_link, [Port]},
              permanent, 5000, worker, [gw_server]},
    ClientSup = {gw_client_sup, {gw_client_sup, start_link, []},
                 permanent, 5000, supervisor, [gw_client_sup]},
    {ok, {RestartStrategy, [Server, ClientSup]}}.
