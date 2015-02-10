-module(gw_client_sup).
-behaviour(supervisor).
-export([start_link/0, start_client/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_client(Socket) ->
    supervisor:start_child(?SERVER, [Socket]).

init([]) ->
    RestartStrategy = {simple_one_for_one, 10, 10},
    Client = {gw_client, {gw_client, start_link, []},
              temporary, 5000, worker, [gw_client]},
    {ok, {RestartStrategy, [Client]}}.
