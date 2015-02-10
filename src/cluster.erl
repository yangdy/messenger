-module(cluster).
-export([start/2, online_nodes/0]).

start(Type, Sup) ->
    supervisor:start_child(Sup, {cluster_server,
                                 {cluster_server, start_link, [Type]},
                                 permanent, 5000, worker, [cluster_server]}).

online_nodes() ->
    cluster_server:online_nodes().
