-module(cluster_server).
-behaviour(gen_server).
-export([start_link/1, online_nodes/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(CHECKPOINT, 3000).
-define(ONLINE_NODES, online_nodes).

start_link(Type) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Type, []).

online_nodes() ->
    gen_server:call(?SERVER, online_nodes).

init(Type) ->
    case {Type, eredis:start_link()} of
        {server, {ok, R}} ->
            register_server(R);
        {gateway, {ok, R}} ->
            register_gateway(R)
    end.

handle_call(online_nodes, _From, R) ->
    Reply = get_online_nodes(R),
    {reply, Reply, R}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(timeout, R) ->
    case get_online_nodes(R) of
        {ok, Nodes} ->
            Deads = checkpoint(Nodes),
            cleanup(R, Deads),
            {noreply, R, ?CHECKPOINT};
        Error ->
            error_logger:error_report([{online_nodes_checkpoint, Error}]),
            {noreply, R, ?CHECKPOINT}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

register_server(R) ->
    {ok, _Res} = eredis:q(R, ['SADD', ?ONLINE_NODES, node()]),
    {ok, R}.

register_gateway(R) ->
    {ok, R, ?CHECKPOINT}.

get_online_nodes(R) ->
    eredis:q(R, ['SMEMBERS', ?ONLINE_NODES]).

checkpoint(Nodes) ->
    checkpoint(Nodes, []).

checkpoint([], Acc) ->
    Acc;
checkpoint([Node|Rest], Acc) ->
    case net_adm:ping(node2atom(Node)) of
        pong ->
            checkpoint(Rest, Acc);
        Error ->
            error_logger:error_report([{ping, Error, Node}]),
            checkpoint(Rest, [Node|Acc])
    end.

node2atom(Node) ->
    list_to_atom(binary_to_list(Node)).

cleanup(_R, []) ->
    ok;
cleanup(R, [Node|Res]) ->
    case eredis:q(R, ['SREM', ?ONLINE_NODES, Node]) of
        {ok, _} ->
            cleanup(R, Res);
        Error ->
            error_logger:error_report([{cleanup, Error}]),
            cleanup(R, Res)
    end.
