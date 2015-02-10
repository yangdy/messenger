-module(gw_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(BACKLOG, 10).
-define(SOCKOPTS, [binary,
                   {active, true},
                   {reuseaddr, true},
                   {packet, 0},
                   {nodelay, true}]).

-record(state, {listen,
                connections=0}).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Port, []).

init(Port) ->
    case gen_tcp:listen(Port, ?SOCKOPTS) of
        {ok, LSocket} ->
            spawn_listener_pool(self(), LSocket),
            {ok, #state{listen=LSocket}};
        Error ->
            {stop, Error}
    end.

handle_call({connect, Socket}, _From, #state{connections = N} = State) ->
    Reply = gw_client_sup:start_client(Socket),
    {reply, Reply, State#state{connections = N +1}};
handle_call(get_connections, _From, #state{connections = N} = State) ->
    {reply, N, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

spawn_listener_pool(From, LSocket) ->
    spawn_listener_pool(From, LSocket, ?BACKLOG).

spawn_listener_pool(_From, _LSocket, 0) ->
    ok;
spawn_listener_pool(From, LSocket, N) ->
    spawn_link(fun() -> gw_listener_proc(From, LSocket) end),
    spawn_listener_pool(From, LSocket, N-1).

gw_listener_proc(From, LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            case gen_server:call(From, {connect, Socket}) of
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid),
                    gw_listener_proc(From, LSocket);
                Error ->
                    error_logger:error_report({gw_listener_proc, Error}),
                    gw_listener_proc(From, LSocket)
            end;
        Error ->
            error_logger:error_report([{gw_listener_proc, Error}]),
            gw_listener_proc(From, LSocket)
    end.
