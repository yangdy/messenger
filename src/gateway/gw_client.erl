-module(gw_client).
-behaviour(gen_fsm).
-export([start_link/1]).
-export([init/1,
         handle_sync_event/4,
         handle_event/3,
         handle_info/3,
         terminate/3,
         code_change/4]).

start_link(Socket) ->
    gen_fsm:start_link(?MODULE, Socket, []).

init(Socket) ->
    {ok, gw_connected, Socket}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_info({tcp_closed, _Socket}, StateName, State) ->
    apply(StateName, event, [tcp_closed, State]),
    {stop, normal, State};
handle_info({tcp, Socket, Data}, StateName, State) ->
    case apply(StateName, event, [Data, State]) of
        {reply, Reply, NextState, NewState} ->
            socket_send(Socket, Reply),
            {next_state, NextState, NewState};
        {noreply, NextState, NewState} ->
            {next_state, NextState, NewState};
        {stop, normal, State} ->
            {stop, normal, State};
        Error ->
            error_logger:error_report([Error]),
            {next_state, StateName, State}
    end.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

socket_send(Socket, Reply) ->
    gen_tcp:send(Socket, Reply).
