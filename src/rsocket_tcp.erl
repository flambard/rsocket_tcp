-module(rsocket_tcp).

%% API
-export([
         start_listening/2,
         connect/3
        ]).


%%%===================================================================
%%% API
%%%===================================================================

start_listening(Port, RSocketHandlers) ->
    {ok, _} = rsocket_tcp_acceptor_sup_sup:start_tcp_acceptor_supervisor(
                Port, RSocketHandlers),
    ok.

connect(Address, Port, RSocketHandlers) ->
    ConnectionResult = rsocket_tcp_connection_sup:initiate_connection(
                         Address, Port, RSocketHandlers),
    case ConnectionResult of
        {error, Reason}                         -> {error, Reason};
        {ok, _TCPConnection, RsocketConnection} -> {ok, RsocketConnection}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
