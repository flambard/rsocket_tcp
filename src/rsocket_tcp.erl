-module(rsocket_tcp).

%% API
-export([
         start_listening/1,
         connect/2
        ]).


%%%===================================================================
%%% API
%%%===================================================================

start_listening(Port) ->
    {ok, _} = rsocket_tcp_acceptor_sup_sup:start_tcp_acceptor_supervisor(Port),
    ok.

connect(Address, Port) ->
    case rsocket_tcp_connection_sup:initiate_connection(Address, Port) of
        {error, Reason}                         -> {error, Reason};
        {ok, _TCPConnection, RsocketConnection} -> {ok, RsocketConnection}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
