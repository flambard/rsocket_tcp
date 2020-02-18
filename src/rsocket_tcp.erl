-module(rsocket_tcp).

%% API
-export([
         connect/2
        ]).


%%%===================================================================
%%% API
%%%===================================================================

connect(Address, Port) ->
    {ok, _TCPConnection, RsocketConnection} =
        rsocket_tcp_connection_sup:initiate_connection(Address, Port),
    {ok, RsocketConnection}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
