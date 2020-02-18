-module(rsocket_tcp).

%% API
-export([
         connect/2
        ]).


%%%===================================================================
%%% API
%%%===================================================================

connect(Address, Port) ->
    case rsocket_tcp_connection_sup:initiate_connection(Address, Port) of
        {error, Reason}                         -> {error, Reason};
        {ok, _TCPConnection, RsocketConnection} -> {ok, RsocketConnection}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
