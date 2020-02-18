-module(rsocket_tcp).

%% API
-export([
         connect/2
        ]).


%%%===================================================================
%%% API
%%%===================================================================

connect(Address, Port) ->
    {ok, Pid} = rsocket_tcp_connection_sup:initiate_connection(Address, Port),
    {ok, #{pid => Pid, module => rsocket_tcp_connection}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
