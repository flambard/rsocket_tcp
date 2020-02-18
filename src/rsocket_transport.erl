-module(rsocket_transport).

%%%
%%% rsocket_transport is the API module for RSocket transport implementations
%%%
%%
%% A transport implementation must:
%% 1. ...
%%

%% API
-export([
         start_connection/1,
         recv_frame/2
        ]).


-callback send_frame(Transport :: term(), Frame :: binary()) -> ok.
-callback close_connection(Transport :: term()) -> ok.


%%%===================================================================
%%% API
%%%===================================================================

-spec start_connection(Transport :: term()) -> {ok, Connection :: term()}.
start_connection(_Transport) ->
    %% TODO: rsocket_connection:start(Transport).
    {ok, pid}.

-spec recv_frame(RSocket :: term(), Frame :: binary()) -> ok.
recv_frame(_RSocket, _Frame) ->
    %% TODO: rsocket_connection:recv_frame(RSocket, Frame).
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
