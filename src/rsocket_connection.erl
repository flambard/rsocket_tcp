-module(rsocket_connection).
-behaviour(gen_statem).

%% API
-export([
         start_link/2,
         recv_frame/2,
         close/1
        ]).

%% gen_statem callbacks
-export([
         callback_mode/0,
         init/1,
         terminate/3,
         code_change/4
        ]).

%% gen_statem states
-export([
         state_name/3
        ]).


-record(data,
        {
         transport_pid,
         transport_mod
        }).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Module :: atom(), Transport :: pid()) ->
          {ok, Pid :: pid()} |
          ignore |
          {error, Error :: term()}.
start_link(Module, Transport) ->
    gen_statem:start_link(?MODULE, [Module, Transport], []).

recv_frame(Server, Frame) ->
    gen_statem:cast(Server, {recv, Frame}).

close(Server) ->
    gen_statem:cast(Server, close_connection).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.


-spec init(Args :: term()) ->
          gen_statem:init_result(atom()).
init([Module, Transport]) ->
    Data = #data{
              transport_mod = Module,
              transport_pid = Transport
             },
    {ok, state_name, Data}.


-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
          any().
terminate(_Reason, _State, _Data) ->
    void.


-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
          {ok, NewState :: term(), NewData :: term()} |
          (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%%%===================================================================
%%% States
%%%===================================================================

-spec state_name('enter',
                 OldState :: atom(),
                 Data :: term()) ->
          gen_statem:state_enter_result('state_name');
                (gen_statem:event_type(),
                 Msg :: term(),
                 Data :: term()) ->
          gen_statem:event_handler_result(atom()).
state_name(cast, close_connection, Data) ->
    #data{ transport_pid = Pid, transport_mod = Mod } = Data,
    Mod:close_connection(Pid),
    {stop, disconnect};

state_name(cast, {recv, _Frame}, Data) ->
    %% TODO: Parse the received frame and act upon it
    {next_state, state_name, Data};

state_name({call, Caller}, _Msg, Data) ->
    {next_state, state_name, Data, [{reply, Caller, ok}]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
