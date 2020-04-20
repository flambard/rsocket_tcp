-module(rsocket_tcp_connection).
-behaviour(rsocket_transport).
-behaviour(gen_server).

%% API
-export([
         start_link/2,
         start_link/3,
         activate_socket/1,
         start_listening_socket/1
        ]).

%% rsocket_transport callbacks
-export([
         send_frame/2,
         close_connection/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2
        ]).

-record(state,
        {
         rsocket,
         tcp_socket
        }).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, RSocketHandlers) ->
    gen_server:start_link(?MODULE, {accept, Socket, RSocketHandlers}, []).

start_link(Address, Port, RSocketHandlers) ->
    {ok, Pid} = gen_server:start_link(
                  ?MODULE, {initiate, Address, Port, RSocketHandlers}, []),
    RSocketConnection = gen_server:call(Pid, get_rsocket),
    {ok, Pid, RSocketConnection}.

activate_socket(Server) ->
    ok = gen_server:cast(Server, activate_socket).

start_listening_socket(Port) ->
    SocketOptions = [binary, {active, false}, {reuseaddr, true}],
    gen_tcp:listen(Port, SocketOptions).


%%%===================================================================
%%% rsocket_transport callbacks
%%%===================================================================

-spec send_frame(Server :: pid(), Frame :: binary()) -> ok.
send_frame(Server, Frame) ->
    gen_server:cast(Server, {send, Frame}).


-spec close_connection(Server :: pid()) -> ok.
close_connection(Server) ->
    gen_server:stop(Server, disconnect, 500).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init({accept, Socket, RSocketHandlers}) ->
    {ok, RSocket} =
        rsocket_transport:accept_connection(?MODULE, RSocketHandlers),
    {ok, #state{rsocket = RSocket, tcp_socket = Socket}};
init({initiate, Address, Port, RSocketHandlers}) ->
    SocketOptions = [binary, {active, true}],
    case gen_tcp:connect(Address, Port, SocketOptions) of
        {error, Reason} -> {stop, Reason};
        {ok, TCPSocket} ->
            {ok, RSocket} =
                rsocket_transport:initiate_connection(?MODULE, RSocketHandlers),
            {ok, #state{rsocket = RSocket, tcp_socket = TCPSocket}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call(get_rsocket, _From, S) ->
    {reply, S#state.rsocket, S};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast({send, Frame}, State) ->
    FrameLength = iolist_size(Frame),
    Packet = [<<FrameLength:24>>, Frame],
    ok = gen_tcp:send(State#state.tcp_socket, Packet),
    {noreply, State};

handle_cast(activate_socket, S) ->
    ok = inet:setopts(S#state.tcp_socket, [{active, true}]),
    {noreply, S};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({tcp, _Socket, <<FrameLength:24, Data/binary>>}, State) ->
    <<Frame:FrameLength/binary, _/binary>> = Data,
    ok = rsocket_transport:recv_frame(State#state.rsocket, Frame),
    {noreply, State};
handle_info(_Info, State) ->
    erlang:display(_Info),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.


%%%===================================================================
%%% Internal functions
%%%===================================================================

