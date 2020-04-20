-module(rsocket_tcp_connection_sup).
-behaviour(supervisor).

%% API
-export([
         start_link/0,
         accept_connection/1,
         initiate_connection/3
        ]).

%% Supervisor callbacks
-export([
         init/1
        ]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

accept_connection(Socket) ->
    {ok, Pid} = supervisor:start_child(?SERVER, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    ok = rsocket_tcp_connection:activate_socket(Pid),
    {ok, Pid}.

initiate_connection(Address, Port, RSocketHandlers) ->
    supervisor:start_child(?SERVER, [Address, Port, RSocketHandlers]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init([]) ->
    SupFlags = #{
                 strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5
                },
    ChildSpec = #{
                  id => rsocket_tcp_connection,
                  start => {rsocket_tcp_connection, start_link, []},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [rsocket_tcp_connection]
                 },
    {ok, {SupFlags, [ChildSpec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
