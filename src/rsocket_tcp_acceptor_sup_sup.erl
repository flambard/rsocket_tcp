-module(rsocket_tcp_acceptor_sup_sup).
-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_tcp_acceptor_supervisor/1
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

start_tcp_acceptor_supervisor(Port) ->
    supervisor:start_child(?SERVER, [Port]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{
                 strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5
                },
    ChildSpec = #{
                  id => rsocket_tcp_acceptor_sup,
                  start => {rsocket_tcp_acceptor_sup, start_link, []},
                  restart => permanent,
                  shutdown => 3000,
                  type => supervisor,
                  modules => [rsocket_tcp_acceptor_sup]
                 },
    {ok, {SupFlags, [ChildSpec]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
