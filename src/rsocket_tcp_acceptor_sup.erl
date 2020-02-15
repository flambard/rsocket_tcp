-module(rsocket_tcp_acceptor_sup).
-behaviour(supervisor).

-export([
         start_link/1,
         start_socket/0
        ]).

%% supervisor callbacks
-export([
         init/1
        ]).


start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).


init([Port]) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, false}]),
    spawn_link(fun empty_listeners/0),
    SupervisorFlags = #{
                        strategy => simple_one_for_one,
                        intensity => 60,
                        period => 3600
                       },
    ChildSpec = #{
                  id => rsocket_tcp_acceptor,
                  start => {rsocket_tcp_acceptor, start_link, [ListenSocket]},
                  restart => temporary,
                  shutdown => 1000,
                  type => worker,
                  modules => [rsocket_tcp_acceptor]
                 },
    {ok, {SupervisorFlags, [ChildSpec]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,3)],
    ok.

