-module(rsocket_tcp_acceptor_sup).
-behaviour(supervisor).

-export([
         start_link/2,
         start_socket/1
        ]).

%% supervisor callbacks
-export([
         init/1
        ]).


start_link(Port, RSocketHandlers) ->
    {ok, Pid} = supervisor:start_link(?MODULE, [Port, RSocketHandlers]),
    spawn_link(fun() -> empty_listeners(Pid) end),
    {ok, Pid}.


init([Port, RSocketHandlers]) ->
    {ok, ListenSocket} = rsocket_tcp_connection:start_listening_socket(Port),
    SupervisorFlags = #{
                        strategy => simple_one_for_one,
                        intensity => 60,
                        period => 3600
                       },
    ChildSpec = #{
                  id => rsocket_tcp_acceptor,
                  start => {
                            rsocket_tcp_acceptor,
                            start_link,
                            [ListenSocket, RSocketHandlers]
                           },
                  restart => temporary,
                  shutdown => 1000,
                  type => worker,
                  modules => [rsocket_tcp_acceptor]
                 },
    {ok, {SupervisorFlags, [ChildSpec]}}.

start_socket(Supervisor) ->
    supervisor:start_child(Supervisor, []).


empty_listeners(Supervisor) ->
    [start_socket(Supervisor) || _ <- lists:seq(1,3)],
    ok.

