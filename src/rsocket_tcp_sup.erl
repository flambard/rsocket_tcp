-module(rsocket_tcp_sup).
-behaviour(supervisor).

%% API
-export([
         start_link/0
        ]).

%% supervisor callbacks
-export([
         init/1
        ]).

-define(SERVER, ?MODULE).


start_link() ->
    Port = 4567, %% TODO: Put this in configuration
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).


%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([Port]) ->
    SupFlags = #{
                 strategy => one_for_all,
                 intensity => 0,
                 period => 1
                },
    RSocketConnectionSup = #{
                             id => rsocket_connection_sup,
                             start => {rsocket_connection_sup, start_link, []},
                             restart => permanent,
                             shutdown => 5000,
                             type => supervisor,
                             modules => [rsocket_connection_sup]
                            },
    TCPConnectionSup = #{
                         id => rsocket_tcp_connection_sup,
                         start => {rsocket_tcp_connection_sup, start_link, []},
                         restart => permanent,
                         shutdown => 5000,
                         type => supervisor,
                         modules => [rsocket_tcp_connection_sup]
                        },
    TCPAcceptorSup = #{
                       id => rsocket_tcp_acceptor_sup,
                       start => {rsocket_tcp_acceptor_sup, start_link, [Port]},
                       restart => permanent,
                       shutdown => 3000,
                       type => supervisor,
                       modules => [rsocket_tcp_acceptor_sup]
                      },
    {ok, {SupFlags,
          [
           RSocketConnectionSup,
           TCPConnectionSup,
           TCPAcceptorSup
          ]}}.
