-module(rsocket_tcp_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:ensure_all_started(rsocket_tcp),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [
     test_open_close_connection,
     test_open_connection_to_not_listening_port
    ].


test_open_close_connection(_Config) ->
    Port = 4567,
    ok = rsocket_tcp:start_listening(Port),
    {ok, Connection} = rsocket_tcp:connect("127.0.0.1", Port),
    receive after 1000 -> ok end,
    ok = rsocket:close_connection(Connection).

test_open_connection_to_not_listening_port(_Config) ->
    Port = 6666,
    {error, _Reason} = rsocket_tcp:connect("127.0.0.1", Port).
