-module(stream_test_ffi).

-export([listen/0, listen_port/1, accept/1, server_send/2, server_recv/2, close_listen/1, close_server/1, get_env/1]).

listen() ->
    {ok, ListenSocket} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}, {ip, {127, 0, 0, 1}}]),
    ListenSocket.

listen_port(ListenSocket) ->
    {ok, Port} = inet:port(ListenSocket),
    Port.

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket, 5000),
    Socket.

server_send(Socket, Data) ->
    ok = gen_tcp:send(Socket, Data),
    nil.

server_recv(Socket, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Data} -> {ok, Data};
        {error, _Reason} -> {error, nil}
    end.

close_listen(Socket) ->
    gen_tcp:close(Socket),
    nil.

close_server(Socket) ->
    gen_tcp:close(Socket),
    nil.

get_env(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, list_to_binary(Value)}
    end.
