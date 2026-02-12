-module(listener_ffi).

-export([listen/1, listener_port/1, accept/2, close/1]).

listen(IpFamily) ->
    Family = case IpFamily of
        ipv4 -> inet;
        ipv6 -> inet6
    end,
    case gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}, Family]) of
        {ok, ListenSocket} -> {ok, ListenSocket};
        {error, Reason} -> {error, Reason}
    end.

listener_port(ListenSocket) ->
    case inet:port(ListenSocket) of
        {ok, Port} -> {ok, Port};
        {error, Reason} -> {error, Reason}
    end.

accept(ListenSocket, Timeout) ->
    case gen_tcp:accept(ListenSocket, Timeout) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> {error, Reason}
    end.

close(ListenSocket) ->
    gen_tcp:close(ListenSocket),
    nil.
