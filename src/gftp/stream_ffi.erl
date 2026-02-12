-module(stream_ffi).

-export([patch_wrap_options/1,
         set_tcp_packet_line/1, set_tcp_packet_raw/1,
         set_ssl_packet_line/1, set_ssl_packet_raw/1,
         local_address/1, peer_address/1,
         tcp_controlling_process/2, ssl_controlling_process/2]).

%% Workaround for kafein bug: kafein passes the server_name_indication as a
%% binary to ssl:connect, but Erlang's ssl module expects a charlist.
%% This function patches the WrapOptions tuple (element 11) to convert
%% the binary hostname to a charlist before kafein.wrap processes it.
%% See: https://github.com/fuzzko/kafein/issues/1
patch_wrap_options(Options) ->
    case erlang:element(11, Options) of
        {some, Name} when is_binary(Name) ->
            erlang:setelement(11, Options, {some, binary_to_list(Name)});
        _ ->
            Options
    end.

set_tcp_packet_line(Socket) ->
    inet:setopts(Socket, [{packet, line}]),
    nil.

set_tcp_packet_raw(Socket) ->
    inet:setopts(Socket, [{packet, raw}]),
    nil.

set_ssl_packet_line(Socket) ->
    ssl:setopts(Socket, [{packet, line}]),
    nil.

set_ssl_packet_raw(Socket) ->
    ssl:setopts(Socket, [{packet, raw}]),
    nil.

local_address(Socket) ->
    case inet:sockname(Socket) of
        {ok, {Addr, Port}} ->
            {ok, {list_to_binary(inet:ntoa(Addr)), Port}};
        {error, Reason} ->
            {error, Reason}
    end.

peer_address(Socket) ->
    case inet:peername(Socket) of
        {ok, {Addr, Port}} ->
            {ok, {list_to_binary(inet:ntoa(Addr)), Port}};
        {error, Reason} ->
            {error, Reason}
    end.

tcp_controlling_process(Socket, Pid) ->
    case gen_tcp:controlling_process(Socket, Pid) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.

ssl_controlling_process(Socket, Pid) ->
    case ssl:controlling_process(Socket, Pid) of
        ok -> {ok, nil};
        {error, Reason} -> {error, Reason}
    end.
