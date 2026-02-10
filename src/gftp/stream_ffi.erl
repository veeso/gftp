-module(stream_ffi).

-export([patch_wrap_options/1]).

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
