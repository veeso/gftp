-module(test_container_ffi).
-export([start_ftp_container/0, get_ftp_port/1, get_mapped_port/2, stop_container/1]).

-define(IMAGE, "delfer/alpine-ftp-server:latest").

start_ftp_container() ->
    %% Clean up any stale containers from previous failed runs
    cmd("docker ps -aq --filter ancestor=" ?IMAGE
        " | xargs docker rm -f 2>/dev/null; true", 10000),
    Cmd = "docker run -d"
        " -e \"USERS=test|test|/home/test\""
        " -e ADDRESS=127.0.0.1"
        " -e MIN_PORT=21100"
        " -e MAX_PORT=21110"
        " -p 2121:21"
        " -p 21100-21110:21100-21110"
        " " ?IMAGE,
    ContainerId = string:trim(cmd(Cmd, 60000)),
    wait_for_ready(ContainerId, 30),
    list_to_binary(ContainerId).

get_ftp_port(_ContainerId) ->
    2121.

get_mapped_port(ContainerId, ContainerPort) ->
    Output = string:trim(cmd(
        "docker port " ++ binary_to_list(ContainerId) ++ " "
        ++ integer_to_list(ContainerPort), 5000)),
    FirstLine = hd(string:split(Output, "\n")),
    [_, PortStr] = string:split(FirstLine, ":", trailing),
    list_to_integer(string:trim(PortStr)).

stop_container(ContainerId) ->
    cmd("docker rm -f " ++ binary_to_list(ContainerId), 10000),
    nil.

%% Internal

wait_for_ready(_ContainerId, 0) ->
    error(container_start_timeout);
wait_for_ready(ContainerId, Retries) ->
    timer:sleep(1000),
    Logs = cmd("docker logs " ++ ContainerId ++ " 2>&1", 5000),
    case string:find(Logs, "passwd:") of
        nomatch ->
            wait_for_ready(ContainerId, Retries - 1);
        _ ->
            %% Give vsftpd a moment to finish starting after user setup
            timer:sleep(1000),
            ok
    end.

%% Run a shell command with an explicit timeout (ms).
%% Uses open_port instead of os:cmd to avoid hangs.
cmd(Command, Timeout) ->
    Port = open_port(
        {spawn, "/bin/sh -c '" ++ escape_single_quotes(Command) ++ "'"},
        [stream, exit_status, use_stdio, binary]),
    cmd_collect(Port, [], Timeout).

cmd_collect(Port, Acc, Timeout) ->
    receive
        {Port, {data, Data}} ->
            cmd_collect(Port, [Data | Acc], Timeout);
        {Port, {exit_status, _}} ->
            binary_to_list(iolist_to_binary(lists:reverse(Acc)))
    after Timeout ->
        catch port_close(Port),
        binary_to_list(iolist_to_binary(lists:reverse(Acc)))
    end.

escape_single_quotes(Str) ->
    lists:flatmap(
        fun($') -> "'\\''";
           (C) -> [C]
        end, Str).
