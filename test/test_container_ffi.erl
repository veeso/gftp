-module(test_container_ffi).
-export([start_ftp_container/0, start_ftps_container/0,
         get_ftp_port/1, get_ftps_port/1, get_mapped_port/2, stop_container/1]).

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

start_ftps_container() ->
    %% Clean up any stale containers from previous failed runs
    cmd("docker ps -aq --filter ancestor=" ?IMAGE
        " | xargs docker rm -f 2>/dev/null; true", 10000),
    Cmd = "docker run -d"
        " -e \"USERS=test|test|/home/test\""
        " -e ADDRESS=127.0.0.1"
        " -e MIN_PORT=21100"
        " -e MAX_PORT=21110"
        " -p 2122:990"
        " -p 21100-21110:21100-21110"
        " " ?IMAGE,
    ContainerId = string:trim(cmd(Cmd, 60000)),
    wait_for_ready(ContainerId, 30),
    setup_ftps(ContainerId),
    list_to_binary(ContainerId).

get_ftps_port(_ContainerId) ->
    2122.

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

%% FTPS setup: generate a self-signed cert and start a second vsftpd
%% instance with TLS enabled on port 990 inside the container.
setup_ftps(ContainerId) ->
    exec(ContainerId, "apk add --no-cache openssl > /dev/null 2>&1"),
    exec(ContainerId, "mkdir -p /etc/ssl/private"),
    exec(ContainerId,
        "openssl req -x509 -nodes -days 1 -newkey rsa:2048"
        " -keyout /etc/ssl/private/vsftpd.key"
        " -out /etc/ssl/certs/vsftpd.crt"
        " -subj /CN=localhost 2>/dev/null"),
    exec(ContainerId,
        "grep -v listen_port /etc/vsftpd/vsftpd.conf"
        " > /etc/vsftpd/vsftpd_tls.conf"),
    exec(ContainerId,
        "echo listen_port=990 >> /etc/vsftpd/vsftpd_tls.conf && "
        "echo ssl_enable=YES >> /etc/vsftpd/vsftpd_tls.conf && "
        "echo rsa_cert_file=/etc/ssl/certs/vsftpd.crt >> /etc/vsftpd/vsftpd_tls.conf && "
        "echo rsa_private_key_file=/etc/ssl/private/vsftpd.key >> /etc/vsftpd/vsftpd_tls.conf && "
        "echo allow_anon_ssl=NO >> /etc/vsftpd/vsftpd_tls.conf && "
        "echo force_local_data_ssl=NO >> /etc/vsftpd/vsftpd_tls.conf && "
        "echo force_local_logins_ssl=NO >> /etc/vsftpd/vsftpd_tls.conf && "
        "echo require_ssl_reuse=NO >> /etc/vsftpd/vsftpd_tls.conf"),
    cmd("docker exec -d " ++ ContainerId
        ++ " vsftpd"
        ++ " -opasv_min_port=21100"
        ++ " -opasv_max_port=21110"
        ++ " -opasv_address=127.0.0.1"
        ++ " /etc/vsftpd/vsftpd_tls.conf", 5000),
    timer:sleep(2000),
    ok.

exec(ContainerId, ShellCmd) ->
    cmd("docker exec " ++ ContainerId
        ++ " sh -c \"" ++ ShellCmd ++ "\"", 60000).
