-module(test_container_ffi).
-export([setup_ftps/1]).

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
    cmd("docker exec -d " ++ binary_to_list(ContainerId)
        ++ " vsftpd"
        ++ " -opasv_min_port=21100"
        ++ " -opasv_max_port=21110"
        ++ " -opasv_address=127.0.0.1"
        ++ " /etc/vsftpd/vsftpd_tls.conf", 5000),
    timer:sleep(2000),
    nil.

%% Internal

exec(ContainerId, ShellCmd) ->
    cmd("docker exec " ++ binary_to_list(ContainerId)
        ++ " sh -c \"" ++ ShellCmd ++ "\"", 60000).

%% Run a shell command with an explicit timeout (ms).
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
