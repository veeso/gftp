# actor_client

Demonstrates the OTP actor wrapper (`gftp/actor`) for safe concurrent FTP operations with message-based streaming.

## What it does

1. Connects to an FTP server and wraps the client in an OTP actor
2. Logs in, prints working directory, and lists files
3. Uploads a file using `open_stor` (message-based streaming)
4. Demonstrates **chunk protection**: shows that control commands return `DataTransferInProgress` while a data channel is open
5. Downloads the file using `open_retr` with a `process.Selector` receive loop
6. Cleans up and disconnects

## Running

To quickly spin up a local FTP server using Docker:

```sh
docker run -d --name gftp-test-ftp \
  -e "USERS=test|test|/home/test" \
  -e ADDRESS=127.0.0.1 \
  -e MIN_PORT=21100 \
  -e MAX_PORT=21110 \
  -p 2121:21 \
  -p 21100-21110:21100-21110 \
  delfer/alpine-ftp-server:latest
```

Then run the client with the appropriate environment variables:

```sh
FTP_HOST=127.0.0.1 FTP_PORT=2121 FTP_USER=test FTP_PASSWORD=test gleam run
```
