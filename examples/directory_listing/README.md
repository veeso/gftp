# directory_listing

Demonstrates parsing FTP directory listings into structured `File` records using `gftp/list`.

## What it does

1. Connects and logs in to an FTP server
2. Runs the `LIST` command and parses each line into a `File` struct
3. Displays structured file info: type, permissions, size, modified date, and name
4. Tries the `MLSD` command (machine-readable format) and parses it too, gracefully handling servers that don't support it
5. Disconnects

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
