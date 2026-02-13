# simple_client

Demonstrates the direct `FtpClient` API for basic FTP operations.

## What it does

1. Connects and logs in to an FTP server
2. Prints the current working directory
3. Lists directory contents
4. Uploads a small text file (`gftp_example.txt`)
5. Downloads it back and prints the content
6. Deletes the uploaded file
7. Disconnects

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
