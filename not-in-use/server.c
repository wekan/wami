#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <proto/socket.h>
#include <proto/exec.h>
#include <proto/dos.h>

#define BUFFER_SIZE 1024
#define MAX_FILE_SIZE 1048576 // 1 MB (adjust as needed)

void send_response(int socket, const char *response) {
    send(socket, response, strlen(response), 0);
}

void send_file(int socket, const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (file) {
        char buffer[BUFFER_SIZE];
        while (fread(buffer, 1, BUFFER_SIZE, file) > 0) {
            send(socket, buffer, BUFFER_SIZE, 0);
        }
        fclose(file);
    }
}

void handle_request(int socket, char *request) {
    char method[10], uri[256], protocol[10];
    sscanf(request, "%s %s %s", method, uri, protocol);

    if (strcmp(method, "GET") == 0) {
        if (strcmp(uri, "/") == 0) {
            send_response(socket, "HTTP/1.0 200 OK\r\nContent-Type: text/html\r\n\r\n");
            send_response(socket, "<html><body><h1>Upload Form</h1><form action='/upload' method='post' enctype='multipart/form-data'>"
                               "<input type='file' name='file'/><br/><input type='submit' value='Upload'/></form></body></html>");
        } else if (strcmp(uri, "/upload") == 0) {
            send_response(socket, "HTTP/1.0 200 OK\r\nContent-Type: text/html\r\n\r\n");
            send_response(socket, "<html><body><h1>File Uploaded Successfully</h1></body></html>");
        } else {
            send_response(socket, "HTTP/1.0 404 Not Found\r\nContent-Type: text/plain\r\n\r\n");
            send_response(socket, "404 Not Found");
        }
    } else if (strcmp(method, "POST") == 0 && strcmp(uri, "/upload") == 0) {
        char boundary[256];
        char line[BUFFER_SIZE];
        FILE *file = NULL;
        int content_length = 0;
        int file_started = 0;

        while (fgets(line, BUFFER_SIZE, stdin) != NULL) {
            if (strncmp(line, "Content-Type: multipart/form-data; boundary=", 44) == 0) {
                sscanf(line, "Content-Type: multipart/form-data; boundary=%s", boundary);
            } else if (strncmp(line, "Content-Length: ", 15) == 0) {
                sscanf(line, "Content-Length: %d", &content_length);
            } else if (strncmp(line, boundary, strlen(boundary)) == 0) {
                if (file_started) {
                    fclose(file);
                    file = NULL;
                }
                file_started = 1;
            } else if (file_started) {
                if (file == NULL) {
                    file = tmpfile();
                }
                fwrite(line, 1, strlen(line) - 1, file);
            }
        }

        if (file) {
            fclose(file);
        }

        send_response(socket, "HTTP/1.0 200 OK\r\nContent-Type: text/html\r\n\r\n");
        send_response(socket, "<html><body><h1>File Uploaded Successfully</h1></body></html>");
    } else {
        send_response(socket, "HTTP/1.0 405 Method Not Allowed\r\nContent-Type: text/plain\r\n\r\n");
        send_response(socket, "405 Method Not Allowed");
    }
}

int main() {
    struct SocketBase *socket_base = (struct SocketBase *)OpenLibrary("socket.library", 0);
    if (socket_base == NULL) {
        printf("Failed to open socket.library\n");
        return 1;
    }

    int server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (server_socket < 0) {
        printf("Failed to create socket\n");
        CloseLibrary((struct Library *)socket_base);
        return 1;
    }

    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(8080);
    server_addr.sin_addr.s_addr = INADDR_ANY;

    if (bind(server_socket, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
        printf("Failed to bind socket\n");
        close(server_socket);
        CloseLibrary((struct Library *)socket_base);
        return 1;
    }

    if (listen(server_socket, 3) < 0) {
        printf("Failed to listen on socket\n");
        close(server_socket);
        CloseLibrary((struct Library *)socket_base);
        return 1;
    }

    printf("Server started. Listening on port 8080...\n");

    while (1) {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);
        int client_socket = accept(server_socket, (struct sockaddr *)&client_addr, &client_len);
        if (client_socket < 0) {
            printf("Failed to accept connection\n");
            continue;
        }

        char request[BUFFER_SIZE];
        int bytes_read = recv(client_socket, request, BUFFER_SIZE - 1, 0);
        if (bytes_read > 0) {
            request[bytes_read] = '\0';
            handle_request(client_socket, request);
        }

        close(client_socket);
    }

    close(server_socket);
    CloseLibrary((struct Library *)socket_base);
    return 0;
}
