#include "../include/erl_comms.h"

void *get_in_addr(struct sockaddr *sa)
{
    if (sa->sa_family == AF_INET) {
        return &(((struct sockaddr_in*)sa)->sin_addr);
    }

    return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

int start_conn(const char *host, const char *port, int &sockfd)
{
    struct addrinfo hints, *servinfo, *p;

    int rv;
    char s[INET6_ADDRSTRLEN];

    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    if ((rv = getaddrinfo(host, port, &hints, &servinfo)) != 0) {
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
        return 1;
    }

    for (p = servinfo; p != NULL; p = p->ai_next) {
        if ((sockfd = socket(p->ai_family, p->ai_socktype,
                        p->ai_protocol)) == -1) {
            perror("client: socket");
            continue;
        }

        if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
            close(sockfd);
            perror("client: connect");
            continue;
        }

        break;
    }

    if (p == NULL) {
        fprintf(stderr, "client: failed to connect\n");
        return 2;
    }

    inet_ntop(p->ai_family, get_in_addr((struct sockaddr *)p->ai_addr),
            s, sizeof s);
    printf("client: connecting to %s\n", s);

    freeaddrinfo(servinfo);

    return 0;
}


// send to_send bytes of buf or die trying
int send_all(int sockfd, int to_send, const byte *buf)
{
    int sent, ttl_sent = 0;
    while (0 < to_send) {
        sent = send(sockfd, buf + ttl_sent, to_send, 0);
        if (sent == -1)
            return -1;
        ttl_sent += sent;
        to_send -= sent;
    }

    return ttl_sent;
}

// send the desired number of bytes to send then send
// that many bytes of buf
int send_buf(int sockfd, int to_send, const byte *buf)
{
    byte size = 0xff & to_send;

    if (send(sockfd, &size, 1, 0) != 1)
        return -1;

    if (size == 0) return 0;
    return send_all(sockfd, to_send, buf);
}

// compute the number of bytes of the input and then send it
// in the required chunks
int send_input(int sockfd, const byte *input)
{
    int to_send = strlen(input);
    int sent = 0;
    
    while (255 < to_send) {
        send_buf(sockfd, 255, input + sent);
        to_send -= 255;
        sent += 255;
    }
    sleep(1);
    to_send -= send_buf(sockfd, to_send, input + sent);

    int ack;
    byte buf[1];
    ack = recv(sockfd, buf, 1, 0);

    if (to_send != 0 || ack != 1)
        return -1;

    return buf[0];
}

int recv_output(int sockfd, byte *buf, byte *output, int &size)
{
    int to_recv;
    int recvd = 0;
    do {
        if (recv(sockfd, buf, 1, 0) != 1)
            return -1;

        to_recv = buf[0];
        printf("will take %d bytes\n", to_recv);
        if (to_recv == 0)
            break;
        if (recv(sockfd, buf, to_recv, 0) != to_recv)
            return -1;
        printf("will got '%s'\n", buf);
        if (size <= recvd + to_recv)
            size = recvd * 2;
            output = (byte *) realloc(output, size);
        memcpy((void *) (output + recvd), (const void *)buf, to_recv);
        recvd += to_recv;
    } while (255 < to_recv);
    output[recvd] = '\0';

    printf("client: received '%s'\n", output);
    return recvd;
}
