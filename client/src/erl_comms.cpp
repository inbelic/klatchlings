#include "../include/erl_comms.h"

// Address collection taken from Beej's guide to socket programming
void *get_in_addr(struct sockaddr *sa)
{
    if (sa->sa_family == AF_INET) {
        return &(((struct sockaddr_in*)sa)->sin_addr);
    }

    return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

// Starting a connection taken from Beej's guide to socket programming
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

void mk_request(Request **req, const std::vector<byte> bytes)
{
    // First clean up the old request
    delete [] (*req)->args;
    delete *req;

    // Then determine the cmd byte
    byte cmd_byte = bytes.front();
    COMMAND cmd;
    if (3 < cmd_byte) cmd = UNKNOWN;
    else cmd = static_cast<COMMAND>(cmd_byte);

    // And then allocate and store the incoming args
    int len = bytes.size();
    byte *args = new byte[len + 1];
    for (int i = 0; i < len; i++)
        args[i] = bytes[i];
    args[len] = '\0';
   
    // 'Publish' the changes that will then awaken the other thread
    Request *new_req = new Request();
    new_req->cmd = cmd;
    new_req->args = args;
    new_req->len = len;
    
    *req = new_req;
}

void stream_recv(int sockfd, Request **req, bool *running,
        std::condition_variable *cv, std::mutex *cv_m)
{
    byte buf[MAXDATASIZE];
    std::vector<byte> bytes;
    int recvd, i;

    while (*running) {
        recvd = recv(sockfd, buf, MAXDATASIZE, 0);
        if (recvd <= 0) {
            std::lock_guard lk(*cv_m);
            *running = false;
            cv->notify_one();
            return;
        }
        for (i = 0; i < recvd; i++) {
            const byte &c = buf[i];
            bytes.push_back(c);
            if (c == '\0') {
                std::lock_guard lk(*cv_m);
                mk_request(req, bytes);
                bytes.erase(bytes.begin(), bytes.end());
                cv->notify_one();
            }
        }
    }
}
