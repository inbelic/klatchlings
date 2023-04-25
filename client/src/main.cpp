#include "../include/erl_comms.h"
#include "../include/commands.h"
#include <string>
#include <iostream>

std::atomic<bool> running;

int main()
{
    const char *host = "localhost";
    const char *port = PORT;
    int sockfd;

    if (start_conn(host, port, sockfd) != 0)
        exit(1);

    std::condition_variable cv;
    std::mutex cv_m;
    bool running = true;

    Request *req = nullptr;

    std::thread stream_thrd(stream_recv, sockfd, &req, &running, &cv, &cv_m);

    close(sockfd);

    return 0;
}
