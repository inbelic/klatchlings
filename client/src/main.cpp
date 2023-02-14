#include "../include/erl_comms.h"
#include <string>
#include <iostream>
#include <thread>
#include <atomic>

std::atomic<bool> running;

void recver(int sockfd)
{
    int size = 1024;
    int recvd;
    byte *output = (byte *) malloc(size);
    char buf[MAXDATASIZE];

    while (running) {
        recvd = recv_output(sockfd, buf, output, size);
        if (recvd == -1)
            running = false;
        else
            printf("recvd: '%s'\n", output);
    }

    printf("stop recving\n");
}

void sender(int sockfd)
{
    const char *resp;
    std::string input;

    while (running) {
        std::getline(std::cin, input);

        resp = input.c_str();

        int status;
        status = send_input(sockfd, resp);

        if (status != 0)
            running = false;
        else
            printf("status: %d\n", status);
    }
}

int main()
{
    running = true;
    int sockfd;
    const char *host = "localhost";
    const char *port = PORT;

    if (start_conn(host, port, sockfd) != 0)
        exit(1);

    std::thread recv_main(recver, sockfd);
    std::thread send_main(sender, sockfd);

    recv_main.join();
    send_main.join();

    close(sockfd);

    return 0;
}
