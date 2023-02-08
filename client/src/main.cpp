#include "../include/erl_comms.h"

int main()
{
    int sockfd;
    char buf[MAXDATASIZE];
    const char *host = "localhost";
    const char *port = PORT;

    if (start_conn(host, port, sockfd) != 0)
        exit(1);

    const char *resp = "Howdy this is quite a long test string to enforce that we can handle a longer test string and anything that is over 255 chars long is then essentially arbitrarily long since it will deal with it equivalently. Furthremore, we can add some sleeps in the send of the request to mimic a shaky network to deal with";
    if (recv_response(sockfd, buf) != 0)
        exit(1);
    if (send_request(sockfd, resp) == -1)
        exit(1);

    close(sockfd);

    return 0;
}
