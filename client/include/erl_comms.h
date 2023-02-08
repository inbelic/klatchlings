#ifndef _ERL_COMMS_HEADER_
#define _ERL_COMMS_HEADER_

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <cstring>

#include <arpa/inet.h>

#include "commands.h"

#define MAXDATASIZE 100

typedef char byte;

int start_conn(const char *host, const char *port, int &sockfd);

int send_request(int sockfd, const byte *request);

int recv_response(int sockfd, byte *response);

#endif
