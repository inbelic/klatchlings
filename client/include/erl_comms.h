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

#define MAXDATASIZE 256

typedef char byte;

int start_conn(const char *host, const char *port, int &sockfd);

int send_input(int sockfd, const byte *input);

int recv_output(int sockfd, byte *response, byte *output, int &size);

#endif
