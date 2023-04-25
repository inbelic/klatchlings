#ifndef _ERL_COMMS_HEADER_
#define _ERL_COMMS_HEADER_

// Networking imports
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

// Custom imports
#include "commands.h"

// C++ standard imports
#include <thread>
#include <atomic>
#include <condition_variable>
#include <vector>

#define MAXDATASIZE 1024

typedef char byte;

int start_conn(const char *host, const char *port, int &sockfd);

void stream_recv(int sockfd, Request **req, bool *running,
        std::condition_variable *cv, std::mutex *cv_m);

#endif
