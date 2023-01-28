#ifndef _ERLCOMMS_HEADER_
#define _ERLCOMMS_HEADER_

#include <stdio.h>
#include <unistd.h>
#include <atomic>
#include "header.h"

typedef unsigned char byte;

int handle_request(byte *buf, std::atomic<bool> &gate);

#endif
