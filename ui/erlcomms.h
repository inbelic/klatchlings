#ifndef _ERLCOMMS_HEADER_
#define _ERLCOMMS_HEADER_

#include <stdio.h>
#include <unistd.h>

enum REQUEST_TYPE { CMD_TARGET, CMD_ORDER };
enum RESPONSE_TYPE { OKAY, VALUE, EOL };

typedef unsigned char byte;

int handle_request(byte *buf);

#endif
