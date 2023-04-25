#ifndef _COMMANDS_HEADER_
#define _COMMANDS_HEADER_
// a header file to define the enumerations used for commands between
// the client and the erl server

// corresponds to the include/command.hrl file in server

#define PORT "4560"

typedef char byte;

enum SERVER_RECV_ERROR { OKAY, BAD_HEADER, BAD_COMMAND, BAD_ARGS };

enum COMMAND {
    ORDER, TARGET, // Game commands
    PLAY, QUIT,     // Menu commands
    UNKNOWN
};

enum REQUEST {
    VIEW, ORDR, TRGT, RAND
};

struct Request
{
    COMMAND cmd;
    byte *args;
    int len;
};

#endif
