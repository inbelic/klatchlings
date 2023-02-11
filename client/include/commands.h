#ifndef _COMMANDS_HEADER_
#define _COMMANDS_HEADER_
// a header file to define the enumerations used for commands between
// the client and the erl server

// corresponds to the include/command.hrl file in server

#define PORT "4560"

enum SERVER_RECV_ERROR { OKAY, BAD_HEADER, BAD_COMMAND, BAD_ARGS };

enum COMMANDS {
    ORDER, TARGET, // Game commands
    PLAY, QUIT     // Menu commands
};

enum REQUEST {
    VIEW, ORDR, TRGT, RAND
};


#endif
