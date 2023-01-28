#ifndef _HEADER_HEADER_
#define _HEADER_HEADER_

struct Header {
    bool system;
    int position;
    int cardID;
    int abilityID;
};

enum REQUEST_TYPE { CMD_TARGET, CMD_ORDER };
enum RESPONSE_TYPE { OKAY, VALUE, EOL };

#endif
