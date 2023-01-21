#ifndef _ERLCOMMS_HEADER_
#define _ERLCOMMS_HEADER_

typedef unsigned char byte;

enum COMMAND{ ORDER, TARGET, HEADER, INFO, ENDLIST, OKAY };

enum HEADER_TYPE{ SYSTEM, PLAYER };

typedef struct header {
    HEADER_TYPE type;
    int cardID;
    int abltyID;
    int position;
} header;

int read_exact(byte* buf, int len);
int write_exact(byte* buf, int len);

int read_cmd(byte* buf);
int write_cmd(byte* buf, int len);

int handle_request(byte* buf);

#endif
