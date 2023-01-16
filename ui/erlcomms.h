#ifndef _ERLCOMMS_HEADER_
#define _ERLCOMMS_HEADER_

typedef unsigned char byte;

enum COMMANDS{ TARGET_HEADER, TARGET_CONTENTS, TARGET_EOF };

int read_exact(byte* buf, int len);
int write_exact(byte* buf, int len);

int read_cmd(byte* buf);
int write_cmd(byte* buf, int len);

int handle_request(byte* buf);

#endif
