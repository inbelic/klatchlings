#include <stdio.h>
#include <unistd.h>
#include "erlcomms.h"

// Helpers defined below
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);

int handle_order(byte *buf, int amount);
int handle_target(byte *buf, int amount);

int recv_header(byte *buf, Header *hdr);
int recv_cid(byte *buf, int *cID);

int send_okay(byte *buf);
int confirm_okay(byte *buf);

int handle_request(byte* buf)
{
    int type, amount, ret;

    if (read_cmd(buf) <= 0) {
        return -1;
    }

    type = static_cast<REQUEST_TYPE>(buf[0]);
    amount = buf[1];

    send_okay(buf);

    switch (type) {
        case CMD_TARGET:
            ret = handle_target(buf, amount);
            break;
        case CMD_ORDER:
            ret = handle_order(buf, amount);
            break;
        defualt:
            break;
    }

    return ret;
}

// Private Helper Functions
int read_exact(byte *buf, int len)
{
    int i, got = 0;

    do {
        if ((i = read(0, buf + got, len - got)) <= 0) {
            return i;
        }
        got += i;
    } while (got < len);

    return len;
}

int write_exact(byte *buf, int len)
{
    int i, wrote = 0;
    do {
        if ((i = write(1, buf + wrote, len - wrote)) <= 0) {
            return i;
        }
        wrote += i;
    } while (wrote < len);

    return len;
}

int read_cmd(byte *buf)
{
    int len;

    if (read_exact(buf, 2) != 2) {
        return -1;
    }

    len = (buf[0] << 8) | buf[1];
    return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
    byte li;

    li = (len >> 8) & 0xff;
    write_exact(&li, 1);

    li = len & 0xff;
    write_exact(&li, 1);

    return write_exact(buf, len);
}


// Helper Function
int send_order(byte *buf, int posn) {
    buf[0] = VALUE;
    buf[1] = posn;
    write_cmd(buf, 2);

    return confirm_okay(buf);
}


int handle_order(byte *buf, int amount)
{
    // First we will receive the headers from the port
    Header *hdrs = new Header[amount];
    Header *cur_hdr = hdrs;

    for (int i = 0; i < amount; i++) {
        recv_header(buf, cur_hdr);
        cur_hdr++;
    }

    // Then we will need to do some fancy stuff to get the order
    int order[amount];

    // Then we can send back our ordering
    for (int i = 0; i < amount; i++) {
        order[i] = i + 1;
        if (send_order(buf, order[i]) <= 0)
            break;
    }
    buf[0] = EOL;
    write_cmd(buf, 1);
    int ret = confirm_okay(buf);

    delete []hdrs;
    return ret;
}

int send_target(byte *buf, int tcID)
{
    buf[0] = VALUE;
    buf[1] = tcID;
    write_cmd(buf, 2);

    return confirm_okay(buf);
}

int handle_target(byte *buf, int amount)
{
    Header *hdr = new Header;
    recv_header(buf, hdr);

    int *rngIDs = new int[amount];
    int *cur = rngIDs;
    
    for (int i = 0; i < amount; i++) {
        recv_cid(buf, cur);
        cur++;
    }

    // Then we will need to do some fancy stuff to get the order
    int target;
    target = rngIDs[0];

    int ret = send_target(buf, target);

    delete []rngIDs;
    delete hdr;
    return ret;
}

int recv_header(byte *buf, Header *hdr)
{
    if (read_cmd(buf) <= 0) {
        return -1;
    }
    hdr->system = 0 == buf[0];
    hdr->position = buf[1];
    send_okay(buf);

    if (read_cmd(buf) <= 0) {
        return -1;
    }
    hdr->cardID = buf[0];
    hdr->abilityID = buf[1];
    send_okay(buf);

    return 0;
}

int recv_cid(byte *buf, int *cID)
{
    if (read_cmd(buf) <= 0) {
        return -1;
    }
    *cID = buf[0];
    send_okay(buf);
    return 0;
}

int send_okay(byte *buf)
{
    buf[0] = static_cast<int>(OKAY);
    buf[1] = static_cast<int>(OKAY);
    return write_cmd(buf, 2);
}

int confirm_okay(byte *buf)
{
    int ret = read_cmd(buf);
    if (buf[0] != OKAY || buf[1] != OKAY)
        return -1;
    return ret;
}
