#ifndef _HEADER_HEADER_
#define _HEADER_HEADER_

struct Header {
    bool system;
    int position;
    int cardID;
    int abilityID;
};

enum REQUEST_TYPE { CMD_TARGET, CMD_ORDER, CMD_INFO };
enum RESPONSE_TYPE { OKAY, VALUE, EOL };

enum Fields { Owner, Zone, Mana, Power, Toughness, Cost, Fatigued, Position };
enum Zones { Hand, Stack, Barrack, Grave, Battlefield, Throne,
    TopDeck, MidDeck, BotDeck };

#endif
