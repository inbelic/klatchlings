#ifndef _GAME_HEADER_
#define _GAME_HEADER_

#include "header.h"
#include <map>

typedef std::map<Fields, int> FieldMap;
typedef std::map<int, FieldMap> CardState;

class Game
{
private:

public:

};


class GameInternal
{
private:
    CardState cardState;
public:
    GameInternal();
    ~GameInternal();

    void update(CardState info);
};

#endif
