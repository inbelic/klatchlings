#include "game.h"

GameInternal::GameInternal()
{
}

GameInternal::~GameInternal()
{
}

bool swappedZones(FieldMap newFM, FieldMap oldFM)
{
    auto newIt = newFM.find(Zone);
    if (newIt == newFM.end()) return false;

    auto oldIt = oldFM.find(Zone);
    if (oldIt == oldFM.end()) return true;

    return newIt->second == oldIt->second;
}

void GameInternal::update(CardState info)
{
    for(auto card = info.begin(); card != info.end(); ++card) {
        int cardID = card->first;
        FieldMap newFM = card->second;
        auto ourFMIt = cardState.find(cardID);

        if (ourFMIt == cardState.end()) {
            // If we don't have this yet then we can add all info
            cardState[cardID] = newFM;
        } else {
            FieldMap ourFM = ourFMIt->second;

            if (swappedZones(newFM, ourFM)) {
                // Then we replace the FM for new schema
                cardState[cardID] = newFM;
            } else {
                // Otherwise, we overwrite the changed fields
                for (auto field = newFM.begin(); field != newFM.end(); ++field)
                    ourFM[field->first] = field->second;
            }
        }
    }
}
