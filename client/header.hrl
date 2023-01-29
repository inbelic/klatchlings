%% Records
-record(header,
        { system
        , position
        , cardID
        , abilityID
        }).

%% Request Enums
-define(CMD_TARGET, 0).
-define(CMD_ORDER, 1).
-define(CMD_INFO, 2).

%% Response Enums
-define(OKAY, 0).
-define(VALUE, 1).
-define(EOL, 2).

%% Field Enums
-define(OWNER, 0).
-define(ZONE, 1).
-define(MANA, 2).
-define(POWER, 3).
-define(TOUGHNESS, 4).
-define(COST, 5).
-define(FATIGUED, 6).
-define(POSITION, 7).

%% Zone Enums
-define(HAND, 0).
-define(STACK, 1).
-define(BARRACK, 2).
-define(GRAVE, 3).
-define(BATTLEFIELD, 4).
-define(THRONE, 5).
-define(TOPDECK, 6).
-define(MIDDECK, 7).
-define(BOTDECK, 8).
