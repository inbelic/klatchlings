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

%% Response Enums
-define(OKAY, 0).
-define(VALUE, 1).
-define(EOL, 2).
