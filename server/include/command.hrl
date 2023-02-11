%% a header file to define the enumerations used for commands between
%% the client and the erl server

%% corresponds to the include/command.h file in client

-define(PORT, 4560).

%% Server Receive Error Codes
-define(OKAY, 0).
-define(BAD_HEADER, 1).
-define(BAD_COMMAND, 2).
-define(BAD_ARGS, 3).

%% Game Commands
-define(ORDER, 0).
-define(TARGET, 1).

%% Menu Commands
-define(PLAY, 2).
-define(QUIT, 3).


%% GAME REQUESTS
-define(VIEW, 0).
-define(ORDR, 1).
-define(TRGT, 2).
-define(RAND, 3).
