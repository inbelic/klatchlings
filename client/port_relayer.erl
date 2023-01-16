-module(port_relayer).

%% server esque exports
-export([start_link/1, init/2, stop/1]).

%% user exports
-export([target/1]).

target(Int) ->
    relayer ! {trgt, Int}.

stop(Reason) ->
    relayer ! {stop, Reason}.

start_link(Parent) ->
    UIPath = "~/dev/trg/ui/build/trg-ui",
    Port = spawn_link(?MODULE, init, [UIPath, Parent]),
    {ok, Port}.

init(ExtPrg, Parent) ->
    register(relayer, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port, Parent).

loop(Port, Parent) ->
    receive
        {trgt, Int} when is_integer(Int) ->
            Port ! {self(), {command, encode({trgt, Int})}},
            receive
                {Port, {data, Data}} ->
                    Parent ! {return, decode(Data)}
            end,
            loop(Port, Parent);
        {stop, Reason} ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit(port_terminated)
    end.

encode({trgt, Int}) -> [0, Int].
decode([Int]) -> Int.
