-module(client_conn_mgr).

-include("../include/client_conn.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start/1]).

-record(state,
        { conns = []
        , client_config
        }).

start(Port) ->
    gen_server:start(?MODULE, Port, []).

init(Port) ->
    register(manager, self()), 
    {ok, ListenSocket}
        = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true},
                                {active, once}]),
    lists:foreach(fun(_) ->
                          gen_server:cast(self(), create)
                  end, lists:seq(1,5)),
    Config = #client_config{listen_socket = ListenSocket},
    {ok, #state{client_config = Config}}.

%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.


handle_cast(create, #state{client_config = Config, conns = Conns} = State) ->
    process_flag(trap_exit, true),
    {ok, Conn} = client_conn:start_link(Config),
    {noreply, State#state{conns = [Conn | Conns]}};

%% Error catch all
handle_cast(_Requst, State) ->
    {stop, unknown_request, State}.

handle_info({'EXIT', Conn, _Reason}, #state{conns = Conns} = State) ->
    {noreply, State#state{conns = lists:delete(Conn, Conns)}};
handle_info(_Info, State) ->
    {stop, unknown_info, State}.

terminate(Reason, #state{conns = Conns}) ->
    lists:foreach(fun(Conn) ->
                          gen_server:cast(Conn, {mgr_stop, Reason})
                  end, Conns),
    ok.
