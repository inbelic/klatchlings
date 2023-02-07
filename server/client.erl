-module(client).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-export([start_link/1]).

-record(state,
        { opponent = unqueued
        , server = undefined
        , inquiry = empty
        , socket = undefined
        }).

start_link({Server, ListenSocket}) ->
    gen_server:start_link(?MODULE, {Server, ListenSocket}, []).

init({Server, ListenSocket}) ->
    gen_server:cast(self(), accept),
    {ok, #state{server = Server, socket = ListenSocket}}.

%% Error catch all
handle_call(_Request, _From, State) ->
    {stop, unknown_request, State}.

handle_cast(accept, #state{server = Server, socket = ListenSocket} = State) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    client_sup:start_client(),
    gen_tcp:send(AcceptSocket, <<"enter username: ">>),
    ReplyStatus = handle_request(register, AcceptSocket, Server),
    {ReplyStatus, State};

%% Error catch all
handle_cast(_Requst, State) ->
    {stop, unknown_request, State}.

terminate(_Reason, _State) ->
    ok.

handle_request(Req, AcceptSocket, Server) ->
    receive
        {tcp, AcceptSocket, Bin} ->
            Response = decode(Bin),
            case Req of
                register ->
                    gen_server:cast(Server, {register, Response, self()}),
                    noreply
            end;
        {tcp_closed, AcceptSocket} ->
            gen_server:cast(Server, tcp_closed),
            stop;
        {error, closed} ->
            gen_tcp:close(AcceptSocket),
            gen_server:cast(Server, tcp_closed),
            stop
    end.

decode(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin).
