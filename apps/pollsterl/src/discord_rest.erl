-module(discord_rest).
-export([start_link/0, channel_send_message/2, set_token/1]).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3]).
-define(SERVER_NAME, ?MODULE).
-define(CONNECT_TIMEOUT, 10000).

%% Public API
%% ---------------
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, {}, []).

set_token(Token) ->
    gen_server:cast(?SERVER_NAME, {set_token, Token}).

% Discord - Channel API
%% ----------------
channel_send_message(ChannelId, Data) ->
    gen_server:call(?SERVER_NAME, {channel_send_message, ChannelId, Data}).

%% Private API
%% ----------------
connect() ->
    logger:debug("[discord:rest] Opening connection to Discord..."),
    {ok, ConnPid} = gun:open("discordapp.com", 443, #{protocols => [http], transport => tls}),
    {ok, _} = gun:await_up(ConnPid, ?CONNECT_TIMEOUT),
    {ok, ConnPid}.

%% Callback implementations
%% ---------------
init(_Args) ->
    {ok, {disconnected}}.

handle_call({channel_send_message, ChannelId, Data}, _From, State = {ready, #{token := Token}}) ->
    {ok, ConnPid} = connect(),

    RequestBody = jsone:encode(Data),
    StreamRef = gun:post(ConnPid, util:join(["/api/v6/channels/", ChannelId, "/messages"]), [
        {<<"Authorization">>, "Bot " ++ Token},
        {<<"Content-Type">>, <<"application/json">>}
    ], RequestBody),

    Result =
        case gun:await(ConnPid, StreamRef) of
            {response, nofin, _Status, _Headers} ->
                {ok, Response} = gun:await_body(ConnPid, StreamRef),
                {ok, Parsed, _} = util:parse_json(Response),
                {reply, {ok, Parsed}, State};
            Other ->
                {reply, {badresponse, Other}, State}
        end,
    gun:close(ConnPid),
    Result;

handle_call(Request, From, State) ->
    logger:info("[discord:rest] call ~w ~w ~w~n", [From, Request, State]),
    {reply, {ok}, State}.

handle_cast({set_token, Token}, {disconnected}) ->
    logger:debug("[discord:rest] Acquired token, client is now ready"),
    {noreply, {ready, #{token => Token}}};
handle_cast(Message, State) ->
    logger:info("[discord:rest] cast ~w ~w~n", [Message, State]),
    {noreply, State}.
