-module(discord_gateway).
-export([start_link/0, open/1]).

-behaviour(gen_statem).
-export([init/1, callback_mode/0, handle_event/4, subscribe/1, unsubscribe/1]).
-define(SERVER_NAME, ?MODULE).

-define(OP_DISPATCH, 0).
-define(OP_HEARTBEAT, 1).
-define(OP_IDENTIFY, 2).
-define(OP_STATUS_UPDATE, 3).
-define(OP_VOICE_STATE_UPDATE, 4).
-define(OP_RESUME, 6).
-define(OP_RECONNECT, 7).
-define(OP_REQUEST_GUILD_MEMBERS, 8).
-define(OP_INVALID_SESSION, 9).
-define(OP_HELLO, 10).
-define(OP_HEARTBEAT_ACK, 11).

-define(EVENT_READY, <<"READY">>).
-define(EVENT_GUILD_CREATE, <<"GUILD_CREATE">>).

%% Public API
%% ---------------
start_link() ->
    gen_statem:start_link({local, ?SERVER_NAME}, ?MODULE, {}, []).

open(Token) ->
    gen_statem:call(?SERVER_NAME, {connect, Token}).

subscribe(Pid) ->
    gen_statem:cast(?SERVER_NAME, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_statem:cast(?SERVER_NAME, {unsubscribe, Pid}).

%% Private methods
%% ------------------------
parse_frame({text, Json}) ->
    jsone:try_decode(Json, [{object_format, map}, {keys, atom}]);
parse_frame({close, Reason, Message}) ->
    {ok, {close, Reason, Message}};
parse_frame(Other) ->
    {error, {unsupported, Other}}.

emit(Msg) ->
    gen_statem:cast(?SERVER_NAME, {emit, Msg}).

%% gen_statem Callbacks
%% ---------------
callback_mode() ->
    handle_event_function.

init(_Args) ->
    {ok, disconnected, #{subs => sets:new()}}.

handle_event(_EventType, {connect, Token}, disconnected, Data) ->
    Timeout = 2000,
    logger:debug("[discord:gateway] Connecting to the server...~n"),

    {ok, ConnPid} = gun:open("gateway.discord.gg", 443, #{protocols => [http], transport => tls}),
    {ok, _} = gun:await_up(ConnPid, Timeout),

    logger:debug("[discord:gateway] Connected to Gateway, now upgrading to WS..."),
    gun:ws_upgrade(ConnPid, "/", [
        {<<"Authorization">>, "Bot " ++ Token}
    ]),
    receive
        {gun_upgrade, ConnPid, _StreamRef, [<<"websocket">>], _} ->
            logger:debug("[discord:gateway] WS upgrade successful"),
            {next_state, connecting, maps:merge(Data, #{token => Token, conn => ConnPid})};
        Other ->
            logger:warn("[discord:gateway] WS upgrade failed ~w~n", [Other]),
            {keep_state, Data}
    after Timeout ->
        logger:warn("[discord:gateway] WS upgrade timed out after ~d milliseconds~n", [Timeout]),
        {keep_state, Data}
    end;

handle_event(_EventType, {gun_ws, _ConnPid, _StreamRef, Frame}, _State, Data) ->
    case parse_frame(Frame) of
        {ok, Parsed, _} ->
            gen_server:cast(?SERVER_NAME, {ws_message, Parsed});
        {error, Reason} ->
            logger:error("[discord:gateway] Failed to parse frame ~w", [Reason]);
        Other ->
            logger:error("[discord:gateway] Unexpected frame format ~w", [Other])
    end,
    {keep_state, Data};

handle_event(_EventType, {ws_message, Message}, connecting, Data) ->
    case Message of
        % connected to server, initiate heartbeat and send identity
        #{op := ?OP_HELLO, d := #{heartbeat_interval := HeartbeatInterval}} ->
            logger:debug("[discord:gateway] Will send heartbeat every ~w milliseconds~n", [HeartbeatInterval]),

            Heartbeat = #{interval => HeartbeatInterval, seq_no => 0, timer => none},
            gen_statem:cast(?SERVER_NAME, {heartbeat}),

            #{token := Token, conn := ConnPid} = Data,
            {_, OsName} = os:type(),
            Identity = #{
                <<"token">> => list_to_binary(Token),
                <<"properties">> => #{
                    <<"$os">> => atom_to_binary(OsName, utf8),
                    <<"$browser">> => <<"pollsterl">>,
                    <<"$device">> => <<"pollsterl">>
                }
            },
            Frame = jsone:encode(#{<<"op">> => ?OP_IDENTIFY, <<"d">> => Identity}),
            gun:ws_send(ConnPid, {text, Frame}),

            NewData = maps:merge(Data, #{heartbeat => Heartbeat}),
            {keep_state, NewData};

        #{op := ?OP_DISPATCH, t := ?EVENT_READY, d := #{user := UserInfo} = DispatchData} ->
            #{id := BotId, username := BotName} = UserInfo,
            logger:debug("[discord:gateway] Bot is now authenticated as ~s (id: ~w)", [BotName, BotId]),

            emit({bot_id, BotId}),

            NewData = maps:merge(Data, #{bot => DispatchData, guilds => []}),
            {next_state, connected, NewData};

        _ ->
            {keep_state, Data}
    end;

handle_event(_EventType, {ws_message, Message}, connected, Data = #{guilds := GuildsBefore}) ->
    case Message of
        #{op := ?OP_DISPATCH, t := ?EVENT_GUILD_CREATE, d := GuildData} ->
            #{name := GuildName} = GuildData,
            logger:debug("[discord:gateway] Discovered guild ~s", [GuildName]),
            Guilds = GuildsBefore ++ GuildData,
            NewData = maps:merge(Data, #{guilds => Guilds}),
            {keep_state, NewData};

        #{op := ?OP_DISPATCH, t := EventName, d := EventData} ->
            emit({discord_dispatch, EventName, EventData}),
            {keep_state, Data};

        _ ->
            {keep_state, Data}
    end;

handle_event(_EventType, {heartbeat}, _State, Data = #{conn := ConnPid, heartbeat := Heartbeat}) ->
    #{timer := OldTimer, interval := Interval, seq_no := SeqNo} = Heartbeat,
    logger:debug("[discord:gateway] Sending heartbeat #~w~n", [SeqNo]),

    spawn(fun() -> timer:cancel(OldTimer) end),

    Frame = jsone:encode(#{<<"op">> => ?OP_HEARTBEAT, <<"d">> => SeqNo}),
    gun:ws_send(ConnPid, {text, Frame}),
    
    NewData = maps:merge(Data, #{heartbeat => #{
        interval => Interval,
        timer => erlang:send_after(Interval, self(), {heartbeat}),
        seq_no => SeqNo
    }}),

    {keep_state, NewData};

handle_event(_EventType, {subscribe, Pid}, _State, Data = #{subs := Subs}) ->
    NewSubs = sets:add_element(Pid, Subs),
    NewData = maps:merge(Data, #{subs => NewSubs}),
    spawn(fun() ->
        erlang:monitor(process, Pid),
        receive
            {'DOWN', _MonitorRef, process, _, _} ->
                logger:debug("[discord:gateway] Subscriber ~w is down, removing it from subscribers", [Pid]),
                unsubscribe(Pid)
        end
    end),
    logger:debug("[discord:gateway] Added ~w to the subscriber list", [Pid]),
    {keep_state, NewData};

handle_event(_EventType, {unsubscribe, Pid}, _State, Data = #{subs := Subs}) ->
    NewSubs = sets:del_element(Pid, Subs),
    NewData = maps:merge(Data, #{subs => NewSubs}),
    logger:debug("[discord:gateway] Removed ~w from the subscriber list", [Pid]),
    {keep_state, NewData};

handle_event(_EventType, {emit, Msg}, _State, Data = #{subs := Subs}) ->
    spawn(fun() ->
        sets:fold(fun(Sub, _) -> Sub ! Msg, {} end, {}, Subs)
    end),
    {keep_state, Data};

handle_event(_EventType, EventContent, State, Data) ->
    logger:debug("[discord:gateway] Received unexpected event ~w while in state ~w with data ~w", [EventContent, State, Data]),
    {keep_state, Data}.
