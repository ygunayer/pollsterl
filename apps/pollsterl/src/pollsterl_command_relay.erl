-module(pollsterl_command_relay).
-export([start_link/0, subscribe/1, unsubscribe/1, emit/1]).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-define(SERVER_NAME, ?MODULE).

%% Public API
%% ---------------
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, {}, []).

subscribe(Pid) ->
    gen_server:cast(?SERVER_NAME, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:cast(?SERVER_NAME, {unsubscribe, Pid}).

emit(Msg) ->
    gen_server:cast(?SERVER_NAME, {emit, Msg}).

%% Callback implementations
%% ---------------
init(_Args) ->
    {ok, Token} = application:get_env(pollsterl, bot_token),
    discord_gateway:subscribe(self()),
    spawn(fun() -> discord_rest:set_token(Token) end),
    spawn(fun() -> discord_gateway:open(Token) end),
    {ok, {initializing, #{subs => sets:new()}}}.

handle_call(_Request, _From, State) ->
    {reply, {ok}, State}.

handle_cast({subscribe, Pid}, {State, Data = #{subs := Subs}}) ->
    NewSubs = sets:add_element(Pid, Subs),
    NewData = maps:merge(Data, #{subs => NewSubs}),
    spawn(fun() ->
        erlang:monitor(process, Pid),
        receive
            {'DOWN', _MonitorRef, process, _, _} ->
                logger:debug("[command-relay] Subscriber ~w is down, removing it from subscribers", [Pid]),
                unsubscribe(Pid)
        end
    end),
    logger:debug("[command-relay] Added ~w to the subscriber list", [Pid]),
    {noreply, {State, NewData}};
handle_cast({unsubscribe, Pid}, {State, Data = #{subs := Subs}}) ->
    NewSubs = sets:del_element(Pid, Subs),
    NewData = maps:merge(Data, #{subs => NewSubs}),
    logger:debug("[command-relay] Removed ~w from the subscriber list", [Pid]),
    {noreply, {State, NewData}};
handle_cast({emit, Msg}, {State, Data = #{subs := Subs}}) ->
    spawn(fun() ->
        sets:fold(fun(Sub, _) -> Sub ! Msg, {} end, {}, Subs)
    end),
    {noreply, {State, Data}};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({bot_id, BotId}, {initializing, Data}) ->
    NewData = maps:merge(#{bot_id => BotId}, Data),
    {noreply, {ready, NewData}};
handle_info({discord_dispatch, EventName, EventData}, State = {ready, #{bot_id := BotId}}) ->
    case {EventName, EventData} of
        {
            <<"MESSAGE_CREATE">>,
            #{content := Content, author := #{id := AuthorId}}
        } when AuthorId =/= BotId ->
            case util:extract_command(binary:bin_to_list(Content)) of
                {ok, Command} ->
                    emit({command, Command, EventData});
                Other ->
                    logger:debug("[command-relay] Command not recognized ~w", [Other])
            end;
        {
            <<"MESSAGE_REACTION_ADD">>,
            #{message_id := MessageId, emoji := #{name := EmojiName}, user_id := AuthorId}
        } when AuthorId =/= BotId ->
            case emoji:for_codepoint(EmojiName, invariant) of
                {ok, Option} ->
                    VoteInfo = #{author_id => AuthorId, message_id => MessageId, option => Option},
                    emit({command, {cast_vote, VoteInfo}, EventData});
                _ -> noop
            end;
        _ -> noop
    end,
    {noreply, State};
handle_info(Info, State) ->
    logger:debug("[command-relay] INFO ~w @ ~w", [Info, State]),
    {noreply, State}.
