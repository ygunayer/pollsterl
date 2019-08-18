-module(pollsterl_manager).
-include("emoji.hrl").
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-define(SERVER_NAME, ?MODULE).

-define(BASIC_POLL_OPTIONS, [
    #{<<"key">> => emoji:key_of(?EMOJI_THUMBS_UP), <<"emoji">> => ?EMOJI_THUMBS_UP, <<"label">> => "Yes/Agree"},
    #{<<"key">> => emoji:key_of(?EMOJI_THUMBS_DOWN), <<"emoji">> => ?EMOJI_THUMBS_DOWN, <<"label">> => "No/Disagree"},
    #{<<"key">> => emoji:key_of(?EMOJI_SHRUG), <<"emoji">> => ?EMOJI_SHRUG, <<"label">> => "Maybe/Undecided"}
]).

%% Public API
%% ---------------
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, {}, []).

init(_Args) ->
    spawn(fun() ->
        gen_server:cast(?SERVER_NAME, {start})
    end),
    {ok, {initializing}}.

%% Callback Implementation
%% ---------------
handle_call(_Request, _From, State) ->
    {reply, {ok}, State}.

handle_cast({start}, {initializing}) ->
    pollsterl_command_relay:subscribe(self()),
    {noreply, {running, #{polls => poll_cache:new()}}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({command, Command, Message}, State = {running, StateData}) ->
    #{polls := PollCache} = StateData,
    #{channel_id := ChannelId} = Message,
    case Command of
        {help, Topic} ->
            TemplateName = case Topic of
                ["!start"] -> "help_start";
                ["!close"] -> "help_close";
                ["!expire"] -> "help_expire";
                _ -> "help"
            end,
            Reply = pollsterl_templates:render(TemplateName, #{}),
            discord_rest:channel_send_message(ChannelId, #{<<"content">> => Reply}),
            {noreply, State};

        {start, Subject, OptionType} ->
            #{id := MessageId, author := #{username := Author}} = Message,
            Options = case OptionType of
                basic ->
                    ?BASIC_POLL_OPTIONS;
                LabelList ->
                    EmojiList =
                        case length(LabelList) of
                            N when N < 11 -> lists:sublist(?EMOJIS_DIGITS, N);
                            N -> lists:sublist(?EMOJIS_ALPHANUM, N)
                        end,
                    [#{<<"key">> => emoji:key_of(Emoji), <<"emoji">> => Emoji, <<"label">> => Label} || {Label, Emoji} <- lists:zip(LabelList, EmojiList)]
            end,
            {ok, Pid} = pollsterl_poll_sup:start_poll(),
            {ok, PollId} = pollsterl_poll_handler:start(Pid, #{original_message_id => MessageId, subject => Subject, author => Author, options => Options, channel_id => ChannelId}),
            NewPollCache = poll_cache:add(PollId, ChannelId, PollCache),
            NewStateData = maps:merge(StateData, #{polls => NewPollCache}),
            {noreply, {running, NewStateData}};

        {close, Polls} ->
            Query =
                case Polls of
                    last -> {last, ChannelId};
                    here -> {channel, ChannelId};
                    all -> all;
                    _ when length(Polls) > 1 -> {many, Polls};
                    _ -> {one, Polls}
                end,
            PollsToClose = poll_cache:find(Query, PollCache),
            CloseType = case length(PollsToClose) of
                N when N > 1 -> silent;
                _ -> normal
            end,
            % TODO improve performance
            NewCache = lists:foldl(
                fun(Id, Acc) ->
                    pollsterl_command_relay:emit({close_poll, Id, CloseType, Message}),
                    poll_cache:remove(Id, Acc)
                end,
                PollCache,
                PollsToClose
            ),
            logger:debug("[manager] Closing the following polls: ~w", [lists:join(", ", PollsToClose)]),
            NewStateData = maps:merge(StateData, #{polls => NewCache}),
            {noreply, {running, NewStateData}};

        _ ->
            {noreply, State}
    end;
handle_info(_Message, State) ->
    {noreply, State}.
