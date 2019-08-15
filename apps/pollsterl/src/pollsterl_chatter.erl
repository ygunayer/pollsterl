-module(pollsterl_chatter).
-export([start_link/0, listen/0]).

-define(BASIC_POLL_OPTIONS, [
    #{<<"emoji">> => ":thumbsup:", <<"label">> => "Yes/Agree"},
    #{<<"emoji">> => ":thumbsdown:", <<"label">> => "No/Disagree"},
    #{<<"emoji">> => ":shrug:", <<"label">> => "Maybe/Undecided"}
]).

-define(EMOJI_DIGITS, [
    ":one:",
    ":two:",
    ":three:",
    ":four:",
    ":five:",
    ":six:",
    ":seven:",
    ":eight:",
    ":nine:",
    ":keycap_ten:"
]).

-define(EMOJI_LETTERS, [
    ":regional_indicator_a:",
    ":regional_indicator_b:",
    ":regional_indicator_c:",
    ":regional_indicator_d:",
    ":regional_indicator_e:",
    ":regional_indicator_f:",
    ":regional_indicator_g:",
    ":regional_indicator_i:",
    ":regional_indicator_j:",
    ":regional_indicator_k:",
    ":regional_indicator_l:",
    ":regional_indicator_m:",
    ":regional_indicator_n:",
    ":regional_indicator_o:",
    ":regional_indicator_p:",
    ":regional_indicator_q:",
    ":regional_indicator_r:",
    ":regional_indicator_s:",
    ":regional_indicator_t:",
    ":regional_indicator_u:",
    ":regional_indicator_v:",
    ":regional_indicator_w:",
    ":regional_indicator_x:",
    ":regional_indicator_y:",
    ":regional_indicator_z:"
]).

-define(EMOJI_ALL, lists:concat(?EMOJI_DIGITS, ?EMOJI_LETTERS)).

start_link() ->
    Pid = spawn_link(?MODULE, listen, []),
    spawn(fun() -> timer:sleep(2000), pollsterl_command_relay:subscribe(Pid) end),
    {ok, Pid}.

listen() ->
    receive
        {command, Command, Message =#{
            channel_id := ChannelId,
            author := #{username := Author}
        }} ->
            case Command of
                {help, Topic} ->
                    TemplateName = case Topic of
                        ["!start"] -> "help_start";
                        ["!stop"] -> "help_stop";
                        ["!expire"] -> "help_expire";
                        _ -> "help"
                    end,
                    Reply = pollsterl_templates:render(TemplateName, #{}),
                    discord_rest:channel_send_message(ChannelId, #{<<"content">> => Reply});

                {start, Subject, OptionType} ->
                    #{id := MessageId} = Message,
                    Options = case OptionType of
                        basic ->
                            ?BASIC_POLL_OPTIONS;
                        LabelList ->
                            EmojiList =
                                case length(LabelList) of
                                    N when N < 11 -> lists:sublist(?EMOJI_DIGITS, N);
                                    N -> lists:sublist(?EMOJI_ALL, N)
                                end,
                            [#{<<"emoji">> => Emoji, <<"label">> => Label} || {Label, Emoji} <- lists:zip(LabelList, EmojiList)]
                    end,
                    {ok, Pid} = pollsterl_poll_sup:start_poll(),
                    {ok, _} = pollsterl_poll_handler:start(Pid, #{original_message_id => MessageId, subject => Subject, author => Author, options => Options, channel_id => ChannelId});

                {stop, Polls} ->
                    PollNames = case Polls of
                        all -> <<"all polls">>;
                        last -> <<"the last poll">>;
                        Other -> list_to_binary([<<"polls ">>, lists:join(", ", Other)])
                    end,
                    Reply = erlang:list_to_binary([
                        Author,
                        <<" is stopping ">>,
                        PollNames,
                        <<" in this channel">>
                    ]),
                    discord_rest:channel_send_message(binary_to_list(ChannelId), #{<<"content">> => Reply});

                _ -> noop
            end;
        _ -> noop
    end,
    listen().
