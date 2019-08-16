-module(pollsterl_chatter).
-include("emoji.hrl").
-export([start_link/0, listen/0]).

-define(BASIC_POLL_OPTIONS, [
    #{<<"key">> => emoji:key_of(?EMOJI_THUMBS_UP), <<"emoji">> => ?EMOJI_THUMBS_UP, <<"label">> => "Yes/Agree"},
    #{<<"key">> => emoji:key_of(?EMOJI_THUMBS_DOWN), <<"emoji">> => ?EMOJI_THUMBS_DOWN, <<"label">> => "No/Disagree"},
    #{<<"key">> => emoji:key_of(?EMOJI_SHRUG), <<"emoji">> => ?EMOJI_SHRUG, <<"label">> => "Maybe/Undecided"}
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
                                    N when N < 11 -> lists:sublist(?EMOJIS_DIGITS, N);
                                    N -> lists:sublist(?EMOJIS_ALPHANUM, N)
                                end,
                            [#{<<"key">> => emoji:key_of(Emoji), <<"emoji">> => Emoji, <<"label">> => Label} || {Label, Emoji} <- lists:zip(LabelList, EmojiList)]
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
