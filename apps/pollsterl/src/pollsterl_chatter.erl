-module(pollsterl_chatter).
-export([start_link/0, init/0, loop/1]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Token} = application:get_env(pollsterl, bot_token),
    spawn(fun() -> discord_rest:set_token(Token) end),
    spawn(fun() -> discord_gateway:open(Token, Pid) end),
    {ok, Pid}.

init() ->
    receive
        {bot_id, BotId} -> loop(BotId);
        _ -> init()
    end.


loop(BotId) ->
    receive
        {event, <<"MESSAGE_CREATE">>, #{
            content := Content,
            channel_id := ChannelId,
            author := #{id := AuthorId, username := Author}
        }} when AuthorId =/= BotId ->
            case util:extract_command(binary:bin_to_list(Content)) of
                {ok, Command} ->
                    case Command of
                        {start, Subject, Options} ->
                            Reply = case Options of
                                basic ->
                                    erlang:list_to_binary([
                                        Author,
                                        <<" has started a basic poll for ">>,
                                        Subject
                                    ]);
                                Other ->
                                    erlang:list_to_binary([
                                        Author,
                                        <<" has started a poll for ">>,
                                        Subject,
                                        <<" with options ">>,
                                        lists:join(", ", Other)
                                    ])
                            end,
                            discord_rest:send_message(binary_to_list(ChannelId), #{<<"content">> => Reply});

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
                            discord_rest:send_message(binary_to_list(ChannelId), #{<<"content">> => Reply});

                        Other ->
                            logger:info("[pollsterl] Received command ~w", [Other]);
                        _ -> {}
                    end;
                Other ->
                    logger:info("[pollsterl] Command not recognized ~w", [Other]);
                _ -> {}
            end
    end,
    loop(BotId).
