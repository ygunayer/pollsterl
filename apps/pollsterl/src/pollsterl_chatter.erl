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
            case re:run(Content, "^\!poll(sterl|ster)?\s+(?<capture>.*+)", [{capture, all_names}]) of
                {match, [{Begin, Length}]} ->
                    Subject = string:slice(Content, Begin, Length),
                    logger:debug("[pollsterl] Initiating poll upon request by ~s", [Author]),
                    Reply = erlang:list_to_binary([
                        <<"selamin aleykum ">>,
                        Author,
                        <<" \"">>,
                        Subject,
                        <<"\" diye anket istemissin ama az bekle aq daha o kadar yazilmadi">>
                    ]),
                    discord_rest:send_message(binary_to_list(ChannelId), #{<<"content">> => Reply});
                _ -> {}
            end
    end,
    loop(BotId).
