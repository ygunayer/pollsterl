-module(emoji).
-include("emoji.hrl").
-export([start_link/0, for_text/1, for_text/2, for_codepoint/1, for_codepoint/2, key_of/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-define(SERVER_NAME, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, {}, []).

for_text(Query) ->
    gen_server:call(?SERVER_NAME, {lookup, {text, exact}, Query}).
for_text(Query, invariant) ->
    gen_server:call(?SERVER_NAME, {lookup, {text, invariant}, Query}).

for_codepoint(Query) ->
    gen_server:call(?SERVER_NAME, {lookup, {codepoint, exact}, Query}).
for_codepoint(Query, invariant) ->
    gen_server:call(?SERVER_NAME, {lookup, {codepoint, invariant}, Query}).

key_of(#{<<"text">> := Key}) ->
    Key.

invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_UP_LIGHT_SKIN_TONE -> ?EMOJI_THUMBS_UP;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_UP_MEDIUM_LIGHT_SKIN_TONE -> ?EMOJI_THUMBS_UP;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_UP_MEDIUM_SKIN_TONE -> ?EMOJI_THUMBS_UP;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_UP_MEDIUM_DARK_SKIN_TONE -> ?EMOJI_THUMBS_UP;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_UP_DARK_SKIN_TONE -> ?EMOJI_THUMBS_UP;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_DOWN_LIGHT_SKIN_TONE -> ?EMOJI_THUMBS_DOWN;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_DOWN_MEDIUM_LIGHT_SKIN_TONE -> ?EMOJI_THUMBS_DOWN;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_DOWN_MEDIUM_SKIN_TONE -> ?EMOJI_THUMBS_DOWN;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_DOWN_MEDIUM_DARK_SKIN_TONE -> ?EMOJI_THUMBS_DOWN;
invariant_of(Emoji) when Emoji =:= ?EMOJI_THUMBS_DOWN_DARK_SKIN_TONE -> ?EMOJI_THUMBS_DOWN;
invariant_of(Emoji) when Emoji =:= ?EMOJI_SHRUG_LIGHT_SKIN_TONE -> ?EMOJI_SHRUG;
invariant_of(Emoji) when Emoji =:= ?EMOJI_SHRUG_MEDIUM_LIGHT_SKIN_TONE -> ?EMOJI_SHRUG;
invariant_of(Emoji) when Emoji =:= ?EMOJI_SHRUG_MEDIUM_SKIN_TONE -> ?EMOJI_SHRUG;
invariant_of(Emoji) when Emoji =:= ?EMOJI_SHRUG_MEDIUM_DARK_SKIN_TONE -> ?EMOJI_SHRUG;
invariant_of(Emoji) when Emoji =:= ?EMOJI_SHRUG_DARK_SKIN_TONE -> ?EMOJI_SHRUG;
invariant_of(Emoji) -> Emoji.

init(_Args) ->
    {ByText, ByCodepoint} = lists:foldl(
        fun(Emoji, {ByText, ByCodepoint}) ->
            #{<<"text">> := Text, <<"codepoint">> := Codepoint} = Emoji,
            {
                maps:put(Text, Emoji, ByText),
                maps:put(Codepoint, Emoji, ByCodepoint)
            }
        end,
        {maps:new(), maps:new()},
        ?EMOJIS_ALL
    ),
    logger:debug("[emoji] Built emoji database"),
    {ok, {ready, {ByText, ByCodepoint}}}.

handle_call({lookup, {LookupType, Variance}, Query}, _From, {ready, {ByText, ByCodepoint}} = State) ->
    Map = case LookupType of
        text -> ByText;
        codepoint -> ByCodepoint;
        _ -> none
    end,
    case Map of
        none -> {reply, {invalid_lookup, LookupType}, State};
        _ ->
            case maps:is_key(Query, Map) of
                true ->
                    Emoji = maps:get(Query, Map),
                    case Variance of
                        exact -> {reply, {ok, Emoji}, State};
                        invariant -> {reply, {ok, invariant_of(Emoji)}, State};
                        _ -> {reply, {invalid_variance, Variance}}
                    end;
                _ ->
                    {reply, {not_found}, State}
            end
    end;
handle_call(_Request, _From, _State) ->
    {reply, {ok}}.

handle_cast(_Message, State) ->
    {noreply, State}.
