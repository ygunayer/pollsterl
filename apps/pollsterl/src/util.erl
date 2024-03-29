-module(util).
-export([parse_json/1, parse_args/1, parse_command/1, parse_message/1, extract_command/1, join/1, join/2, random_string/1, random_string/2, to_binary/1, make_render_safe/1]).

random_string(Length) ->
    random_string(Length, "abcdefghijklmnopqrstuvwxyz1234567890").

random_string(Length, AllowedChars) ->
    lists:foldl(
        fun(_, Acc) ->
            [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
        end,
        [],
        lists:seq(1, Length)
    ).

join(Items) ->
    join(Items, "").

join(Items, Separator) ->
    NormalizedItems = [
        case is_binary(I) of
            true -> binary_to_list(I);
            false -> I
        end
        || I <- Items
    ],
    lists:join(Separator, NormalizedItems).

parse_json(Json) ->
    jsone:try_decode(Json, [{object_format, map}, {keys, atom}]).

parse_args(String) ->
    CleanString = string:trim(String),
    case re:run(CleanString, "([^\\s\\\"]+|\\\"[^\\\"]+\\\")", [global, {capture, all_but_first}]) of
        {match, Matches} ->
            lists:foldl(
                fun({Begin, Length}, Acc) ->
                    Term = string:slice(CleanString, Begin, Length),
                    Trimmed = string:trim(string:trim(Term, both, "\"")),
                    case string:is_empty(Trimmed) of
                        true -> Acc;
                        false -> Acc ++ [Trimmed]
                    end
                end,
                [],
                lists:flatten(Matches)
            );
        _ -> []
    end.

parse_command(Message) ->
    case re:run(Message, "^!(?<command>\\w+)", [{capture, all_names}]) of
        {match, [{Begin, Length}]} ->
            Verb = string:slice(Message, Begin, Length),
            Args = parse_args(string:slice(Message, Length + 1)),
            logger:debug("[util:parse_command] ~w ~w", [Verb, Args]),
            {command, Verb, Args};
        nomatch ->
            case string:find(Message, "\"") of
                nomatch ->
                    {no_command, [Message]};
                _ ->
                    Args = parse_args(Message),
                    {no_command, Args}
            end
    end.

parse_message(Message) ->
    MsgString = util:join([Message]),
    case re:run(MsgString, "^!poll(sterl|ster)?\s+(?<capture>.*+)", [{capture, all_names}]) of
        {match, [{Begin, Length}]} ->
            Subject = string:slice(MsgString, Begin, Length),
            parse_command(Subject);
        _ ->
            {ignore}
    end.

extract_command(Message) ->
    case parse_message(Message) of
        {command, "help", Topic} -> {ok, {help, Topic}};
        {command, "info", Topic} -> {ok, {help, Topic}};

        {command, "start", []} -> {error, not_enough_args, start};
        {command, "start", [Subject]} -> {ok, {start, Subject, basic}};
        {command, "start", [Subject | Options]} when length(Options) < 37 -> {ok, {start, Subject, Options}};
        {no_command, [Subject]} -> {ok, {start, Subject, basic}};
        {no_command, [Subject | Options]} when length(Options) < 37 -> {ok, {start, Subject, Options}};

        {command, "close", []} -> {ok, {close, last}};
        {command, "close", ["here"]} -> {ok, {close, here}};
        {command, "close", ["last"]} -> {ok, {close, last}};
        {command, "close", ["all"]} -> {ok, {close, all}};
        {command, "close", Ids} -> {ok, {close, Ids}};

        {command, "expire", []} -> {error, not_enough_args, start};
        {command, "expire", _Args} -> {error, not_implemented, expire};

        {command, Cmd, _} -> {error, unknown_command, Cmd};

        _ -> none
    end.

to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_atom(X) == true -> erlang:atom_to_binary(X, utf8);
to_binary(X) when is_binary(X) == true -> X.

make_render_safe(Map) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            NewValue = case erlang:is_map(Value) of
                true -> make_render_safe(Value);
                _ -> Value
            end,
            NewKey = to_binary(Key),
            maps:put(NewKey, NewValue, Acc)
        end,
        maps:new(),
        Map
    ).
