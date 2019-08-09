-module(util).
-export([parse_args/1, parse_command/1, parse_message/1, extract_command/1]).

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
    case re:run(Message, "^!poll(sterl|ster)?\s+(?<capture>.*+)", [{capture, all_names}]) of
        {match, [{Begin, Length}]} ->
            Subject = string:slice(Message, Begin, Length),
            parse_command(Subject);
        _ ->
            {ignore}
    end.

extract_command(Message) ->
    case parse_message(Message) of
        {command, "help", Topic} -> {ok, help, Topic};
        {command, "info", Topic} -> {ok, help, Topic};

        {command, "start", []} -> {error, not_enough_args, start};
        {command, "start", [Subject]} -> {ok, {start, Subject, basic}};
        {command, "start", [Subject | Options]} -> {ok, {start, Subject, Options}};
        {no_command, [Subject]} -> {ok, {start, Subject, basic}};
        {no_command, [Subject | Options]} -> {ok, {start, Subject, Options}};

        {command, "stop", []} -> {ok, {stop, last}};
        {command, "stop", ["last"]} -> {ok, {stop, last}};
        {command, "stop", ["all"]} -> {ok, {stop, all}};
        {command, "stop", Ids} -> {ok, {stop, Ids}};

        {command, "expire", []} -> {error, not_enough_args, start};
        {command, "expire", _Args} -> {error, not_implemented, expire};

        {command, Cmd, _} -> {error, unknown_command, Cmd};

        _ -> none
    end.
