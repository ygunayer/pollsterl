-module(util_tests).
-include_lib("eunit/include/eunit.hrl").

parse_args_test() ->
    [] = util:parse_args(""),
    [] = util:parse_args("  "),
    ["a"] = util:parse_args("a"),
    ["a"] = util:parse_args(" a "),
    ["a"] = util:parse_args("\"a\""),
    ["a", "b"] = util:parse_args("a b"),
    ["a b"] = util:parse_args("\"a b\""),
    ["a b", "c", "d"] = util:parse_args("\"a b\" c d"),
    ["a b", "c", "d", "e"] = util:parse_args("\"a b\" c d \"e\""),
    ok.

parse_command_test() ->
    {command, "foo", []} = util:parse_command("!foo"),
    {command, "foo", ["bar"]} = util:parse_command("!foo bar"),
    {command, "foo", ["bar", "baz"]} = util:parse_command("!foo bar \"baz\""),
    {command, "foo", ["bar baz", "quux"]} = util:parse_command("!foo \"bar baz\" quux"),
    {no_command, ["bar baz", "quux"]} = util:parse_command("\"bar baz\" quux"),
    {no_command, ["bar baz quux"]} = util:parse_command("bar baz quux"),
    ok.

parse_message_test() ->
    {ignore} = util:parse_message("foo"),
    {ignore} = util:parse_message(" !poll"),
    {command, "foo", []} = util:parse_message("!poll !foo"),
    {command, "foo", []} = util:parse_message("!pollster !foo"),
    {command, "foo", []} = util:parse_message("!pollsterl !foo"),
    {ignore} = util:parse_message("!pollsterlfoo !foo"),
    ok.

extract_command_test() ->
    % Help commands
    {ok, {help, []}} = util:extract_command("!poll !help"),
    {ok, {help, []}} = util:extract_command(<<"!poll !help">>),
    {ok, {help, []}} = util:extract_command("!poll !info"),
    {ok, {help, ["foo"]}} = util:extract_command("!poll !help foo"),

    % Start commands
    {ok, {start, "foo", basic}} = util:extract_command("!poll !start foo"),
    {ok, {start, "anyone up for some gr?", basic}} = util:extract_command("!poll anyone up for some gr?"),
    {ok, {
        start,
        "Has anyone really been far even as decided to use even go want to do look more like?",
        basic
    }} = util:extract_command("!poll \"Has anyone really been far even as decided to use even go want to do look more like?\""),
    {ok, {
        start,
        "Which starter pokemon is your favorite?",
        ["Charmander", "Bulbasaur", "Squirtle"]
    }} = util:extract_command("!poll \"Which starter pokemon is your favorite?\" Charmander Bulbasaur Squirtle"),
    {ok, {
        start,
        "Which faction do you prefer?",
        ["NCR", "Caesar's Legion", "Mr. House", "I go about my own way"]
    }} = util:extract_command("!poll \"Which faction do you prefer?\" NCR \"Caesar's Legion\" \"Mr. House\" \"I go about my own way\""),

    % Stop commands
    {ok, {stop, ["foo"]}} = util:extract_command("!poll !stop foo"),
    {ok, {stop, ["foo", "bar"]}} = util:extract_command("!poll !stop foo bar"),
    {ok, {stop, all}} = util:extract_command("!poll !stop all"),
    {ok, {stop, last}} = util:extract_command("!poll !stop last"),
    {ok, {stop, last}} = util:extract_command("!poll !stop"),

    % Expire commands
    % NOT YET IMPLEMENTED
    %{ok, {expire, "foo", {relative, 3600}}} = util:extract_command("!poll !expire foo 1 hour"),
    %{ok, {expire, "foo", {relative, 86400}}} = util:extract_command("!poll !expire foo tomorrow"),
    %{ok, {expire, "foo", {exact, "2019-08-16T13:44:51+0300"}}} = util:extract_command("!poll !expire <poll id> \"2019-08-16T13:44:51+0300\""),
    %{ok, {expire, last, {relative, 3600}}} = none = util:extract_command("!poll !expire 1 hour"),
    %{ok, {expire, last, {relative, 3600}}} = none = util:extract_command("!poll !expire last 1 hour"),
    ok.
