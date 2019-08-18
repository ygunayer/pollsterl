-module(poll_cache_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    [] = poll_cache:new(),
    ok.

add_test() ->
    [{foo, bar}] = poll_cache:add(foo, bar, []),
    [{foo, bar}] = poll_cache:add(foo, bar, [{foo, bar}]),
    [{foo, bar}, {bar, baz}] = poll_cache:add(foo, bar, [{bar, baz}, {foo, bar}]),
    ok.

remove_test() ->
    [] = poll_cache:remove(foo, []),
    [] = poll_cache:remove(foo, [{foo, bar}]),
    [{foo, bar}] = poll_cache:remove(bar, [{foo, bar}]),
    [{foo, bar}, {baz, quux}] = poll_cache:remove(bar, [{foo, bar}, {bar, baz}, {baz, quux}]),
    ok.

remove_many_test() ->
    [] = poll_cache:remove_many([], []),
    [] = poll_cache:remove_many([foo], []),
    [{foo, bar}] = poll_cache:remove_many([bar, baz], [{foo, bar}, {bar, baz}, {baz, quux}]),
    ok.

clear_channel_test() ->
    [] = poll_cache:clear_channel(foo, []),
    [{foo, bar}] = poll_cache:clear_channel(foo, [{foo, bar}]),
    [] = poll_cache:clear_channel(bar, [{foo, bar}]),
    [{bar, quux}] = poll_cache:clear_channel(bar, [{foo, bar}, {baz, bar}, {bar, quux}]),
    ok.

find_test() ->
    [] = poll_cache:find(all, []),
    [foo] = poll_cache:find(all, [{foo, bar}]),
    [foo, bar] = poll_cache:find(all, [{foo, bar}, {bar, baz}]),

    [] = poll_cache:find({last, bar}, []),
    [foo] = poll_cache:find({last, bar}, [{foo, bar}]),
    [foo] = poll_cache:find({last, bar}, [{foo, bar}, {baz, bar}]),
    [] = poll_cache:find({last, baz}, [{foo, bar}, {baz, bar}]),

    [] = poll_cache:find({channel, bar}, []),
    [foo] = poll_cache:find({channel, bar}, [{foo, bar}]),
    [foo, baz] = poll_cache:find({channel, bar}, [{foo, bar}, {baz, bar}]),
    [] = poll_cache:find({channel, baz}, [{foo, bar}, {baz, bar}]),

    [] = poll_cache:find({one, foo}, []),
    [foo] = poll_cache:find({one, foo}, [{foo, bar}]),
    [bar] = poll_cache:find({one, bar}, [{foo, bar}, {bar, baz}]),

    [] = poll_cache:find({many, [foo]}, []),
    [foo] = poll_cache:find({many, [foo]}, [{foo, bar}]),
    [bar] = poll_cache:find({many, [bar]}, [{foo, bar}, {bar, baz}]),

    [] = poll_cache:find({many, [foo]}, []),
    [foo] = poll_cache:find({many, [foo, bar]}, [{foo, bar}]),
    [foo, bar] = poll_cache:find({many, [foo, bar]}, [{foo, bar}, {bar, baz}]),
    [bar, foo] = poll_cache:find({many, [bar, foo]}, [{foo, bar}, {bar, baz}]),
    [foo, bar] = poll_cache:find({many, [foo, bar]}, [{foo, bar}, {bar, baz}, {baz, quux}]),
    ok.
