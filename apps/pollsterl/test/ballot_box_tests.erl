-module(ballot_box_tests).
-include("emoji.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(KU, emoji:key_of(?EMOJI_THUMBS_UP)).
-define(KD, emoji:key_of(?EMOJI_THUMBS_DOWN)).

-define(TU, #{<<"key">> => ?KU, <<"emoji">> => ?EMOJI_THUMBS_UP, <<"label">> => "Yes/Agree"}).
-define(TD, #{<<"key">> => ?KD, <<"emoji">> => ?EMOJI_THUMBS_DOWN, <<"label">> => "Yes/Agree"}).

-define(OPTIONS, [?TU, ?TD]).

new_test() ->
    Box = ballot_box:new(?OPTIONS),
    {ok, 0} = ballot_box:count_of(?KU, Box),
    {ok, 0} = ballot_box:count_of(?KD, Box),
    ok.

cast_test() ->
    B1 = ballot_box:new(?OPTIONS),
    {ok, B2} = ballot_box:cast(?KU, foo, B1),
    {ok, 1} = ballot_box:count_of(?KU, B2),
    {ok, 0} = ballot_box:count_of(?KD, B2),

    {ok, B3} = ballot_box:cast(?KU, foo, B2),
    {ok, 1} = ballot_box:count_of(?KU, B3),
    {ok, 0} = ballot_box:count_of(?KD, B3),

    {ok, B4} = ballot_box:cast(?KU, bar, B2),
    {ok, 2} = ballot_box:count_of(?KU, B4),
    {ok, 0} = ballot_box:count_of(?KD, B4),

    {ok, B5} = ballot_box:cast(?KD, foo, B2),
    {ok, 1} = ballot_box:count_of(?KU, B5),
    {ok, 1} = ballot_box:count_of(?KD, B5),
    ok.

remove_test() ->
    B1 = ballot_box:new(?OPTIONS),
    {ok, B2} = ballot_box:cast(?KU, foo, B1),
    {ok, 1} = ballot_box:count_of(?KU, B2),

    {ok, B3} = ballot_box:remove(?KU, foo, B2),
    {ok, 0} = ballot_box:count_of(?KU, B3),

    {ok, B4} = ballot_box:remove(?KU, bar, B2),
    {ok, 1} = ballot_box:count_of(?KU, B4),

    {ok, B5} = ballot_box:remove(?KD, foo, B2),
    {ok, 1} = ballot_box:count_of(?KU, B5),
    {ok, 0} = ballot_box:count_of(?KD, B5),
    ok.

count_votes_test() ->
    TU = ?TU,
    TD = ?TD,

    B1 = ballot_box:new(?OPTIONS),
    {none} = ballot_box:count_votes(B1),

    {ok, B2} = ballot_box:cast(?KU, foo, B1),
    {winner, {1, 1, #{option := TU}}} = ballot_box:count_votes(B2),

    {ok, B3} = ballot_box:cast(?KD, foo, B2),
    {draw, {1, 2, [#{option := TD}, #{option := TU}]}} = ballot_box:count_votes(B3),

    {ok, B4} = ballot_box:cast(?KU, baz, B3),
    {winner, {2, 3, #{option := TU}}} = ballot_box:count_votes(B4),
    ok.
