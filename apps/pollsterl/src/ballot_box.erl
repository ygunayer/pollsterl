-module(ballot_box).
-export([new/1, cast/3, remove/3, count_of/2, count_votes/1]).

new(Options) ->
    lists:foldl(
        fun(Option, Acc) ->
            #{<<"key">> := Key} = Option,
            maps:put(Key, #{
                count => 0,
                option => Option,
                votes => sets:new()
            }, Acc)
        end,
        maps:new(),
        Options
    ).

cast(Key, Voter, Box) ->
    case maps:is_key(Key, Box) of
        true ->
            Entry = maps:get(Key, Box),
            #{votes := OldVotes} = Entry,
            NewVotes = sets:add_element(Voter, OldVotes),
            NewEntry = maps:merge(Entry, #{count => sets:size(NewVotes), votes => NewVotes}),
            NewBox = maps:put(Key, NewEntry, Box),
            {ok, NewBox};
        false -> false
    end.

remove(Key, Voter, Box) ->
    case maps:is_key(Key, Box) of
        true ->
            Entry = maps:get(Key, Box),
            #{votes := OldVotes} = Entry,
            NewVotes = sets:del_element(Voter, OldVotes),
            NewEntry = maps:merge(Entry, #{count => sets:size(NewVotes), votes => NewVotes}),
            NewBox = maps:put(Key, NewEntry, Box),
            {ok, NewBox};
        false -> false
    end.

count_of(Key, Box) ->
    case maps:is_key(Key, Box) of
        true ->
            #{count := Count} = maps:get(Key, Box),
            {ok, Count};
        false -> false
    end.

count_votes(Box) ->
    {MaxCount, TotalCount, Options} =
        maps:fold(
            fun(_Key, Option = #{count := Count}, {Max, Total, Opts}) ->
                case Count of
                    0 -> {Max, Total, Opts};
                    N when N == Max ->
                        {Max, Total + N, Opts ++ [Option]};
                    N when N > Max ->
                        {N, Total + N, [Option]};
                    N ->
                        {Max, Total + N, Opts}
                end
            end,
            {0, 0, []},
            Box
        ),
    case length(Options) of
        0 -> {none};
        1 ->
            [Option] = Options,
            {winner, {MaxCount, TotalCount, Option}};
        _ ->
            {draw, {MaxCount, TotalCount, Options}}
    end.
