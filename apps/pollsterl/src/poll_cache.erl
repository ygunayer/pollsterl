-module(poll_cache).
-export([new/0, add/3, remove/2, remove_many/2, clear_channel/2, find/2]).

new() ->
    [].

add(PollId, ChannelId, Cache) ->
    % we could use keystore here but we'd like to retain the order of things
    CleanCache = lists:keydelete(PollId, 1, Cache),
    [{PollId, ChannelId} | CleanCache].

remove(PollId, Cache) ->
    lists:keydelete(PollId, 1, Cache).

remove_many(PollIds, Cache) ->
    % TODO improve performance of this
    lists:foldl(
        fun(Id, Acc) -> poll_cache:remove(Id, Acc) end,
        Cache,
        PollIds
    ).

clear_channel(ChannelId, Cache) ->
    lists:filter(
        fun({_PollId, ChanId}) -> ChanId =/= ChannelId end,
        Cache
    ).

find(all, Cache) ->
    [PollId || {PollId, _} <- Cache];
find({last, ChannelId}, Cache) ->
    case lists:keyfind(ChannelId, 2, Cache) of
        false -> [];
        {PollId, _} -> [PollId]
    end;
find({channel, ChannelId}, Cache) ->
    lists:foldl(
        fun({PollId, ChanId}, Acc) ->
            case ChanId =:= ChannelId of
                false -> Acc;
                true -> Acc ++ [PollId]
            end
        end,
        [],
        Cache
    );
find({many, PollIds}, Cache) ->
    lists:foldl(
        fun(Id, Acc) ->
            case lists:keymember(Id, 1, Cache) of
                false -> Acc;
                true -> Acc ++ [Id]
            end
        end,
        [],
        PollIds
    );
find({one, PollId}, Cache) ->
    case lists:keyfind(PollId, 1, Cache) of
        false -> [];
        {PollId, _} -> [PollId]
    end;
find(_Query, _Cache) ->
    [].
