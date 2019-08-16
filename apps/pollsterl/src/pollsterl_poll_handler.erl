-module(pollsterl_poll_handler).
-export([start_link/1, start/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Parent) ->
    gen_server:start_link(?MODULE, {Parent}, []).

init({Parent}) ->
    {ok, {initializing, #{parent => Parent, votes => maps:new()}}}.

start(Pid, PollData) ->
    gen_server:call(Pid, {start, PollData}).

handle_call({start, PollData}, _From, {initializing, State}) ->
    #{subject := Subject, author := Author, options := Options, channel_id := ChannelId} = PollData,
    pollsterl_command_relay:subscribe(self()),

    Votes = lists:foldl(
        fun(#{<<"key">> := Key}, Map) ->
            maps:put(Key, sets:new(), Map)
        end,
        maps:new(),
    Options),

    Id = util:random_string(8),
    Reply = pollsterl_templates:render("poll_start", #{<<"subject">> => Subject, <<"author">> => Author, <<"options">> => Options, <<"poll_id">> => Id}),
    {ok, Response} = discord_rest:channel_send_message(ChannelId, #{<<"content">> => Reply}),
    #{id := ReplyId} = Response,

    logger:debug("[poll:handler:~s] Started handler for poll #~s, will watch message ~s for reactions", [Id, Id, ReplyId]),

    NewState = maps:merge(State, #{id => Id, reply_id => ReplyId, poll => PollData, votes => Votes}),
    {reply, {ok, Id}, {watching, NewState}};
handle_call(Request, _From, _State) ->
    io:format("sie aq ~w", [Request]),
    {reply, {sie}}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(
    {
        command,
        {cast_vote, #{author_id := AuthorId, message_id := MessageId, option := Option}},
        _EventData
    },
    State = {
        StateName,
        StateData = #{id := PollId, reply_id := ReplyId, votes := Votes}
    }
) when MessageId == ReplyId ->
    Key = emoji:key_of(Option),
    case maps:is_key(Key, Votes) of
        true ->
            NextVotes = maps:update_with(Key, fun(Set) -> sets:add_element(AuthorId, Set) end, Votes),
            NewState = {StateName, maps:merge(StateData, #{votes => NextVotes})},
            CurrentCount = sets:size(maps:get(Key, NextVotes)),
            logger:debug("[poll:handler:~s] A ~s vote was cast for poll ~s, it now has ~w votes for that option", [PollId, Key, PollId, CurrentCount]),
            {noreply, NewState};
        false ->
            logger:debug("[poll:handler:~s] ~s is not a valid option for poll ~s and will be ignored", [PollId, Key, PollId]),
            {noreply, State}
    end;
handle_info(Info, State = {_, #{id := PollId}}) ->
    logger:debug("[poll:handler:~s] INFO ~w IN STATE ~w", [PollId, Info, State]),
    {noreply, State}.
