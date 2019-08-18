-module(pollsterl_poll_handler).
-export([start_link/1, start/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Public API
%% ---------------
start_link(Parent) ->
    gen_server:start_link(?MODULE, {Parent}, []).

init({Parent}) ->
    {ok, {initializing, #{parent => Parent, votes => maps:new()}}}.

start(Pid, PollData) ->
    gen_server:call(Pid, {start, PollData}).

%% Callback Implementation
%% ---------------
handle_call({start, PollData}, _From, {initializing, StateData}) ->
    #{subject := Subject, author := Author, options := Options, channel_id := ChannelId} = PollData,
    pollsterl_command_relay:subscribe(self()),

    Votes = ballot_box:new(Options),

    Id = util:random_string(8),
    Reply = pollsterl_templates:render("poll_start", #{subject => Subject, author => Author, options => Options, poll_id => Id}),
    {ok, Response} = discord_rest:channel_send_message(ChannelId, #{<<"content">> => Reply}),
    #{id := ReplyId} = Response,

    logger:debug("[poll:handler:~s] Started handler for poll ~s, will watch message ~s for reactions", [Id, Id, ReplyId]),

    NewStateData = maps:merge(StateData, #{pid => self(), id => Id, reply_id => ReplyId, poll => PollData, votes => Votes}),
    {reply, {ok, Id}, {watching, NewStateData}};
handle_call({close, CloseType}, _From, {watching, StateData}) ->
    #{pid := Pid, id := Id, poll := PollData, votes := Votes} = StateData,
    #{subject := Subject, author := Author, options := Options, channel_id := ChannelId} = PollData,

    Result = ballot_box:count_votes(Votes),

    case CloseType of
        normal ->
            DefaultStats = #{
                no_winner => false,
                one_winner => false,
                draw => false
            },
            ActualStats =
                case Result of
                    {none} ->
                        #{no_winner => true};
                    {winner, {MaxCount, TotalCount, #{option := Option}}} ->
                        #{one_winner => true, max_count => MaxCount, total_count => TotalCount, winner => Option};
                    {draw, {MaxCount, TotalCount, Winners}} ->
                        WinningOptions = [Option || #{option := Option} <- Winners],
                        #{max_count => MaxCount, total_count => TotalCount, draw => true, winner_count => length(Winners), winners => WinningOptions}
                end,
            Stats = maps:merge(DefaultStats, ActualStats),
            ReplyData = maps:merge(
                #{subject => Subject,author => Author,options => Options,poll_id => Id},
                Stats
            ),
            Reply = pollsterl_templates:render("poll_close", ReplyData),
            {ok, _} = discord_rest:channel_send_message(ChannelId, #{<<"content">> => Reply});

        _ -> noop
    end,

    logger:debug("[poll:handler:~s] Poll ~s is now closed", [Id]),
    spawn(fun() -> pollsterl_poll_sup:stop_poll(Pid) end),
    {reply, {ok, Result}, {closed, StateData}};
handle_call(_Request, _From, _State) ->
    {reply, {ok}}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({close_poll, Id, CloseType, _Message}, State = {_, #{pid := Pid, id := PollId}}) when Id == PollId ->
    spawn(fun() -> gen_server:call(Pid, {close, CloseType}) end),
    {noreply, State};
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
    case ballot_box:cast(Key, AuthorId, Votes) of
        {ok, NextVotes} ->
            logger:debug("[poll:handler:~s] A ~s vote was cast for poll ~s", [PollId, Key, PollId]),
            NewState = {StateName, maps:merge(StateData, #{votes => NextVotes})},
            {noreply, NewState};
        false ->
            logger:debug("[poll:handler:~s] ~s is not a valid option for poll ~s and will be ignored", [PollId, Key, PollId]),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.
