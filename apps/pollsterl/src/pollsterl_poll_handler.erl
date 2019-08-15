-module(pollsterl_poll_handler).
-export([start_link/1, start/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Parent) ->
    gen_server:start_link(?MODULE, {Parent}, []).

init({Parent}) ->
    {ok, {initializing, #{parent => Parent}}}.

start(Pid, PollData) ->
    gen_server:call(Pid, {start, PollData}).

handle_call({start, PollData}, _From, {initializing, State}) ->
    #{subject := Subject, author := Author, options := Options, channel_id := ChannelId} = PollData,
    pollsterl_command_relay:subscribe(self()),

    Id = util:random_string(8),
    Reply = pollsterl_templates:render("poll_start", #{<<"subject">> => Subject, <<"author">> => Author, <<"options">> => Options, <<"poll_id">> => Id}),
    {ok, Response} = discord_rest:channel_send_message(ChannelId, #{<<"content">> => Reply}),
    #{id := ReplyId} = Response,

    logger:debug("[poll:handler:~s] Started handler for poll #~s, will watch message ~s for reactions", [Id, Id, ReplyId]),

    NewState = maps:merge(State, #{id => Id, reply_id => ReplyId, poll => PollData}),
    {reply, {ok, Id}, {watching, NewState}};
handle_call(Request, _From, _State) ->
    io:format("sie aq ~w", [Request]),
    {reply, {sie}}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({command, {cast_vote, MessageId, Option}, _EventData}, State = {_, #{id := PollId, reply_id := ReplyId}}) when MessageId == ReplyId ->
    logger:debug("[poll:handler:~s] A vote was cast to poll ~s: ~s", [PollId, Option]),
    {noreply, State};
handle_info(Info, State = {_, #{id := PollId}}) ->
    logger:debug("[poll:handler:~s] INFO ~w IN STATE ~w", [PollId, Info, State]),
    {noreply, State}.
