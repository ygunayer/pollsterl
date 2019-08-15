-module(pollsterl_poll).
-export([start_link/1, start/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Parent) ->
    gen_server:start_link(?MODULE, {Parent}, []).

init({Parent}) ->
    {ok, {initializing, #{parent => Parent}}}.

start(Pid, {ChannelId, Author, Subject, Options}) ->
    Id = util:random_string(8),
    PollData = #{id => Id, channel_id => ChannelId, author => Author, subject => Subject, options => Options},
    gen_server:call(Pid, {start, PollData}).

handle_call({start, PollData}, _From, {initializing, State}) ->
    #{id := Id, subject := Subject, author := Author, options := Options, channel_id := ChannelId} = PollData,
    discord_gateway:subscribe(self()),
    
    Reply = message_builder:render("poll_start", #{<<"subject">> => Subject, <<"author">> => Author, <<"options">> => Options, <<"poll_id">> => Id}),
    discord_rest:channel_send_message(ChannelId, #{<<"content">> => Reply}),

    logger:debug("[poll:~s] Started handler for poll #~s", [Id, Id]),

    NewState = maps:merge(State, #{poll => PollData}),
    {reply, {ok, Id}, {watching, NewState}};
handle_call(Request, _From, _State) ->
    io:format("sie aq ~w", [Request]),
    {reply, {sie}}.

handle_cast({cast_vote, Option}, State) ->
    #{poll := #{id := PollId}} = State,
    logger:debug("[poll:~s] A vote was cast: ~s", [PollId, Option]),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    logger:debug("[poll] INFO ~w IN STATE ~w", [Info, State]),
    {noreply, State}.
