-module(pollsterl_poll_sup).
-export([start_link/0, start_poll/0, stop_poll/1]).

-behaviour(supervisor).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_poll() ->
    supervisor:start_child(?SERVER, []).

stop_poll(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
    ChildSpecs = [#{id => poll, start => {pollsterl_poll_handler, start_link, [self()]}}],
    logger:debug("[supervisor:poll] Poll supervisor is launching"),
    {ok, {SupFlags, ChildSpecs}}.
