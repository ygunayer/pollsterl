-module(pollsterl_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    ChildSpecs = [
        #{id => discord_sup, start => {discord_sup, start_link, []}, restart => transient},
        #{id => emoji, start => {emoji, start_link, []}, restart => transient},
        #{id => templates, start => {pollsterl_templates, start_link, []}, restart => transient},
        #{id => cmd_relay, start => {pollsterl_command_relay, start_link, []}, restart => transient},
        #{id => poll_sup, start => {pollsterl_poll_sup, start_link, []}, restart => transient},
        #{id => manager, start => {pollsterl_manager, start_link, []}, restart => transient}
    ],
    logger:debug("[supervisor:root] Root supervisor is launching"),
    {ok, {SupFlags, ChildSpecs}}.
