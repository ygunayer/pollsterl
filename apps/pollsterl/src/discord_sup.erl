-module(discord_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    ChildSpecs = [
        #{id => discord_rest, start => {discord_rest, start_link, []}, restart => transient},
        #{id => discord_gateway, start => {discord_gateway, start_link, []}, restart => transient}
    ],
    {ok, {SupFlags, ChildSpecs}}.
