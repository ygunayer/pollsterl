-module(pollsterl_chatter_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    ChildSpecs = [
        #{id => chatter, start => {pollsterl_chatter, start_link, []}, restart => transient}
    ],
    logger:debug("[supervisor:chatter] Chatter supervisor is launching"),
    {ok, {SupFlags, ChildSpecs}}.
