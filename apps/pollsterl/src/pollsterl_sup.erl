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
        #{id => discord, start => {discord, start_link, []}, restart => transient},
        #{id => message_builder, start => {message_builder, start_link, []}, restart => transient},
        #{id => chatter_sup, start => {pollsterl_chatter_sup, start_link, []}, restart => transient}
    ],
    {ok, {SupFlags, ChildSpecs}}.
