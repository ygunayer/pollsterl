-module(pollsterl_templates).
-export([start_link/0, render/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-define(SERVER_NAME, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, {}, []).

init(_Args) ->
    RootDir = filename:join(code:priv_dir(pollsterl), "templates"),
    {ok, Filenames} = file:list_dir(RootDir),
    Templates = lists:foldl(
        fun(Filename, Acc) ->
            case filename:extension(Filename) of
                ".mustache" ->
                    TemplateName = filename:basename(filename:rootname(Filename)),
                    FullPath = filename:join(RootDir, Filename),
                    Template = bbmustache:parse_file(FullPath),
                    maps:merge(Acc, #{TemplateName => Template});
                _ -> Acc
            end
        end,
        maps:new(),
        Filenames
    ),
    logger:debug("[templates] ~w template(s) were read from the file system", [maps:size(Templates)]),
    {ok, {ready, #{templates => Templates}}}.

render(TemplateName, Data) ->
    gen_server:call(?SERVER_NAME, {render, {TemplateName, Data}}).

handle_call({render, {TemplateName, Data}}, _From, {ready, #{templates := Templates}} = State) ->
    case maps:is_key(TemplateName, Templates) of
        true ->
            Template = maps:get(TemplateName, Templates),
            {reply, bbmustache:compile(Template, Data, [{key_type, binary}, raise_on_context_miss]), State};
        _ ->
            {reply, {error, invalid_template}, State}
    end;

handle_call(Request, _From, _State) ->
    io:format("sie aq ~w", [Request]),
    {reply, {sie}}.

handle_cast(_Message, State) ->
    {noreply, State}.
