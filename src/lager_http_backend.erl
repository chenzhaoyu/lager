%%create by czy
-module(lager_http_backend).

-include("lager.hrl").

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-define(METHODS,
    [get, post]).

-define(HTTPFORMATTER,
    [lager_json_format]).

-define(DEFAULT_LOG_LEVEL, info).

-define(HTTP_CONTENT_TYPE_FORM_URLENCODED, "application/x-www-form-urlencoded").

-record(state, {
        address :: string(),
        level :: {'mask', integer()},
        method :: atom(),
        formatter :: atom(),
        formatter_config :: any(),
        http_formatter :: atom(),
        http_formatter_config :: any()
    }).

-type option() :: {address, string()} | {level, lager:log_level()} |
                  {method, atom()} | 
                  {formatter, atom()} |
                  {formatter_config, term()}|
                  {http_formatter, atom()} |
                  {http_formatter_config, term()}.

-spec init([option(),...]) -> {ok, #state{}} | {error, bad_config}.

init(LogFileConfig) when is_list(LogFileConfig) ->
    case validate_logfile_proplist(LogFileConfig) of
        false ->
            %% falied to validate config
            {error, {fatal, bad_config}};
        Config ->
            %% probabably a better way to do this, but whatever
            [Address, Level, Method, Formatter, FormatterConfig, HttpFormatter, HttpFormatterConfig] =
              [proplists:get_value(Key, Config) || Key <- [address, level, method, formatter, formatter_config, http_formatter, http_formatter_config]],
            State = #state{address=Address, level=Level, method=Method, formatter=Formatter,
                formatter_config=FormatterConfig, http_formatter=HttpFormatter,
                http_formatter_config=HttpFormatterConfig},
            inets:start(),
            {ok, State}
    end.

validate_logfile_proplist(List) ->
    try validate_logfile_proplist(List, []) of
        Res ->
            case proplists:get_value(address, Res) of
                undefined ->
                    ?INT_LOG(error, "Missing required address option", []),
                    false;
                _Address ->
                    %% merge with the default options
                    lists:keymerge(1, lists:sort(Res), lists:sort([
                            {address, ""},
                            {level, ?DEFAULT_LOG_LEVEL},
                            {method, post},
                            {formatter, lager_default_formatter}, {formatter_config, []},
                            {http_formatter, lager_json_format}, {http_formatter_config, []}
                        ]))
            end
    catch
        {bad_config, Msg, Value} ->
            ?INT_LOG(error, "~s ~p for file ~p",
                [Msg, Value, proplists:get_value(file, List)]),
            false
    end.

validate_logfile_proplist([], Acc) ->
    Acc;
validate_logfile_proplist([{address, Address}|Tail], Acc) ->
    %% is there any reasonable validation we can do here?
    validate_logfile_proplist(Tail, [{address, Address}|Acc]);
validate_logfile_proplist([{http_formatter_config, Http_formatter_config}|Tail], Acc) ->
    %% is there any reasonable validation we can do here?
    validate_logfile_proplist(Tail, [{http_formatter_config, Http_formatter_config}|Acc]);
validate_logfile_proplist([{level, Level}|Tail], Acc) ->
    case validate_loglevel(Level) of
        false ->
            throw({bad_config, "Invalid loglevel", Level});
        Res ->
            validate_logfile_proplist(Tail, [{level, Res}|Acc])
    end;
validate_logfile_proplist([{method, Method}|Tail], Acc) ->
    case validate_httpmethod(Method) of
        false ->
            throw({bad_config, "Invalid httpmethod", Method});
        Res ->
            validate_logfile_proplist(Tail, [{level, Res}|Acc])
    end;
validate_logfile_proplist([{formatter, Fmt}|Tail], Acc) ->
    case is_atom(Fmt) of
        true ->
            validate_logfile_proplist(Tail, [{formatter, Fmt}|Acc]);
        false ->
            throw({bad_config, "Invalid formatter module", Fmt})
    end;
validate_logfile_proplist([{formatter_config, FmtCfg}|Tail], Acc) ->
    case is_list(FmtCfg) of
        true ->
            validate_logfile_proplist(Tail, [{formatter_config, FmtCfg}|Acc]);
        false ->
            throw({bad_config, "Invalid formatter config", FmtCfg})
    end;
validate_logfile_proplist([{http_formatter, HttpFormatter}|Tail], Acc) ->
    case validate_httpformatter(HttpFormatter) of
        false ->
            throw({bad_config, "Invalid httpformatter", HttpFormatter});
        _Res ->
            validate_logfile_proplist(Tail, [{http_formatter, HttpFormatter}|Acc])
    end;
validate_logfile_proplist([Other|_Tail], _Acc) ->
    throw({bad_config, "Invalid option", Other}).


validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Levels ->
            Levels
    catch
        _:_ ->
            false
    end.

validate_httpmethod(Method) ->
    proplists:is_defined(Method, ?METHODS).
    
validate_httpformatter(Formatter) ->
    proplists:is_defined(Formatter, ?HTTPFORMATTER).

handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
   try lager_util:config_to_mask(Level) of
        Levels ->
            {ok, ok, State#state{level=Levels}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message},
    #state{level=Level,formatter=Formatter,formatter_config=FormatConfig, http_formatter=HttpFormatter,
    http_formatter_config=HttpFormatterConfig, method=Method,address=Address} = State) ->
    Ret = Formatter:format(Message,FormatConfig),
    case string:len(Ret) of
        0 ->
            {ok, State};
        Len ->
            CleanRet = string:sub_string(Ret, 1, Len-1),
            CleanRet2 = list_to_binary(lists:flatten(CleanRet)),
            HttpRet = HttpFormatter:format(CleanRet2, [{logLevel,Level}|HttpFormatterConfig),
            spawn(httpc, request, [Method, {Address, [], "", HttpRet}, [], [{sync, true}]]),
            %%httpc:request(post, {, [], "", HttpRet}, [], [{sync, true}]),
            {ok, State} 
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
