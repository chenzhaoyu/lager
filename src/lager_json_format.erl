-module(lager_json_format).

-export([format/1,format/2]).

-define(DEFAULT_PROJECT_NAME, <<"OneApp-MQTT">>).
-define(DEFAULT_LOG_LEVEL, <<"info">>).
-define(DEFAULT_PROJECT_VERSION, <<"1.0.0">>).

addDefaultPar(ParsList) ->
    ParsList0 = addDefaultPar(ParsList, {projectName, ?DEFAULT_PROJECT_NAME}),
    ParsList1 = addDefaultPar(ParsList0, {logLevel, ?DEFAULT_LOG_LEVEL}),
    addDefaultPar(ParsList1, {projectVersion, ?DEFAULT_PROJECT_VERSION}).
addDefaultPar(ParsList, {Key, Value}) ->
    case proplists:is_defined(Key, ParsList) of
        true ->
            ParsList;
        _    ->
            [{Key, Value}|ParsList]
    end.
            
format(Body, ParsList)->
    format([{body, Body}|ParsList]).
format(ParsList) ->
    jsonFormat(addDefaultPar(ParsList)).

jsonFormat(ParsList) ->
    jiffy:encode(ParsList).
