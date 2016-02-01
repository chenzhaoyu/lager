-module(lager_nelo_format).

-export([format/2]).

-define(DEFAULT_PROJECT_NAME, "OneApp-MQTT").
-define(DEFAULT_LOG_LEVEL, "info").
-define(DEFAULT_PROJECT_VERSION, "1.0.0").

jsonEntryStr(Key, Value) when is_list(Key), is_list(Value) ->
    "\"" ++ Key ++ "\":\"" ++ Value ++ "\"";
jsonEntryStr(_Key, _Value) ->
    false.

jsonFormat(ParsList) when is_list(ParsList) ->
    jsonFormat(ParsList, "");
jsonFormat(_ParsList) ->
    false.
jsonFormat([], JsonEntrys) ->
    "{" ++ JsonEntrys ++ "}";
jsonFormat([{Key, Value}|ResPaeList], RJsonEntrys) ->
    case jsonEntryStr(Key, Value) of
        false ->
            jsonFormat(ResPaeList, RJsonEntrys);
        LJsonEntrys ->
            case string:equal("",RJsonEntrys) of
                true ->
                    jsonFormat(ResPaeList, LJsonEntrys);
                _    ->
                    jsonFormat(ResPaeList, LJsonEntrys++ "," ++ RJsonEntrys)
            end
    end.

addDefaultPar(ParsList) ->
    ParsList0 = addDefaultPar(ParsList, {"projectName", ?DEFAULT_PROJECT_NAME}),
    ParsList1 = addDefaultPar(ParsList0, {"logLevel", ?DEFAULT_LOG_LEVEL}),
    addDefaultPar(ParsList1, {"projectVersion", ?DEFAULT_PROJECT_VERSION}).
addDefaultPar(ParsList, {Key, Value}) ->
    case proplists:is_defined(Key, ParsList) of
        true ->
            ParsList;
        _    ->
            [{Key, Value}|ParsList]
    end.
            
format(Body, ParsList) when is_atom(Body)->
    format(atom_to_list(Body), ParsList);
format(Body, ParsList) when is_binary(Body)->
    format(atom_to_list(Body), ParsList);
format(Body, ParsList) when is_list(Body)->
    format([{"body", Body}|ParsList]);
format(Body, ParsList) ->
    format(atom_to_list(Body), ParsList).
    %%false.
format(ParsList) ->
    jsonFormat(addDefaultPar(ParsList)).
