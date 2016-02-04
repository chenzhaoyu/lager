-module(lager_json_format).

-export([format/1,format/2]).

format(Body, ParsList)->
    format([{body, Body}|ParsList]).
format(ParsList) ->
    jsonFormat(ParsList).

jsonFormat(ParsList) ->
    jiffy:encode(ParsList).
