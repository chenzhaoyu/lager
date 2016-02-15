-module(lager_json_format).

-export([format/1,format/2]).

format(Body, ParsList)->
    format([{body, Body}|ParsList]).
format(ParsList) ->
    jsonFormat(ParsList).

json_encode(Term) ->
    try
        {ok, mochijson2:encode(Term)}
    catch
        exit:{json_encode, E} ->
            {error, E}
    end.

json_decode(Term) ->
    try
        {ok, mochijson2:decode(Term)}
    catch
        %% Sadly `mochijson2:decode/1' does not offer a nice way to catch
        %% decoding errors...
        error:_ -> error
    end.

jsonFormat(ParsList) ->
    case json_encode({struct,ParsList}) of
        {error, _E} ->
            "";
        {ok, Ret} ->
            iolist_to_binary(Ret)
    end.

