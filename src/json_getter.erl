-module(json_getter).

-export([get/1]).


get(Url) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = 
        httpc:request(get, {Url, []}, [], []),
    io:format("Made request to ~p~n",[Url]),
    jiffy:decode(Body).
