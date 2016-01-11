-module(test_harness).
-export([go/2]).

go(NumOfProcesses, Url) ->
    start_server(NumOfProcesses, Url),
    ok.

start_server(0, _Url) ->
    done;
start_server(NumOfProcesses, Url) ->
    fun_kickoff(Url),
    start_server(NumOfProcesses - 1, Url).

fun_kickoff(Url) ->
    spawn(pub_sub_server, start_link, [Url]).
                  
