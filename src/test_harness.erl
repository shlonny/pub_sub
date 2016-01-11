-module(test_harness).
-export([go/2]).

go(NumOfProcesses, Url) ->
    start_server(NumOfProcesses, Url),
    Pids = [],
    loop(NumOfProcesses, Pids),
    ok.

loop(NumOfProcesses, Pids) ->
    receive
        {'DOWN', _Ref, process, Pid, _Why} ->
            case Pids ++ [Pid] of
                 List when length(List) =:= NumOfProcesses ->
                    io:format("completed all!!!!", []),
                    ok;
                _ ->
                    loop(NumOfProcesses, Pids ++ [Pid])
            end
    end.

start_server(0, _Url) ->
    done;
start_server(NumOfProcesses, Url) ->
    fun_kickoff(Url),
    start_server(NumOfProcesses - 1, Url).

fun_kickoff(Url) ->
    spawn_monitor(pub_sub_server, start_link, [Url]).
                  
