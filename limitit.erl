-module(limitit).
-export([start/0]).
-export([control_loop/0, loop/1]).
-define(RUNNUM, 20).
-define(AIM, "192.168.1.12").
-define(STARTP, 49000).
-define(ENDP,51000).
-define(CONTENT, "uwhdiwqhdisaldjheluhskjdhqiuhduelqiuhduhsqlwiudhqiuhsd").


start() ->
    init().

init() ->
    register(control, spawn(?MODULE, control_loop, [])),
    run_new_loop(?RUNNUM).

run_new_loop(0) -> ok;
run_new_loop(Num) ->
    {ok, Socket} = gen_udp:open(0),
    spawn(?MODULE, loop, [Socket]),
    run_new_loop(Num-1).

control_loop() ->
    receive
        {add, Num} ->
            run_new_loop(Num),
            io:format("start ~p new process.~n", [Num]);
        _ ->
            ok
    end,
    control_loop().

loop(Socket) ->
%    {ok,Socket} = gen_udp:open(0),
    gen_udp:send(Socket, ?AIM, create_random_port(?STARTP, ?ENDP), ?CONTENT),
    loop(Socket).
    

create_random_port(Start, End) ->
    Start + random:uniform(End-Start).
