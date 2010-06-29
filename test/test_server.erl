-module (test_server).

-export ([
  start/0,
  run/1,
  get_value/1,
  stop/0
]).

start() -> 
  Pid = spawn_link(fun loop/0),
  register(?MODULE, Pid),
  {ok, Pid}.
  
run(N) -> whereis(?MODULE) ! {run, N}.
get_value(From)  -> whereis(?MODULE) ! {get_value, From}.
stop()  -> whereis(?MODULE) ! {stop}.

loop() -> loop(0).
loop(N) ->
  receive
    {run, I} ->
      NewN = N + I,
      loop(NewN);
    {get_value, From} ->
      From ! {value, N},
      loop(N);
    {stop} ->
      ok;
    _X ->
      loop(N)
  end.