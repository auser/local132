-module (local132_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  local132_sup:start_link(2),
  % dummy server
  _Pid = test_server:start(),
  ok.
  
teardown(_X) ->
  local132_sup:stop(),
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun simple_queue_tests/0,
        fun lots_of_jobs_tests/0
      ]
    }
  }.

simple_queue_tests() ->
  ?assert(10 =:= local132:submit_job(fun() -> 10 end)),
  ?assert(11 =:= local132:submit_job(fun() -> 11 end)),
  ?assert(90 =/= local132:submit_job(fun() -> 10 end)),
  passed.

lots_of_jobs_tests() ->
  lists:map(fun(N) ->
    ok = local132:async_job(fun() -> test_server:add(N) end)
  end, lists:seq(1,100)),
  timer:sleep(100),
  test_server:get_value(self()),
  Val = receive
    {value, N} -> N
  end,
  ?assert(5050 =:= Val),
  passed.