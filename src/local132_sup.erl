-module(local132_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0, % start on generic workers
  start_link/1, % start with specified N workers
  stop/0        % stop it all!
]).

-define (DEFAULT_WORKERS_COUNT, 2).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(TCHILD (I, Type), {I, {I, start_link, []}, transient, 16#ffffffff, Type, [I]}).
-define(COUNT_CHILD (I, Type, Count), fun() -> [{N, {I, start_link, [N]}, transient, 16#ffffffff, Type, [I]} || N <- lists:seq(1, Count)] end()).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  start_link(?DEFAULT_WORKERS_COUNT).

start_link(Count) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Count]).
  
stop() ->
  lists:foreach(fun({WorkerId, _Pid, _, _}) ->
    supervisor:terminate_child(?MODULE, WorkerId)
  end, supervisor:which_children(local132_sup)).
  
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Count]) ->
  Server = ?TCHILD(local132, worker),
  Clients = ?COUNT_CHILD(local132_worker, worker, Count),
  Children = [Server|Clients],
  {ok, { {one_for_one, 10, 10}, Children} }.