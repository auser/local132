%%% local132.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 06/28/10 Ari Lerner <arilerner@mac.com>
%% @doc A basic worker pool
-module (local132).

-behaviour(gen_server).

%% API
-export([
  submit_job/1, % submit a new job
  async_job/1,   % submit a job and wait for the result
  ready/1,      % tell that the worker is ready
  start_link/0  % start the server
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
    handle_info/2, terminate/2, code_change/3]).

-record(state, {
  available,
  pending
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, infinity}]).

submit_job(Fun) ->
  WorkerPid = gen_server:call(?SERVER, next_available, infinity),
  local132_worker:sync_run(WorkerPid, Fun).

async_job(Fun) ->
  gen_server:cast(?SERVER, {run, Fun}).

ready(WorkerId) ->
  gen_server:cast(?SERVER, {ready, WorkerId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state { pending = queue:new(), available = queue:new() }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
% Get the next available worker
handle_call(next_available, From, #state{available = Avail, pending = Pend} = State) ->
  case queue:out(Avail) of
    {empty, _NewAvail} ->
      % There are no available workers, put it in the pending queue!
      {noreply, State#state{pending = queue:in({next_available, From}, Pend)}};
    {{value, WorkerId}, NewAvail} ->
      {reply, get_worker(WorkerId), State#state{available = NewAvail}}
  end;
handle_call(Request, _From, State) ->
  {stop, {unhandled_message, Request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({ready, WorkerId}, #state{available = Avail, pending = Pend} = State) ->
  NewState = case queue:out(Pend) of
    {empty, _Pend} -> State#state{available = queue:in(WorkerId, Avail)};
    {{value, {next_available, From}}, NewPend} ->
      gen_server:reply(From, get_worker(WorkerId)), State#state{pending = NewPend};
    {{value, {run, Fun}}, NewPend} ->
      local132_worker:submit_run(get_worker(WorkerId), Fun), State#state{pending = NewPend}
  end,
  {noreply, NewState};
handle_cast({run, Fun}, #state{available = Avail, pending = Pend} = State) ->
  NewState = case queue:out(Avail) of
    {empty, _NewAvail} -> State#state{pending = queue:in({run, Fun}, Pend)};
    {{value, WorkerId}, NewAvail} ->
      local132_worker:submit_run(get_worker(WorkerId), Fun),
      State#state{available = NewAvail}
  end,
  {noreply, NewState};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_worker(WorkerId) ->
  [{WorkerId, Pid, _Type, _Modules} | _] =
    lists:dropwhile(fun ({Id, _Pid, _Type, _Modules})
      when Id =:= WorkerId  -> false;
      (_)                   -> true
    end,
    supervisor:which_children(local132_sup)),
  Pid.
