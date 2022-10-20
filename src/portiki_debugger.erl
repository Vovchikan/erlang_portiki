-module (portiki_debugger).

-behaviour (gen_server).

% API
-export ([start_link/0,
          set_mode_enabled/2,
          set_debug_interval/1,
          dbg/1]).

% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-record (st, {dbg_cli       = false,
              dbg_shell     = true,
              min_interval  = 0,
              timer,
              timer_expired = true}).

-define (TIMER_MSG, [104,117,106,97,107,32,98,108,101,97,116]). %% put it in shell
                                                                %% if you are so
                                                                %% curious
-define (TIMER_EXPIRED_MSG(Ref), {timeout, Ref, ?TIMER_MSG}).

%%======================================================================
%% API
%%======================================================================

start_link () ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%-----------------------------------------------------------------------

set_mode_enabled (Mode, Enabled) when
  (Mode == cli orelse Mode == shell) andalso is_boolean(Enabled) ->
  gen_server:call(?MODULE, {enable, Mode, Enabled}).

%-----------------------------------------------------------------------

set_debug_interval (Interval) when
  is_integer(Interval) andalso Interval >= 0 ->
  gen_server:call(?MODULE, {min_interval, Interval}).

%-----------------------------------------------------------------------

dbg (Str) when
  is_list(Str) ->
  catch gen_server:call(?MODULE, {dbg, Str}),
  ok.

%%======================================================================
%% gen_server callbacks
%%======================================================================

init (_Args) ->
  {ok, #st{}}.

%-----------------------------------------------------------------------

handle_call ({enable, Mode, Enabled} = _Msg, _From, St) ->
  NewSt = do_enable_mode(Mode, Enabled, St),
  {reply, ok, NewSt};
handle_call ({min_interval, Interval} = _Msg, _From, St) ->
  NewSt = do_update_interval(Interval, St),
  {reply, ok, NewSt};
handle_call ({dbg, Str} = _Msg, _From, St) ->
  do_debug(Str, St),
  {reply, ok, St};
handle_call (_Msg, _From, St) ->
  {reply, ok, St}.

%-----------------------------------------------------------------------

handle_cast (_Msg, St) ->
  {noreply, St}.

%-----------------------------------------------------------------------

handle_info (?TIMER_EXPIRED_MSG(Ref) = _Msg, St) ->
  NewSt = do_expire(Ref, St),
  {noreply, NewSt};
handle_info (_Msg, St) ->
  {noreply, St}.

%-----------------------------------------------------------------------

terminate (_Reason, _State) ->
  ok.

%-----------------------------------------------------------------------

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%======================================================================
%% private
%%======================================================================

do_enable_mode (cli, Enabled, State) ->
  State#st{dbg_cli = Enabled};
do_enable_mode (shell, Enabled, State) ->
  State#st{dbg_shell = Enabled};
do_enable_mode (_Mode, _Enabled, State) ->
  State.

%-----------------------------------------------------------------------

do_update_interval (Interval, State = #st{timer = T}) ->
  catch erlang:cancel_timer(T),
  Timer = erlang:start_timer(Interval, ?MODULE, ?TIMER_MSG),
  State#st{timer = Timer, min_interval = Interval, timer_expired = false};
do_update_interval (_Interval, State) ->
  State.

%-----------------------------------------------------------------------

do_expire (Ref, State = #st{timer = Ref}) ->
  State#st{timer = undefined, timer_expired = true};
do_expire (_Ref, State) ->
  State.

%-----------------------------------------------------------------------

do_debug (Str, #st{dbg_cli = C,
                   dbg_shell = S,
                   timer_expired = true} = _State) ->
  if C -> do_debug_cli (Str); true -> ok end,
  if S -> do_debug_shell (Str); true -> ok end.

%-----------------------------------------------------------------------

do_debug_cli (Str) ->
  catch event:notify(re:replace(Str, "\n", "~n", [global, {return, list}])).

%-----------------------------------------------------------------------

do_debug_shell (Str) ->
  catch io:format(standard_error, "~s", [Str]).
