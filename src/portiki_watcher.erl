-module (portiki_watcher).


-include ("../include/debug.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour (gen_server).

%% API
-export ([start_link/0,
          add_portik/0,
          del_portik/0]).

%% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).


-record (state, {
           db :: ets:table() }
        ).
-record (db_rec, {
           system_pid  :: integer(), %% Key
           erlang_port :: erlang:port() }
        ).

%% DEFs
-define (DB, watcher_db).


%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the server as part of a supervision tree.
%% @end
%%------------------------------------------------------------------------------

-spec start_link () -> gen_server:start_ret().
start_link () ->
  ServerName = {local, ?MODULE},
  Module = ?MODULE,
  Args = [],
  Options = [],
  gen_server:start_link (ServerName, Module, Args, Options).

%%------------------------------------------------------------------------------
%% @doc Starting llmsniffer for vlan.
%% @end
%%------------------------------------------------------------------------------
-spec add_portik () -> ok | {error, term()}.
add_portik () ->
  gen_server:call (?MODULE, add_portik).

-spec del_portik () -> ok | {error, term()}.
del_portik () ->
  gen_server:call (?MODULE, del_portik).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================
init ([]) ->
  Db = ets:new (?DB, [set, named_table, {keypos, 2}]),
  {ok, #state{ db = Db }}.

handle_call (add_portik, _From, #state{ db = Db }=State) ->
  Cmd = get_portik_cmd (),
  do_add_portik (Cmd, Db),
  ?DBG ("Updated ets = ~p~n", [ets:tab2list(Db)]),
  {reply, ok, State};

handle_call (del_portik, _From, #state{ db = Db }=State) ->
  {reply, ok, State};

handle_call (_Request, _From, State) ->
  ?DBG ("Undandled REQUEST!~n"),
  {reply, ignored, State}.

handle_cast (_Msg, State) ->
  ?DBG ("Undandled MSG!~n"),
  {noreply, State}.

handle_info ({PortDriver, {exit_status, Status}}, #state{ db = Db }=State) ->
  ?DBG ("Received exit_status(~p) on port - ~p~n", [Status, PortDriver]),
  MatchSpec =
    ets:fun2ms (
      fun (#db_rec{ erlang_port = PD }=Rec) when PD == PortDriver ->
          Rec
      end),
  ?DBG ("MatchSpec = ~p~n", [MatchSpec]),
  R = ets:match_object (Db, MatchSpec),
  ?DBG ("Match objects = ~p~n", [R]),
  ets:match_delete (Db, MatchSpec),
  % TODO add check_dummy_config with adding new sniffers
  {noreply, State};

handle_info (_Info, State) ->
  ?DBG ("Undandled INFO!~n"),
  ?DBG ("Info = ~p~n", [_Info]),
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
get_portik_cmd () ->
  Cmd = "top",
  Cmd.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Open erlang port and start sniffer.
%% @end
%%------------------------------------------------------------------------------
do_add_portik (Cmd, Db) ->
  ErlPort =  open_port ({spawn, Cmd},
                   [use_stdio, stderr_to_stdout, exit_status, stream]),
  ?DBG ("open port (~p) with Cmd = ~s~n", [ErlPort, Cmd]),
  case erlang:port_info (ErlPort, os_pid) of
    {os_pid, OsPid} ->
      ?DBG ("Port with os pid = ~p~n", [OsPid]),
      ets:insert (Db, #db_rec{ system_pid = OsPid,
                               erlang_port = ErlPort });
    Error ->
      ?DBG ("Smth go wrong: ~p~n", [Error]),
      catch port_close(ErlPort) end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Close erlang port and kill sniffer pid.
%% @end
%%------------------------------------------------------------------------------
do_del_portik (#db_rec{ system_pid = OsPid,
                        erlang_port = Pd }, Db) ->
  os:cmd (io_lib:format (
            "kill ~p", [OsPid])),
  port_close (Pd),
  ets:delete (Db, OsPid).
