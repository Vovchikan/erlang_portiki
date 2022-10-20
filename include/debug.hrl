-define (PRINT (F, A),
         io:format (
           "~s.erl(~p): ~s/~p -> ~s",
           [?MODULE, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY, io_lib:format(F, A)]
          )).
-define (PRINT (F), ?PRINT (F, [])).


%%==============================================================================
%% DEBUG MACROS
%%==============================================================================
-define (DBG (F, A),
  portiki_debugger:dbg(io_lib:format("~s:~w: " ++ F, [?MODULE, ?LINE | A]))).
-define (DBG (F), ?DBG (F, []) ).
