-module(ra_process_set_app).

-behaviour(application).

%% API exports
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ra_proces_set_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
