%% @author Ondrej Kupka <ondra.cap@gmail.com>
%% @copyright 2012 Ondrej Kupka.

%% @doc Callbacks for the versionserver_rest application.

-module(versionserver_rest_app).
-author('Ondrej Kupka <ondra.cap@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for versionserver_rest.
start(_Type, _StartArgs) ->
    versionserver_rest_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for versionserver_rest.
stop(_State) ->
    ok.
