%% @author Ondrej Kupka <ondra.cap@gmail.com>
%% @copyright 2012 Ondrej Kupka.

%% @doc versionserver_rest startup code

-module(versionserver_rest).
-author('Ondrej Kupka <ondra.cap@gmail.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    ensure_started(versionserver),
    versionserver_rest_sup:start_link().

%% @spec start() -> ok
%% @doc Start the versionserver_rest server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    ensure_started(versionserver),
    application:start(versionserver_rest).

%% @spec stop() -> ok
%% @doc Stop the versionserver_rest server.
stop() ->
    Res = application:stop(versionserver_rest),
    application:stop(versionserver),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
