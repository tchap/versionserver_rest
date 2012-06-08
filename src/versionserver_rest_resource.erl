%% @author Ondrej Kupka <ondra.cap@gmail.com>
%% @copyright 2012 Ondrej Kupka.
%% @doc Example webmachine_resource.

-module(versionserver_rest_resource).

% Resource callbacks
-export([init/1, allowed_methods/2, content_types_provided/2,
	 delete_resource/2]).

% Handlers
-export([to_plain/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ===================================================================
%% Webmachine Resource callbacks
%% ===================================================================

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) -> 
	Result = ['GET', 'DELETE'],
	{Result, ReqData, Context}.

content_types_provided(ReqData, Context) ->
	Result = [{"text/plain", to_plain}, {"application/json", to_json}],
	{Result, ReqData, Context}.

delete_resource(ReqData, Context) ->
	Result = delete_project(ReqData),
	{Result, ReqData, Context}.

%% ===================================================================
%% Webmachine handlers
%% ===================================================================

to_plain(ReqData, Context) ->
	Result = next_build_number(ReqData),
	{Result, ReqData, Context}.

to_json(ReqData, Context) ->
	Result = ["{ build: ", next_build_number(ReqData), "}"],
	{Result, ReqData, Context}.

%% ===================================================================
%% Private functions
%% ===================================================================

next_build_number(ReqData) ->
	try
		Project = get_project_name(ReqData),
		Version = get_version(ReqData),
		integer_to_list(versionserver:next_build_number(Project,
								Version))
	catch
		throw:query_string -> 
			{halt, 400}
	end.

delete_project(ReqData) ->
	Project = get_project_name(ReqData),
	case catch versionserver:delete_project(Project) of
		ok -> true;
		{error, _} -> false;
		{'EXIT', _} -> false
	end.

get_project_name(ReqData) ->
	list_to_atom(wrq:path_info(project, ReqData)).

get_version(ReqData) ->
	try
		F = fun(Q) -> list_to_integer(wrq:get_qs_value(Q, ReqData)) end,
		[Major, Minor, Release] = lists:map(F, ["major", "minor", "release"]),
		{Major, Minor, Release}
	catch
		error:badarg ->
			throw(query_string)
	end.
