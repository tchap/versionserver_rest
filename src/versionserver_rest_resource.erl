%% Copyright (c) 2012, Ondrej Kupka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met: 
%%
%% 1. Redistributions of source code must retain the above copyright notice, this
%%    list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution. 
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
