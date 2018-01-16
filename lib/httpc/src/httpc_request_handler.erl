%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(httpc_request_handler).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).


%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(_Parent, _Request = #{from := From, requestid := RequestId}, _Options, _ProfileName) ->
    Opts = [],
    gen_server:start_link(?MODULE, {From, RequestId}, Opts).


%%%=========================================================================
%%%  Gen_server callbacks
%%%=========================================================================
init({From, RequestId}) ->
    self() ! {fake_http_answer, From, RequestId},
    {ok, []}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({fake_http_answer, From, RequestId}, State) ->
    From ! {http, {RequestId, {"200 OK", "test", ""}}},
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
