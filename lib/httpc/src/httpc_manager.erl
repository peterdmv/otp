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
-module(httpc_manager).

-behaviour(gen_server).

%% API
-export([start_link/1, request/1]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).


%%%=========================================================================
%%%  API
%%%=========================================================================
-spec start_link(Session) -> Response when
      Session :: atom(),
      Response :: {ok, pid()} | ignore | {error, Error},
      Error :: {already_started, pid()} | term().
start_link(Session) ->
    SessionName = session_name(Session),
    Server = {local, SessionName},
    Args = {Session},
    Opts = [],
    gen_server:start_link(Server, ?MODULE, Args, Opts).


%%--------------------------------------------------------------------------
%% Description: Sends a request to the httpc manager process.
%%--------------------------------------------------------------------------
-spec request(Request) -> Response when
      Request :: httpc:request(),
      Response :: {ok, reference()} | {error, httpc:reason()}.
request(Request = #{session := Session}) ->
    gen_server:call(session_name(Session), {request, Request}, infinity).


%%%=========================================================================
%%%  Gen_server callbacks
%%%=========================================================================
init({_Session}) ->
    {ok, []}.


handle_call({request, Request}, _, State) ->
    case (catch handle_request(Request, State)) of
	{reply, Msg, NewState} ->
	    {reply, Msg, NewState};
	Error ->
	    {stop, Error, http_error(Request, Error), State}
    end;
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
session_name(Session) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Session)).


http_error(#{id := Id}, Reason) ->
    {Id, {error, Reason}}.

handle_request(Request, State) ->
    RequestId = make_ref(),
    start_handler(Request#{requestid => RequestId}),
    {reply, {ok, RequestId}, State}.


start_handler(Request = #{session := Session}) ->
    {ok, Pid} = httpc_handler_sup:start_child([whereis(httpc_handler_sup),
                                               Request, [], session_name(Session)]),
    erlang:monitor(process, Pid).
