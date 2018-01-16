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

-module(httpc).

-include("httpc_internal.hrl").

-export([start/0,
         start_session/1,
         stop/0,
         request/1,
         request/2
        ]).


%%%=========================================================================
%%%  API
%%%=========================================================================

start() ->
    application:start(httpc).

start_session(Config) ->
    httpc_session_sup:start_child(Config).

stop() ->
    application:stop(httpc).

request(Uri) ->
    request(Uri, ?DEFAULT_SESSION).

request(Uri, Session) ->
    request(get, {Uri, []}, [], [], Session).

request(Method,
        {Uri, Headers},
        _HTTPOptions, _Options, Session)
  when (Method =:= options) orelse
       (Method =:= get) orelse
       (Method =:= head) orelse
       (Method =:= delete) orelse
       (Method =:= trace) andalso
       is_atom(Session) ->
    case uri_string:parse(uri_string:normalize(Uri)) of
	{error, Reason, _} ->
	    {error, Reason};
	ParsedUri ->
            handle_request(Method, Uri, ParsedUri, Headers, [], [],
                           Session)
    end.


%%%========================================================================
%%% Internal functions
%%%========================================================================
handle_request(Method, _Uri,
               URI,
	       _Headers0, _ContentType, Body0,
	       Session) ->
    Request = create_request(Method, URI, Session, Body0),
    case httpc_manager:request(Request) of
        {ok, RequestId} ->
            io:format("# RequestId: ~p", [RequestId]),
            handle_answer(RequestId);
        {error, Reason} ->
            {error, Reason}
    end.


create_request(Method, URI, Session, Body) ->
    #{from => self(),
      method  => Method,
      uri     => URI,
      session => Session,
      body    => Body
     }.

handle_answer(RequestId) ->
    receive
	{http, {RequestId, {StatusLine, Headers, Body}}} ->
            {ok, {StatusLine, Headers, Body}};
	{http, {RequestId, {error, Reason}}} ->
	    {error, Reason}
    after 3000 ->
            timeout
    end.
