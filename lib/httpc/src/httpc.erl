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
         request/2,
         request/4,
         request/5
        ]).

%%%=========================================================================
%%%  Data Types
%%%=========================================================================
-type method() :: head | get | put | post | trace | options | delete | patch.

-type request() :: {uri(), headers()}
                 | {uri(), headers(), content_type(), body()}.

-type uri() :: uri_string:uri_string().

-type status_line() :: {http_version(), status_code(), reason_phrase()}.

-type http_version() :: string().

-type status_code() :: integer().

-type reason_phrase() :: string().

-type content_type() :: string().

-type headers() :: [header()].

-type header() :: {field(), value()}.

-type field() :: string().

-type value() :: string().

-type body() :: string()
              | binary()
              | {fun((accumulator()) -> body_processing_result()), accumulator()}
              | {chunkify, fun((accumulator()) -> body_processing_result()), accumulator()}.

-type body_processing_result() :: eof
                                | {ok, iolist(), accumulator()}.

-type accumulator() :: term().

-type filename() :: string().

-type http_options() :: [http_option()].

-type http_option() :: {timeout, timeout()}
                     | {connect_timeout, timeout()}
                     | {ssl, ssl_options()}
                     | {essl, ssl_options()}
                     | {autoredirect, boolean()}
                     | {proxy_auth, {user_string(), password_string()}}
                     | {version, http_version()}
                     | {relaxed, boolean()}
                     | {url_encode, boolean()}.

-type ssl_options() :: [ssl:ssl_option()].

-type user_string() :: string().

-type password_string() :: string().

-type options() :: [option()].

-type option() :: {sync, boolean()}
                | {stream, stream_to()}
                | {body_format, body_format()}
                | {full_result, boolean()}
                | {headers_as_is, boolean()}
                | {socket_opts, socket_options()}
                | {receiver, receiver()}
                | {ipv6_host_with_brackets, boolean()}.

-type stream_to() :: none
                   | self
                   | {self, once}
                   | filename().

-type socket_options() :: [ssl:option()].

-type receiver() :: pid()
                  | fun((term()) -> term())
                  | {Module :: atom(), Function :: atom(), Args :: list()}.

-type body_format() :: string | binary.
-type result() :: {status_line(), headers(), body()}
                | {status_code(), body()}
                | request_id().

-type request_id() :: reference().

-type profile() :: atom().

-type reason() :: {connect_failed, term()}
                | {send_failed, term()}
                | term().

%%%=========================================================================
%%%  API
%%%=========================================================================

start() ->
    application:start(httpc).

start_session(Config) ->
    httpc_session_sup:start_child(Config).

stop() ->
    application:stop(httpc).


%%--------------------------------------------------------------------------
%% Description: Sends a HTTP-request. The function can be both
%% syncronus and asynchronous in the later case the function will
%% return {ok, RequestId} and later on a message will be sent to the
%% calling process on the format {http, {RequestId, {StatusLine,
%% Headers, Body}}} or {http, {RequestId, {error, Reason}}}
%%--------------------------------------------------------------------------
-spec request(Uri) -> Response when
      Uri :: uri(),
      Response :: {ok, result()}
                | {error, reason()}.
request(Uri) ->
    request(Uri, ?DEFAULT_SESSION).


-spec request(Uri, Profile) -> Response when
      Uri :: uri(),
      Profile :: atom(),
      Response :: {ok, result()}
                | {error, reason()}.
request(Uri, Profile) ->
    request(get, {Uri, []}, [], [], Profile).


-spec request(Method, Request, HTTPOptions, Options) -> Response when
      Method :: method(),
      Request :: request(),
      HTTPOptions :: http_options(),
      Options :: options(),
      Response :: {ok, result()}
                | {ok, saved_to_file}
                | {error, reason()}.
request(Method, Request, HttpOptions, Options) ->
    request(Method, Request, HttpOptions, Options, ?DEFAULT_SESSION).


-spec request(Method, Request, HTTPOptions, Options, Profile) -> Response when
      Method :: method(),
      Request :: request(),
      HTTPOptions :: http_options(),
      Options :: options(),
      Profile :: profile(),
      Response :: {ok, result()}
                | {ok, saved_to_file}
                | {error, reason()}.
request(Method, Request, HTTPOptions, Options, Profile) when
      (is_tuple(Request) andalso (tuple_size(Request) =:= 2) andalso is_atom(Profile)) ->
    do_request(Method, Request, HTTPOptions, Options, Profile);
request(Method, Request, HTTPOptions, Options, Profile) when
      (is_tuple(Request) andalso (tuple_size(Request) =:= 4) andalso is_atom(Profile)) ->
    do_request_with_body(Method, Request, HTTPOptions, Options, Profile).



%%%========================================================================
%%% Internal functions
%%%========================================================================

-spec do_request(Method,
                 {Uri, Headers},
                 HTTPOptions, Options, Session) -> Response when
      Method :: method(),
      Uri :: uri(),
      Headers :: headers(),
      HTTPOptions :: http_options(),
      Options :: options(),
      Session :: profile(),
      Response :: {ok, result()}
                | {ok, saved_to_file}
                | {error, reason()}.
do_request(Method,
           {Uri, Headers},
           HTTPOptions, Options, Session)
  when (Method =:= options) orelse (Method =:= get) orelse
       (Method =:= head) orelse (Method =:= delete) orelse
       (Method =:= trace) ->
    prep_handle_request(Method,
                        Uri, Headers, [], [],
                        HTTPOptions, Options,Session).


-spec do_request_with_body(Method,
                           {Uri, Headers, ContentType, Body},
                           HTTPOptions, Options, Session) -> Response when
      Method :: method(),
      Uri :: uri(),
      Headers :: headers(),
      ContentType :: content_type(),
      Body :: body(),
      HTTPOptions :: http_options(),
      Options :: options(),
      Session :: profile(),
      Response :: {ok, result()}
                | {ok, saved_to_file}
                | {error, reason()}.
do_request_with_body(Method,
                     {Uri, Headers, ContentType, Body},
                     HTTPOptions, Options, Session)
  when ((Method =:= post) orelse (Method =:= path) orelse
       (Method =:= put) orelse (Method =:= delete))
       andalso is_list(ContentType) ->
    case check_body(Body) of
        ok ->
            prep_handle_request(Method,
                                Uri, Headers, ContentType, Body,
                                HTTPOptions, Options, Session);
        Error ->
            Error
    end.


prep_handle_request(Method,
                    Uri, Headers, ContentType, Body,
                    HTTPOptions, Options, Session) ->
    case uri_string:parse(uri_string:normalize(Uri)) of
	{error, Reason, _} ->
	    {error, Reason};
	ParsedUri ->
            handle_request(Method, Uri, ParsedUri, Headers, ContentType, Body,
                           HTTPOptions, Options, Session)
    end.



handle_request(Method, _Uri,
               URI, _, _,
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


check_body({Fun, _}) when is_function(Fun) ->
    ok;
check_body({chunkify, Fun, _}) when is_function(Fun) ->
    ok;
check_body(Body) when is_list(Body) orelse is_binary(Body) ->
    ok;
check_body(Body) ->
    {error, {bad_body_generator, Body}}.
