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
-module(httpc_transport).

-export([create_socket/3,send/3]).

-define(SP, <<" ">>).
-define(CRLF, <<"\r\n">>).

%%%=========================================================================
%%%  API
%%%=========================================================================

send(_Uri = #{host := Host},
     _Request = #{method := Method, headers := Headers0, body := Body},
     Options = #{socket := Socket}) ->
    Headers = build_headers(Host, Headers0, Options),
    Message = [method(Method), ?SP, "/", ?SP, <<"HTTP/1.1">>, ?CRLF,
               Headers, ?CRLF, Body],
    io:format("### Transport SEND: ~p~n", [Message]),
    gen_tcp:send(Socket, Message).


%%====================================================================
%% Internal functions
%%====================================================================

create_socket(#{host := Host, port := Port}, HTTPOpts, SocketOpts0) ->
    Timeout = maps:get(timeout, HTTPOpts),
    SocketOpts = [ binary, {packet, http}, {active, false}, {reuseaddr, true}| SocketOpts0],

    {ok, Socket} = %gen_tcp:connect(Host, Port, SocketOpts, Timeout),
    try gen_tcp:connect(Host, Port, SocketOpts, Timeout) of
	{ok, _} = OK ->
	    OK;
	{error, _} = ERROR ->
	    ERROR
    catch
	exit:{badarg, _} ->
	    {error, {eoptions, HTTPOpts}};
	exit:badarg ->
	    {error, {eoptions, HTTPOpts}};
        Type:Exception  ->
            io:format("### Transport Socket: ~p ## ~p ~n", [Type, Exception])
    end,
    io:format("### Transport Socket: ~p~n", [Socket]),
    Socket.


method(head) ->
    <<"HEAD">>;
method(get) ->
    <<"GET">>;
method(put) ->
    <<"PUT">>;
method(post) ->
    <<"POST">>;
method(trace) ->
    <<"TRACE">>;
method(options) ->
    <<"OPTIONS">>;
method(delete) ->
    <<"DELETE">>.


build_headers(Host,_,_) ->
    [<<"Host: ">>,Host,?CRLF].
    %% [<<"Host: ">>,Host,?CRLF,
    %%  <<"Content-Length: 0">>, ?CRLF].
