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

send(Uri,
     _Request = #{method := Method}, 
     _Options = #{socket := Socket}) ->
    Target = uri_string:recompose(Uri),
    Message = [method(Method), ?SP, Target, ?SP, <<"HTTP/1.1">>, ?CRLF],
    io:format("### Transport SEND: ~p~n", [Message]),
    Res = gen_tcp:send(Socket, Message),
    io:format("### Transport SEND RES: ~p~n", [Res]).


%%====================================================================
%% Internal functions
%%====================================================================

create_socket(#{host := Host, port := Port}, HTTPOpts, SocketOpts0) ->
    Timeout = maps:get(timeout, HTTPOpts),
    io:format("### Transport Create Socket: ~p~n", [Timeout]),
    SocketOpts = [ binary, {packet, http}, {active, false}, {reuseaddr, true}| SocketOpts0],
    {ok, Socket} = %gen_tcp:connect(Host, Port, SocketOpts, Timeout),
    try gen_tcp:connect(Host, Port, SocketOpts, Timeout) of
	{ok, _} = OK ->
	    OK;
	{error, _} = ERROR ->
	    ERROR
    catch 
	exit:{badarg, _} ->
            io:format("### Transport Exception: ~p~n", [badarg]),
	    {error, {eoptions, HTTPOpts}};
	exit:badarg ->
            io:format("### Transport Socket: ~p~n", [badarg]),
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



