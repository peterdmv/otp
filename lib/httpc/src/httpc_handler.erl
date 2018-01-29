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
-module(httpc_handler).

-behaviour(gen_server).

%% API
-export([start_link/5,
         send/2]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state,
        {
          request,
          options,
          socket,
          socket_type,
          session_options,
          socket_options
        }).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Parent, _SessionName, Request, SessionOpts, SocketOpts) ->
    %% Opts = [],
    %% gen_server:start_link(?MODULE, {Request, Options}, Opts).
    {ok, proc_lib:start_link(?MODULE, init, [{Parent, Request, SessionOpts, SocketOpts}])}.

send(Pid, Request) ->
    gen_server:call(Pid, {request, Request}).

%%%=========================================================================
%%%  Gen_server callbacks
%%%=========================================================================
init({Parent, Request, SessionOpts, SocketOpts}) ->
    process_flag(trap_exit, true),
    %% Do not let initial tcp-connection block the manager-process
    proc_lib:init_ack(Parent, self()),
    %%
    %% TODO Init connection
    %%

    %% Initial state
    State0 = #state{session_options=SessionOpts,
                    socket_options=SocketOpts
                   },
    io:format("send request: ~p~n", [Request]),
    {ok, State} = send_request(Request, State0),


    %% Send initial fake http answer
    %% #{from := From, requestid := RequestId} = Request,
    %% self() ! {fake_http_answer, From, RequestId},
    

    %% State = #state{request=Request,
    %%                options=Options,
    %%                socket=Socket,
    %%                session_options=SessionOpts,
    %%                socket_options=SocketOpts
    %%               },

    gen_server:enter_loop(?MODULE, [], State).
    %% {ok, #state{request=Request, options=Options}}.

handle_call({request, _Request = #{from := From, requestid := RequestId}}, _, State) ->
    self() ! {fake_http_answer, From, RequestId},
    {reply, ok, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({fake_http_answer, From, RequestId}, State) ->
    From ! {http, {RequestId, {"200 OK", "test", ""}}},
    {noreply, State};
handle_info({http, Socket, HttpPacket}, State = #state{request=_Request}) ->
    io:format("HTTP: ~p~n", [HttpPacket]),
    %% From = maps:get(from, Request),
    inet:setopts(Socket, [{active, once}]),
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

send_request(Request0 = #{uri := Uri, http_opts := HTTPOpts},
             State0 = #state{session_options=_SessionOpts, socket_options=SocketOpts}) ->

    %% Create socket
    Socket = httpc_transport:create_socket(Uri, HTTPOpts, SocketOpts),
    Options = #{socket => Socket},
    case httpc_transport:send(Uri, Request0, Options) of
        ok ->
            %% update state
            Res = inet:setopts(Socket, [{active, once}]),
            io:format("# setopts {active, once} ~p~n", [Res]),
            {ok, State0#state{request=Request0}};
        {error, Reason} ->
            io:format("send error ~p~n", [Reason]),
            self() ! {init_error, error_connecting,
                      httpc_response:error(Request0, Reason)},
            {ok, State0#state{request=Request0}}
    end.


