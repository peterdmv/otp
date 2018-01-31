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
          request,       % Original HTTP request
          options,
          socket,
          socket_type,
          session_options,
          socket_options,
          status_line,   % HTTP response
          headers=[],    % HTTP response
          body = <<>>,   % HTTP response
          content_length,
          rstate='start' % request state
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

handle_call({request, Request}, _, State0) ->
    %% self() ! {fake_http_answer, From, RequestId},
    io:format("send request: ~p~n", [Request]),
    {ok, State} = send_request(Request, State0),
    {reply, ok, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

%% handle_info({fake_http_answer, From, RequestId}, State) ->
%%     From ! {http, {RequestId, {"200 OK", "test", ""}}},
%%     {noreply, State};

handle_info({http, Socket, HttpPacket},
            State0 = #state{request=_Request}) ->
    handle_http_msg(Socket, HttpPacket, State0);
handle_info({tcp, Socket, Data},
            State0 = #state{request=_Request}) ->
    handle_http_msg(Socket, Data, State0);


handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

handle_http_msg(Socket, {http_response,{1,1},Code,Reason},
                State0 = #state{rstate='waiting_response'}) ->
    io:format("HTTP response: ~p~n", [Code]),
    inet:setopts(Socket, [{active, once}]),
    %% TODO get version
    StatusLine = {"HTTP/1.1", Code, Reason},
    {noreply, State0#state{status_line=StatusLine, rstate='waiting_headers'}};
handle_http_msg(Socket, {http_header, _, Name, _, Value},
                State0 = #state{rstate='waiting_headers'}) ->
    io:format("HTTP header: ~p ~p~n", [Name, Value]),
    inet:setopts(Socket, [{active, once}]),
    Headers = [{Name, Value}|State0#state.headers],
    %% Handle Content-Length
    State1 =
        case Name of
            'Content-Length' ->
                State0#state{content_length=list_to_integer(Value)};
            _ ->
                State0
        end,
    {noreply, State1#state{headers=Headers, rstate='waiting_headers'}};
handle_http_msg(Socket, http_eoh,
                State0 = #state{rstate='waiting_headers'}) ->
    io:format("HTTP End of Headers~n"),
    inet:setopts(Socket, [{packet, raw},{active, once}]),
    {noreply, State0#state{rstate='waiting_body'}};
handle_http_msg(Socket, Data,
                State0 = #state{rstate='waiting_body',
                                content_length=Size0}) when Size0 > 0 ->
    inet:setopts(Socket, [{active, once}]),
    handle_message_body(Data, State0).


%%-------------------------------------------------------------------------
%% [RFC 7230, Chapter 3.3. Message Body]
%%-------------------------------------------------------------------------
handle_message_body(Data, State0 = #state{content_length=Size0,
                                          status_line={_,200,_}}) when Size0 > 0 ->
    io:format("HTTP body part received (~p bytes)~n", [byte_size(Data)]),
    Body0 = State0#state.body,
    Body1 = <<Body0/binary,Data/binary>>,
    Size1 = Size0 - byte_size(Data),
    case Size1 of
        0 ->
            send_response(State0),
            {noreply, State0#state{rstate='start',
                                   status_line=undefined,
                                   headers=[],
                                   body= <<>>}};
        _ ->
            {noreply, State0#state{content_length=Size1,body=Body1}}
    end.



send_response(#state{request= #{from := From, requestid := RequestId},
                    status_line=StatusLine,
                    headers=Headers,
                    body=Body}) ->
    From ! {http, {RequestId, {StatusLine, Headers, Body}}}.


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
            {ok, State0#state{request=Request0,rstate='waiting_response'}};
        {error, Reason} ->
            io:format("send error ~p~n", [Reason]),
            self() ! {init_error, error_connecting,
                      httpc_response:error(Request0, Reason)},
            {ok, State0#state{request=Request0}}
    end.
