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

-module(httpc_options).

-export([add_default_options/2,
         create_http_options/1,
         create_session_options/1,
         create_socket_options/1,
         default_http_options/0,
         default_session_options/0,
         default_socket_options/0
        ]).


create_http_options(HTTPOpts) ->
    add_default_options(default_http_options(), maps:from_list(HTTPOpts)).


create_session_options(SessionOpts) ->
    add_default_options(default_session_options(), maps:from_list(SessionOpts)).


create_socket_options(SocketOpts) ->
    add_default_options(default_socket_options(), maps:from_list(SocketOpts)).


add_default_options(Default, Options) ->
    Fun = fun(K,V) ->
                  case maps:is_key(K, Options) of
                      true ->
                          maps:get(K, Options);
                      false ->
                          V
                  end
          end,
    maps:map(Fun, Default).


default_http_options() ->
    #{version         => "HTTP/1.1",
      timeout         => infinity,
      autoredirect    => true,
      ssl             => [],
      proxy_auth      => undefined,
      relaxed         => false,
      connect_timeout => infinity,
      sync            => true,
      stream          => none,
      body_format     => string,
      full_result     => true,
      header_as_is    => false,
      receiver        => self()
     }.


default_session_options() ->
    #{proxy                 => undefined,
      https_proxy           => undefined,
      max_keep_alive_length => 5,
      keep_alive_timeout    => 120000,
      max_sessions          => 2,
      cookies               => disabled,
      verbose               => false}.


default_socket_options() ->
    #{ip => default,
      port => default,
      socket_opts => [binary,
                      {packet, http},
                      {active, true},
                      {reuseaddr, true}]}.
