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
-module(httpc_session_sup).

-behaviour(supervisor).
-include("httpc_internal.hrl").

%% API
-export([start_link/1]).
-export([start_child/1]).

%% Supervisor callback
-export([init/1]).


%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

start_child(Config) ->
    supervisor:start_child(?MODULE, httpc_manager_spec(Config)).


%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([]) ->
    init([[]]);
init([Config]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 3600},
    {ok, {SupFlags, child_specs(Config)}}.


%%====================================================================
%% Internal functions
%%====================================================================
child_specs(Config) when is_list(Config) ->
    [httpc_manager_spec(Config)].

httpc_manager_spec(Config) ->
    Session = proplists:get_value(session, Config, ?DEFAULT_SESSION),
    #{id => {httpc, Session},
      start => {httpc_manager, start_link, [Session]},
      restart => permanent,
      shutdown => 4000,
      type => worker,
      modules => [httpc_manager]}.

