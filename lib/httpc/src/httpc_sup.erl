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

%%%-------------------------------------------------------------------
%% @doc httpc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(httpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
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
child_specs(Config) ->
    [httpc_session_sup(Config), httpc_worker_sup()].

httpc_session_sup(Config) ->
    #{id => httpc_session_sup,
      start => {httpc_session_sup, start_link, [Config]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [httpc_session_sup]}.

httpc_worker_sup() ->
    #{id => httpc_worker_sup,
      start => {httpc_worker_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [httpc_worker_sup]}.
