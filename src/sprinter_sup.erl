%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Christopher S. Meiklejohn.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(sprinter_sup).
-author("Christopher Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(APP, sprinter).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    EvaluationIdDefault = list_to_atom(os:getenv("EVAL_ID", "undefined")),
    EvaluationId = application:get_env(?APP,
                                       evaluation_identifier,
                                       EvaluationIdDefault),
    sprinter_config:set(evaluation_identifier, EvaluationId),

    EvaluationTimestampDefault = list_to_integer(os:getenv("EVAL_TIMESTAMP", "0")),
    EvaluationTimestamp = application:get_env(?APP,
                                              evaluation_timestamp,
                                              EvaluationTimestampDefault),
    sprinter_config:set(evaluation_timestamp, EvaluationTimestamp),

    Backend = {sprinter_backend,
               {sprinter_backend, start_link, []},
                permanent, 5000, worker,
                [sprinter_backend]},

    {ok, { {one_for_all, 0, 1}, [Backend]} }.

%%====================================================================
%% Internal functions
%%====================================================================
