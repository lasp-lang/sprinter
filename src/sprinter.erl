%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Christopher Meiklejohn.  All Rights Reserved.
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

-module(sprinter).
-author("Christopher Meiklejohn <christopher.meiklejohn@gmail.com>").

%% API
-export([graph/0,
         tree/0,
         orchestration/0,
         orchestrated/0,
         was_connected/0,
         servers/0,
         nodes/0]).

graph() ->
    sprinter_backend:graph().

tree() ->
    sprinter_backend:tree().

orchestration() ->
    sprinter_backend:orchestration().

orchestrated() ->
    sprinter_backend:orchestrated().

was_connected() ->
    sprinter_backend:was_connected().

servers() ->
    sprinter_backend:servers().

nodes() ->
    sprinter_backend:nodes().
