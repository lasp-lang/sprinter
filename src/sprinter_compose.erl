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

-module(sprinter_compose).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-include("sprinter.hrl").
-behavior(sprinter_backend).

-export([clients/1,
         servers/1,
         upload_artifact/3,
         download_artifact/2]).

%% @private
upload_artifact(#state{eredis=Eredis}, Node, Membership) ->
    {ok, <<"OK">>} = eredis:q(Eredis, ["SET", Node, Membership]),
    lager:info("Pushed artifact to Redis: ~p => ~p", [Node, Membership]),
    ok.

%% @private
download_artifact(#state{eredis=Eredis}, Node) ->
    lager:info("Retrieving object ~p from redis.", [Node]),

    try
        case eredis:q(Eredis, ["GET", Node]) of
            {ok, Membership} ->
                lager:info("Received artifact from Redis: ~p", [Node]),
                Membership;
            {error,no_connection} ->
                undefined
        end
    catch
        _:Error ->
            lager:info("Exception caught: ~p", [Error]),
            undefined
    end.

%% @private
clients(#state{eredis=Eredis}) ->
    case eredis:q(Eredis, ["KEYS", prefix("client/*")]) of
        {ok, Nodes} ->
            Nodes1 = lists:map(fun(N) ->
                N1 = binary_to_list(N),
                list_to_atom(string:substr(N1, length(prefix("client/")) + 1, length(N1)))
                end, Nodes),
            lager:info("Received client keys from Redis: ~p", [Nodes]),
            sets:from_list(Nodes1);
        {error,no_connection} ->
            sets:new()
    end.

%% @private
servers(#state{eredis=Eredis}) ->
    case eredis:q(Eredis, ["KEYS", prefix("server/*")]) of
        {ok, Nodes} ->
            Nodes1 = lists:map(fun(N) ->
                N1 = binary_to_list(N),
                list_to_atom(string:substr(N1, length(prefix("server/")) + 1, length(N1)))
                end, Nodes),
            lager:info("Received server keys from Redis: ~p", [Nodes]),
            sets:from_list(Nodes1);
        {error,no_connection} ->
            sets:new()
    end.

%% @private
prefix(File) ->
    EvalIdentifier = sprinter_config:get(evaluation_identifier, undefined),
    EvalTimestamp = sprinter_config:get(evaluation_timestamp, 0),
    "sprinter" ++ "/" ++ atom_to_list(EvalIdentifier) ++ "/" ++ integer_to_list(EvalTimestamp) ++ "/" ++ File.