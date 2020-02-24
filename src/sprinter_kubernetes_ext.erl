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

-module(sprinter_kubernetes_ext).

-author("Junghun Yoo <junghun.yoo@cs.ox.ac.uk>").

-include("sprinter.hrl").
-behaviour(sprinter_backend).

-export([clients/1,
         servers/1,
         upload_artifact/3,
         download_artifact/2]).

%% @private
upload_artifact(#state{eredis=Eredis}, Node, Membership) ->
    {ok, <<"OK">>} = eredis:q(Eredis, ["SET", Node, Membership]),
    % lager:info("Pushed artifact to Redis: ~p", [Node]),
    ok.

%% @private
download_artifact(#state{eredis=Eredis}, Node) ->
    % lager:info("Retrieving object ~p from redis.", [Node]),

    try
        case eredis:q(Eredis, ["GET", Node]) of
            {ok, Membership} ->
                % lager:info("Received artifact from Redis: ~p", [Node]),
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
clients(_State) ->
    EvalTimestamp = sprinter_config:get(evaluation_timestamp, 0),
    LabelSelector = "tag%3Dclient,evaluation-timestamp%3D" ++ integer_to_list(EvalTimestamp),
    pods_from_kubernetes(LabelSelector).

%% @private
servers(_State) ->
    EvalTimestamp = sprinter_config:get(evaluation_timestamp, 0),
    LabelSelector = "tag%3Dserver,evaluation-timestamp%3D" ++ integer_to_list(EvalTimestamp),
    pods_from_kubernetes(LabelSelector).

%% @private
pods_from_kubernetes(LabelSelector) ->
    DecodeFun = fun(Body) -> jsx:decode(Body, [return_maps]) end,

    case get_request(generate_pods_url(LabelSelector), DecodeFun) of
        {ok, PodList} ->
            generate_pod_nodes(PodList);
        Error ->
            _ = lager:info("Invalid response: ~p", [Error]),
            sets:new()
    end.

%% @private
generate_pods_url(LabelSelector) ->
    APIServer = os:getenv("APISERVER"),
    APIServer ++ "/api/v1/pods?labelSelector=" ++ LabelSelector.

%% @private
generate_pod_nodes(#{<<"items">> := Items}) ->
    case Items of
        null ->
            sets:new();
        _ ->
            Nodes = lists:foldr(
                fun(Item, Acc) ->

                    %% get name if defined
                    Name = case maps:is_key(<<"metadata">>, Item) of
                               true ->
                                   Metadata = maps:get(<<"metadata">>, Item),
                                   maps:get(<<"name">>, Metadata, undefined);
                               false ->
                                   undefined
                           end,

                    %% get pod ip if defined
                    PodIP = case maps:is_key(<<"status">>, Item) of
                                true ->
                                    Status = maps:get(<<"status">>, Item),
                                    maps:get(<<"podIP">>, Status, undefined);
                                false ->
                                    undefined
                            end,

                    case Name /= undefined andalso PodIP /= undefined of
                        true ->
                            [generate_pod_node(Name, PodIP) | Acc];
                        false ->
                            Acc
                    end
                end,
                [],
                Items
            ),
            sets:from_list(Nodes)
    end.

%% @private
generate_pod_node(Name, Host) ->
    {ok, _IPAddress} = inet_parse:address(binary_to_list(Host)),
    Port = list_to_integer(os:getenv("PEER_PORT", "9090")),
    #{name => list_to_atom(binary_to_list(Name) ++ "@" ++ binary_to_list(Host)),
      listen_addrs => [#{ip => binary_to_list(Name), port => Port}]}.
%      listen_addrs => [#{ip => IPAddress, port => Port}]}.

%% @private
get_request(Url, DecodeFun) ->
    Headers = headers(),
    SSLOptions = [{cacertfile, "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt"}],
    HTTPOptions = [{ssl, SSLOptions}],
    case httpc:request(get, {Url, Headers}, HTTPOptions, [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, DecodeFun(Body)};
        Other ->
            _ = lager:info("Request failed; ~p", [Other]),
            {error, invalid}
    end.

%% @private
headers() ->
    Token = os:getenv("TOKEN"),
    [{"Authorization", "Bearer " ++ Token}].
