-module(vasttrafik).
-export([departures/2, get_departures/1]).

departures(StopGid, _DirectionGid) ->
    application:ensure_all_started(oauth2c),
    application:ensure_all_started(ssl),
    application:ensure_started(inets),

    ClientKey = list_to_binary(os:getenv("VTClient")),
    Secret    = list_to_binary(os:getenv("VTSecret")),
    {ok, _Headers, Client} = oauth2c:retrieve_access_token(
        <<"client_credentials">>, 
        <<"https://ext-api.vasttrafik.se/token">>,
        ClientKey, 
        Secret
    ),
    Url = restc:construct_url("https://ext-api.vasttrafik.se/pr/v4/", 
                              "stop-areas/" ++ StopGid ++ "/departures", [
                                {<<"maxDeparturesPerLineAndDirection">>, <<"3">>},
                                {<<"limit">>, <<"100">>}
                              ]),
    {{ok, _, _, Result}, _} = oauth2c:request(get, json, Url, [200], Client),
    Result.

get_departures(Stops) ->
    Parent = self(),
    Pids = [
        {Title, spawn_link(fun() ->
            case catch get_and_process_departure(Start, Direction) of
                {'EXIT', _} -> 
                    Parent ! {self(), error};
                Dep ->
                    Parent ! {self(), Dep}
            end
        end)}
        || {Title, Start, Direction} <- Stops
    ],
    [receive {Pid, Res} -> #{title => list_to_binary(Title), departures => Res} end || {Title, Pid} <- Pids].

get_and_process_departure(Start, Direction) ->
    Deps = departures(Start, Direction),
    {<<"results">>, Results} = lists:keyfind(<<"results">>, 1, Deps),
    Extracted = util:extract_info(Results),
    Combined  = util:combine_departures(Extracted),
    util:sort(Combined).