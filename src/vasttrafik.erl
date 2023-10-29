-module(vasttrafik).
-export([departures/2, get_departures/1, searchstop/1]).
-export([get_gid/1]).
-export([util_departures/2]).

get_token() ->
    application:ensure_all_started(oauth2c),
    application:ensure_all_started(ssl),
    application:ensure_started(inets),

    % Get OAuth credentials from environment variables
    ClientKey = list_to_binary(os:getenv("VTClient")),
    Secret    = list_to_binary(os:getenv("VTSecret")),
    % Get token from Västtrafik
    {ok, _Headers, Client} = oauth2c:retrieve_access_token(
        <<"client_credentials">>, 
        <<"https://ext-api.vasttrafik.se/token">>,
        ClientKey, 
        Secret
    ),
    Client.

departures(StopGid, DirectionGid) ->
    Client = get_token(),
    % Create the request URL and send to Västtrafik
    Url = restc:construct_url("https://ext-api.vasttrafik.se/pr/v4/", 
                              "stop-areas/" ++ StopGid ++ "/departures", [
                                {<<"maxDeparturesPerLineAndDirection">>, <<"3">>},
                                {<<"limit">>, <<"100">>},
                                {<<"directionGid">>, DirectionGid} % Kräver just nu att den finns
                              ]),
    {{ok, _, _, Result}, _} = oauth2c:request(get, json, Url, [200], Client),
    Result.

get_departures(Stops) ->
    Parent = self(),
    Pids = [
        % Start one process for each stop/direction
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
    % Receive the replies in the correct order
    [receive {Pid, Res} -> #{title => list_to_binary(Title), departures => Res} end || {Title, Pid} <- Pids].

get_and_process_departure(Start, Direction) ->
    % Get departures from Västtrafik
    Deps = departures(Start, Direction),
    % Extract the result part of response
    {<<"results">>, Results} = lists:keyfind(<<"results">>, 1, Deps),
    % Extract the important data points
    Extracted = util:extract_info(Results),
    % Combine the departures line-wise
    Combined  = util:combine_departures(Extracted),
    % Put departures in ascending line order
    util:sort(Combined).

searchstop(Stop) ->
    Client = get_token(),
    % Create the request URL and send to Västtrafik
    Url = restc:construct_url("https://ext-api.vasttrafik.se/pr/v4/", 
                              "locations/by-text", [
                                {<<"q">>, Stop},
                                {<<"types">>, <<"stoparea">>}
                              ]),
    {{ok, _, _, Result}, _} = oauth2c:request(get, json, Url, [200], Client),
    Result.

get_gid(StopName) ->
    Json = searchstop(StopName),
    {<<"results">>, Results} = lists:keyfind(<<"results">>, 1, Json),
    [S|_] = Results,
    {<<"gid">>, Gid} = lists:keyfind(<<"gid">>, 1, S),
    {<<"name">>, Name} = lists:keyfind(<<"name">>, 1, S),
    {Name, Gid}.

util_departures(StopGid, DateTime) ->
    Client = get_token(),
    % Create the request URL and send to Västtrafik
    Url = restc:construct_url("https://ext-api.vasttrafik.se/pr/v4/", 
                              "stop-areas/" ++ binary_to_list(StopGid) ++ "/departures", [
                                {<<"maxDeparturesPerLineAndDirection">>, <<"100">>},
                                {<<"limit">>, <<"20">>},
                                {<<"timeSpanInMinutes">>, <<"1439">>},
                                {<<"startDateTime">>, DateTime}
                              ]),
    {{ok, _, _, Result}, _} = oauth2c:request(get, json, Url, [200], Client),
    {<<"results">>, Departures} = lists:keyfind(<<"results">>, 1, Result),
    Departures.
