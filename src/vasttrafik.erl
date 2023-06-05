-module(vasttrafik).
-export([departures/1]).

departures(StopGid) ->
    application:ensure_all_started(oauth2c),
    application:ensure_all_started(ssl),
    application:ensure_started(inets),
    ClientKey = list_to_binary(os:getenv("VTClient")),
    Secret    = list_to_binary(os:getenv("VTSecret")),
    io:format("all started! ~p~n~p~n", [ClientKey, jiffy:encode([1,3,4])]),
    Res = oauth2c:retrieve_access_token(
        <<"client_credentials">>, 
        <<"https://ext-api.vasttrafik.se/token">>,
        ClientKey, 
        Secret
    ),
    % io:format("Res: ~p~n", [Res]),
    {ok, _Headers, Client} = Res,
    io:format("Access token acquired~n"),
    Url = restc:construct_url("https://ext-api.vasttrafik.se/pr/v4/", 
                              "stop-areas/" ++ StopGid ++ "/departures", [
                                {<<"maxDeparturesPerLineAndDirection">>, <<"3">>},
                                {<<"limit">>, <<"20">>}
                              ]),
    io:format("Url: ~p~n", [Url]),
    {{ok, _, _, Result}, _} = oauth2c:request(get, json, Url, [200], Client),
    Result.