-module(vasttrafik).
-export([departures/1, departures/2]).

departures(StopGid) ->
    departures(StopGid, nil).

departures(StopGid, _) -> % DirectionGid
    application:ensure_all_started(oauth2c),
    application:ensure_all_started(ssl),
    application:ensure_started(inets),

    ClientKey = list_to_binary(os:getenv("VTClient")),
    Secret    = list_to_binary(os:getenv("VTSecret")),
    Res = oauth2c:retrieve_access_token(
        <<"client_credentials">>, 
        <<"https://ext-api.vasttrafik.se/token">>,
        ClientKey, 
        Secret
    ),
    {ok, _Headers, Client} = Res,
    Url = restc:construct_url("https://ext-api.vasttrafik.se/pr/v4/", 
                              "stop-areas/" ++ StopGid ++ "/departures", [
                                {<<"maxDeparturesPerLineAndDirection">>, <<"3">>},
                                {<<"limit">>, <<"100">>}
                              ]),
    {{ok, _, _, Result}, _} = oauth2c:request(get, json, Url, [200], Client),
    Result.