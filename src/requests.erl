-module(requests).
-behavior(cowboy_rest).

-export([init/2]).
-export([content_types_provided/2]).
-export([malformed_request/2]).
-export([to_json/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[{<<"application/json">>, to_json}], Req, State}.

malformed_request(Req, State) ->
    % url?stop= is necessary
    case catch cowboy_req:match_qs([stop], Req) of
        {'EXIT', _} -> {true, Req, State};
        _ -> {false, Req, State}
    end.

to_json(Req, State) ->
    #{stop := Stop} = cowboy_req:match_qs([stop], Req),

    % Get the stops/directions for the requested area
    Stops = util:stops(Stop),
    % Get the processed departures
    Deps = vasttrafik:get_departures(Stops),

    Body = jsx:encode(Deps),
    {Body, Req, State}.
    
