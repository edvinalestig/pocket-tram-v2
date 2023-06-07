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
    % Gid = "9021014003760000",
    #{stop := Stop} = cowboy_req:match_qs([stop], Req),
    Gid = util:get_gids([Stop]),

    Result = vasttrafik:departures(Gid),
    {<<"results">>, Results} = lists:keyfind(<<"results">>, 1, Result),
    
    Extracted = util:extract_info(Results),
    Combined  = util:combine_departures(Extracted),
    Sorted    = util:sort(Combined),

    Body = jsx:encode(Sorted),
    {Body, Req, State}.
    
