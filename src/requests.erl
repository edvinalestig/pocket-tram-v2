-module(requests).
-behavior(cowboy_rest).

-export([init/2]).
-export([content_types_provided/2]).
-export([malformed_request/2]).
-export([to_json/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[
        {<<"application/json">>, to_json}
	], Req, State}.

malformed_request(Req, State) ->
    StopMap = catch cowboy_req:match_qs([stop], Req),
    case StopMap of
        {'EXIT', _} -> {true, Req, State};
        _ -> {false, Req, State}
    end.

to_json(Req, State) ->
    % io:format("~nBEGIN!!!~n"),
    Gid = "9021014003760000",
    Result = vasttrafik:departures(Gid),
    % io:format("Results received~n"),
    {<<"results">>, Results} = lists:keyfind(<<"results">>, 1, Result),
    % io:format("Encoding..~n~p~n", [jsx:is_json(Results)]),
    

    
    Body = jsx:encode(Results),
    % io:format("Encoded, sending...~n"),
    {Body, Req, State}.
    
