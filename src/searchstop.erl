-module(searchstop).
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
    % url?name= is necessary
    case catch cowboy_req:match_qs([name], Req) of
        {'EXIT', _} -> {true, Req, State};
        _ -> {false, Req, State}
    end.

to_json(Req, State) ->
    #{name := Name} = cowboy_req:match_qs([name], Req),
    Response = vasttrafik:searchstop(Name),
    Body = jsx:encode(Response),
    {Body, Req, State}.
