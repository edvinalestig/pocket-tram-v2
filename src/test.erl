-module(test).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    #{name := Name} = cowboy_req:match_qs([name], Req0),
    io:format("TEST!!!!~n"),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Name,
        Req0),
    {ok, Req, State}.
