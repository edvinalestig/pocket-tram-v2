-module(pockettram2_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, pockettram2, "index.html"}},
            {"/test", test, []},
            {"/stop/[...]", cowboy_static, {priv_file, pockettram2, "stop.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, pockettram2, "static"}},
            {"/request", requests, []} % REST
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    pockettram2_sup:start_link().

stop(_State) ->
    ok.
