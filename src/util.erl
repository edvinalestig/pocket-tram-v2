-module(util).
-export([get_gids/1]).


get_gids(Stops) ->
    Gids = #{
        "kapellplatsen" => "9021014003760000",
        "chalmers" => "9021014001960000",
        "lindholmen" => "9021014004490000"
    },
    [begin 
        {ok, Gid} = maps:find(binary_to_list(Stop), Gids), 
        Gid 
    end || Stop <- Stops].

% stops(Stop) ->
%     Stops = #{
%         "kapellplatsen" => ["kapellplatsen"]
%     }.