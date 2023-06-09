-module(util).
-export([get_gid/1, 
         extract_info/1, 
         combine_departures/1, 
         sort/1, 
         stops/1]).


get_gid(Stop) ->
    Gids = #{
        "kapellplatsen"  => "9021014003760000",
        "chalmers"       => "9021014001960000",
        "lindholmen"     => "9021014004490000",
        "brunnsparken"   => "9021014001760000",
        "lindholmspiren" => "9021014004493000",
        nil              => nil
    },
    {ok, Gid} = maps:find(Stop, Gids), 
    Gid.

stops(Stop) ->
    Stops = #{
        "kapellplatsen" => [{"Kapellplatsen", "kapellplatsen", nil},
                            {"Chamlers", "chalmers", nil}],
        "chalmers"      => [{"Chalmers", "chalmers", nil}],
        "lindholmen"    => [{"Lindholmen", "lindholmen", nil},
                            {"Lindholmspiren", "lindholmspiren", nil}],
        "brunnsparken"  => [{"Brunnsparken", "brunnsparken", nil}]
    },
    case maps:find(binary_to_list(Stop), Stops) of
        error ->
            not_implemented;
        {ok, Res} ->
            [{Title, get_gid(S1), get_gid(S2)} || {Title, S1, S2} <- Res]
    end.

extract_info(Departures) ->
    extract_info(Departures, []).

extract_info([], Acc) ->
    Acc;
extract_info([Dep|Departures], Acc) ->
    % Departure time
    {<<"isCancelled">>, Cancelled} = lists:keyfind(<<"isCancelled">>, 1, Dep),
    Time = case lists:keyfind(<<"estimatedTime">>, 1, Dep) of
        false ->
            {<<"plannedTime">>, T} = lists:keyfind(<<"plannedTime">>, 1, Dep),
            #{
                time => T,
                realtime => false,
                cancelled => Cancelled
            };
        {<<"estimatedTime">>, T} -> 
            #{
                time => T,
                realtime => true,
                cancelled => Cancelled
            }
    end,

    {<<"serviceJourney">>, ServiceJourney} = lists:keyfind(<<"serviceJourney">>, 1, Dep),
    {<<"line">>, LineInfo} = lists:keyfind(<<"line">>, 1, ServiceJourney),

    % Other information
    {<<"direction">>,     Direction} = lists:keyfind(<<"direction">>, 1, ServiceJourney),
    {<<"shortName">>,          Name} = lists:keyfind(<<"shortName">>, 1, LineInfo),
    {<<"designation">>, Designation} = lists:keyfind(<<"designation">>, 1, LineInfo),
    {<<"backgroundColor">>, BgColor} = lists:keyfind(<<"backgroundColor">>, 1, LineInfo),
    {<<"foregroundColor">>, FgColor} = lists:keyfind(<<"foregroundColor">>, 1, LineInfo),
    {<<"borderColor">>, BorderColor} = lists:keyfind(<<"borderColor">>, 1, LineInfo),

    Info = #{
        time => [Time],
        direction => strip(Direction),
        name => Name,
        designation => Designation,
        bgColor => BgColor,
        fgColor => FgColor,
        borderColor => BorderColor
    },
    extract_info(Departures, [Info|Acc]).


strip(Str) ->
    Str2 = lists:nth(1, string:split(Str, " via")),
    Str3 = lists:nth(1, string:split(Str2, ", Påst")),
    lists:nth(1, string:split(Str3, ", Fri resa")).


combine_departures(Departures) ->
    lists:foldl(fun combine/2, [], Departures).


combine(Map, []) -> 
    [Map];
combine(Map, List) ->
    Counter = counters:new(1, []),
    #{name := Name, direction := Direction} = Map,
    NewList = [begin
        case (maps:get(name,      Elem) == Name) and 
             (maps:get(direction, Elem) == Direction) of
            false ->
                Elem;
            true ->
                counters:add(Counter, 1, 1),
                maps:merge_with(fun(Key, Val1, Val2) ->
                    case Key == time of
                        true ->
                            Val1 ++ Val2;
                        false ->
                            Val1
                    end
                end, Map, Elem)
        end
    end || Elem <- List],
    case counters:get(Counter, 1) > 0 of
        true  -> NewList;
        false -> [Map|NewList]
    end.


sort(List) ->
    List2 = lists:sort(fun(E1,E2) ->
        #{direction := Dir1} = E1,
        #{direction := Dir2} = E2,
        Dir1 < Dir2
    end, List),
    lists:sort(fun(A,B) ->
        #{name := NameA} = A,
        #{name := NameB} = B,
        NameA1 = binary_to_list(NameA),
        NameB1 = binary_to_list(NameB),
        N = max(length(NameA1), length(NameB1)),
        NameA2 = lists:concat(lists:duplicate(N-length(NameA1), "0")) ++ NameA1,
        NameB2 = lists:concat(lists:duplicate(N-length(NameB1), "0")) ++ NameB1,
        NameA2 =< NameB2
    end, List2).
