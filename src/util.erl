-module(util).
-export([get_gids/1, extract_info/1]).


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

extract_info(Departures) ->
    extract_info(Departures, []).

extract_info([], Acc) ->
    Acc;
extract_info([Dep|Departures], Acc) ->
    % Departure time
    Time = case lists:keyfind(<<"estimatedTime">>, 1, Dep) of
        false ->
            {<<"plannedTime">>, T} = lists:keyfind(<<"plannedTime">>, 1, Dep),
            #{
                time => T,
                realtime => false
            };
        {<<"estimatedTime">>, T} -> 
            #{
                time => T,
                realtime => true
            }
    end,

    {<<"serviceJourney">>, ServiceJourney} = lists:keyfind(<<"serviceJourney">>, 1, Dep),
    {<<"line">>, LineInfo} = lists:keyfind(<<"line">>, 1, ServiceJourney),

    % Other information
    {<<"isCancelled">>,    Cancelled} = lists:keyfind(<<"isCancelled">>, 1, Dep),
    {<<"direction">>,      Direction} = lists:keyfind(<<"direction">>, 1, ServiceJourney),
    {<<"shortName">>,           Name} = lists:keyfind(<<"shortName">>, 1, LineInfo),
    {<<"designation">>,  Designation} = lists:keyfind(<<"designation">>, 1, LineInfo),
    {<<"backgroundColor">>, BgColour} = lists:keyfind(<<"backgroundColor">>, 1, LineInfo),
    {<<"foregroundColor">>, FgColour} = lists:keyfind(<<"foregroundColor">>, 1, LineInfo),
    {<<"borderColor">>, BorderColour} = lists:keyfind(<<"borderColor">>, 1, LineInfo),


    Info = #{
        cancelled => Cancelled,
        time => Time,
        direction => Direction,
        name => Name,
        designation => Designation,
        bgColour => BgColour,
        fgColour => FgColour,
        borderColour => BorderColour
    },
    extract_info(Departures, [Info|Acc]).