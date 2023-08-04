-module(departurepage).
-behavior(cowboy_handler).

-export([init/2]).

mapget(Key, Map) ->
    case lists:keyfind(list_to_binary(Key), 1, Map) of
        {_,V} -> V;
        _     -> "-"
    end.

mapgetr([Key], Map) ->
    mapget(Key, Map);
mapgetr([Key|Keys], Map) ->
    mapgetr(Keys, mapget(Key, Map)).

get_dir(Dep) ->
    [Dir|_] = string:split(mapgetr(["serviceJourney", "direction"], Dep), <<", Påstigning fram"/utf8>>),
    Dir.

get_delay(Dep) ->
    case mapget("isCancelled", Dep) of
        true ->
            " X";
        _  ->
            case mapget("estimatedTime", Dep) of
               "-" ->
                    "";
                ETime ->
                    PTime = binary_to_list(mapget("plannedTime", Dep)),
                    Diff = round((calendar:rfc3339_to_system_time(binary_to_list(ETime)) - calendar:rfc3339_to_system_time(PTime))/60),
                    if
                        Diff >= 0 ->
                            "+" ++ integer_to_list(Diff);
                        true ->
                            integer_to_list(Diff)
                    end
            end
    end.

rfc3339_time(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [Year, Month, Day, Hour, Min, Sec]).

get_date_time_from_rfc339(DateTime) ->
    [Date, TimePlus] = string:split(DateTime, "T"),
    [Time,_] = re:split(TimePlus, ":[0-9]{2}\\."),
    {Date, Time}.

get_time(DateTime) ->
    {_, T} = get_date_time_from_rfc339(DateTime),
    T.

init(Req0, State) ->
    #{stop := Name, time := Time, date := Date} = cowboy_req:match_qs([stop, time, date], Req0),

    {StopName, StopGid} = vasttrafik:get_gid(Name),
    DateTime = if
        (Time == <<"">>) and (Date == <<"">>) ->
            <<"">>;
        Time == <<"">> ->
            [Y,Mo,D]     = lists:map(fun binary_to_integer/1, string:split(Date, "-", all)),
            {_,{H,Mi,S}} = calendar:local_time(),
            [UTCTime]    = calendar:local_time_to_universal_time_dst({{Y,Mo,D},{H,Mi,S}}),
            rfc3339_time(UTCTime);
        Date == <<"">> ->
            [H,Mi]       = lists:map(fun binary_to_integer/1, string:split(Time, ":")),
            {{Y,Mo,D},_} = calendar:local_time(),
            [UTCTime]    = calendar:local_time_to_universal_time_dst({{Y,Mo,D},{H,Mi,0}}),
            rfc3339_time(UTCTime);
        true ->
            [Y,Mo,D]  = lists:map(fun binary_to_integer/1, string:split(Date, "-", all)),
            [H,Mi]    = lists:map(fun binary_to_integer/1, string:split(Time, ":")),
            [UTCTime] = calendar:local_time_to_universal_time_dst({{Y,Mo,D},{H,Mi,0}}),
            rfc3339_time(UTCTime)
    end,
    Departures = vasttrafik:util_departures(StopGid, DateTime),

    DisplayTime = DateTime,
    HTMLHead = ["<!DOCTYPE html>\n<html lang=\"sv\">\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width,",
            <<" initial-scale=1.0\">\n<title>Avgångar</title>\n</head>\n<body style=\"font-family: sans-serif\">"/utf8>>,
            <<"<a href='/util'>Till sökruta</a>"/utf8>>,
            "<h2>", StopName, " - ", DisplayTime,"</h2>" %,
            % "<a href='/'>Ankomster</a>" % /findArrivals?{'&'.join([f'{k}={v}' for k,v in args.items()])}
        ],
    HTMLBody = case length(Departures) of
        0 -> <<"<p>Hittade inga avgångar!</p>"/utf8>>;
        _ ->
            DepRows = [[
                "\n<tr style=\"border: 5px solid red;\">",
                "<td style='background-color: ", mapgetr(["serviceJourney", "line", "backgroundColor"], Dep), 
                "; color: ", mapgetr(["serviceJourney", "line", "foregroundColor"], Dep),
                "; text-align: center; border: 1px solid ", mapgetr(["serviceJourney", "line", "borderColor"], Dep), ";'>", 
                mapgetr(["serviceJourney", "line", "shortName"], Dep), "</td>",
                "<td><a href='/simpleDepInfo?ref=", mapget("detailsReference", Dep), "'>", 
                get_dir(Dep), "</a></td>",
                "<td style='text-align: center;'>", get_time(mapget("plannedTime", Dep)), get_delay(Dep), "</td>",
                "<td style='text-align: center;'>", mapgetr(["stopPoint", "platform"], Dep), "</td>",
                "</tr>"
             ] || Dep <- Departures],
             {Date2, Time2} = get_date_time_from_rfc339(mapget("plannedTime", lists:last(Departures))),
             ["\n<table>", <<"\n<tr><th>Linje</th><th>Destination</th><th>Tid</th><th>Läge</th></tr>"/utf8>>] ++
             DepRows ++
             ["\n</table><br><a href='/findDepartures?stop=", Name, "&time=", Time2,
             "&date=", Date2, <<"'>Fler avgångar</a>"/utf8>>, "\n</body>\n</html>"]
    end,

    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"text/html">>},
        HTMLHead ++ HTMLBody,
        Req0
    ),
	{ok, Req, State}.

