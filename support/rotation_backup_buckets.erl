%% @author Arthur Clemens
%% @copyright 2018 Arthur Clemens
%% @doc Interval time spans

-module(rotation_backup_buckets).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    distribute_archives/2,
    prune/1,
    create_interval_data/1,
    debug/2,
    bucket_archives/1
]).

-define(ST_JUTTEMIS_SECONDS, calendar:datetime_to_gregorian_seconds({{9999,8,17}, {12,0,0}})).


distribute_archives(Archives, IntervalData) ->
    SortedArchives = lists:sort(fun sort_by_date/2, Archives),
    MostRecentArchive = lists:nth(1, SortedArchives),
    MostRecentArchiveDate = proplists:get_value(date_seconds, MostRecentArchive),    
    {BucketData, _} = lists:foldl(fun(Interval, {Buckets, RemainingArchives}) ->
        % dates relative to most recent archive, backward in time
        BucketStart = MostRecentArchiveDate - proplists:get_value(start_date, Interval),
        BucketEnd = MostRecentArchiveDate - proplists:get_value(end_date, Interval),
        MatchingArchives = lists:filter(fun(A) ->
            ArchiveDate = proplists:get_value(date_seconds, A),
            (ArchiveDate =< BucketStart) and (ArchiveDate > BucketEnd)
        end, RemainingArchives),
        Bucket = [{archives, MatchingArchives}|Interval],
        {[Bucket|Buckets], RemainingArchives -- MatchingArchives}
    end, {[], SortedArchives}, IntervalData),
    lists:sort(fun sort_by_start_date/2, BucketData).


prune(Buckets) ->
    Pruned = lists:foldl(fun(Bucket, Acc) ->
        Archives = proplists:get_value(archives, Bucket),
        Count = proplists:get_value(count, Bucket),
        DistributionInterval = proplists:get_value(distribution, Bucket),
        Sorted = lists:sort(fun sort_by_date/2, Archives),
        Keep = case Sorted of 
            [] -> [[]];
            [A] -> [A];
            _ ->
                % keep the newest item (next gen)
                % then process the rest from old to new
                % first write out between intervals before and after
                Combined = lists:zip3(
                    [[]] ++ Sorted ++ [[]], % orginal
                    [[]] ++ [[]] ++ Sorted, % before
                    Sorted ++ [[]] ++ [[]] % after
                ),
                WithBeforeAfter = lists:foldl(fun({O, B, A}, Acc1) ->
                    case O of
                        [] -> Acc1;
                        _ -> 
                            OriginalDate = proplists:get_value(date_seconds, O),
                            BeforeDate = proplists:get_value(date_seconds, B),
                            AfterDate = proplists:get_value(date_seconds, A),
                            BeforeProp = case (OriginalDate =/= undefined) and (BeforeDate =/= undefined) of
                                false -> [];
                                true -> [{interval_before, abs(OriginalDate - BeforeDate)}]
                            end,
                            AfterProp = case (OriginalDate =/= undefined) and (AfterDate =/= undefined) of
                                false -> [];
                                true -> [{interval_after, abs(OriginalDate - AfterDate)}]
                            end,
                            [BeforeProp ++ AfterProp ++ O|Acc1]
                    end
                end, [], Combined),
                [First|Rest] = lists:sort(fun sort_by_date/2, WithBeforeAfter),
                {Remaining, _} = lists:foldl(fun(A, {Acc2, LastDate}) ->
                    Date = proplists:get_value(date_seconds, A),
                    case LastDate of
                        undefined ->
                            % keep the last item
                            {[clean_interval_props(A)|Acc2], Date};
                        _ -> 
                            % allow for a little bit of randomization in interval input
                            MaxInterval = DistributionInterval * 0.9,
                            Before = proplists:get_value(interval_before, A, ?ST_JUTTEMIS_SECONDS),
                            After = proplists:get_value(interval_after, A, ?ST_JUTTEMIS_SECONDS),
                            ExpireCondition =
                                (Before < MaxInterval) and
                                (After < MaxInterval) and
                                (((Before + After) / 2) < MaxInterval),
                            ExpireCondition1 = case (Count =/= undefined) of
                                true -> ExpireCondition and (length(Archives) > Count);
                                false -> ExpireCondition
                            end,
                            case ExpireCondition1 of
                                true ->
                                    {Acc2, Date};
                                false ->
                                    {[clean_interval_props(A)|Acc2], Date}
                            end
                    end
                end, {[clean_interval_props(First)], undefined}, lists:reverse(Rest)),
                Remaining
        end,
        [[{archives, Keep}|proplists:delete(archives, Bucket)]|Acc]
    end, [], Buckets),
    Pruned.


create_interval_data(Intervals) ->
    [_|Durations] = Intervals,
    Combined = lists:zip(Intervals, Durations ++ [{x, undefined}]),
    {IntervalData, _} = lists:foldl(fun({{Key, IntervalSeconds}, {_, Duration}}, {AccData, StartDate}) ->
        Count = case Duration of
            undefined -> undefined;
            _ -> round(Duration / IntervalSeconds) - 1 % subtract 1 because we will have next gens
        end,
        EndDate = case Duration of
            undefined -> ?ST_JUTTEMIS_SECONDS;
            _ -> StartDate + Duration
        end,
        Data = [
            {key, Key},
            {start_date, StartDate},
            {end_date, EndDate},
            {count, Count},
            {distribution, IntervalSeconds}
        ],
        {[Data|AccData], EndDate}
    end, {[], 0}, Combined),
    lists:reverse(IntervalData).


bucket_archives(Buckets) ->
    lists:foldl(fun(B, Acc) ->
        case proplists:get_value(archives, B) of
            [[]] -> Acc;
            A -> A ++ Acc
        end
    end, [], Buckets).
    

bucket_info(BucketData) ->
  lists:map(fun(Bucket) ->
      Key = proplists:get_value(key, Bucket),
      StartDate = proplists:get_value(start_date, Bucket),
      Distribution = proplists:get_value(distribution, Bucket),
      EndDate = proplists:get_value(end_date, Bucket),
      Count = proplists:get_value(count, Bucket),
      CountText = case Count of
        undefined -> "Not specified";
        _ -> Count
      end,
      Archives = proplists:get_value(archives, Bucket),
      DaySeconds = 24 * 3600,
      [{key, Key}, {start_date, StartDate/DaySeconds}, {end_date, EndDate/DaySeconds}, {count, CountText}, {interval, Distribution/DaySeconds}, {archives, Archives}]
  end, BucketData).

debug(BucketData, Context) ->
    mod_rotation_backup:dev_debug("BucketData: key: start day - end day (count x interval):", Context),
    lists:map(fun(Bucket) ->
        Key = proplists:get_value(key, Bucket),
        StartDate = proplists:get_value(start_date, Bucket),
        Distribution = proplists:get_value(distribution, Bucket),
        EndDate = proplists:get_value(end_date, Bucket),
        Count = proplists:get_value(count, Bucket),
        CountText = case Count of
          undefined -> "Not specified";
          _ -> Count
        end,
        Archives = proplists:get_value(archives, Bucket),
        DaySeconds = 24 * 3600,
        mod_rotation_backup:dev_debug("\t ~p: ~p - ~p (~p x ~p)", [Key, StartDate/DaySeconds, EndDate/DaySeconds, CountText, Distribution/DaySeconds], Context),
        case Archives of
            [] -> mod_rotation_backup:dev_debug("\t\t no archives", Context);
            _ -> 
                lists:map(fun(A) -> 
                    mod_rotation_backup:dev_debug("\t\t ~p", [A], Context)
                end, Archives)
        end
    end, BucketData).


clean_interval_props(List) ->
    lists:foldl(fun(P, List1) ->
        proplists:delete(P, List1)
    end, List, [interval_before, interval_after]).

% sort newest first
sort_by_date(A, B) ->
    proplists:get_value(date, B) =< proplists:get_value(date, A).


sort_by_start_date(A, B) ->
    proplists:get_value(start_date, A) =< proplists:get_value(start_date, B).


