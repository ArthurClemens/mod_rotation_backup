%% @author Arthur Clemens
%% @copyright 2018 Arthur Clemens
%% @doc Test creating and deleting of archives

-module(rotation_backup_test).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    test_large_collection/1,
    test_many_generations/1,
    
    backup/2,
    delete/4,
    
    % testing
    months_back/1,
    days_back/1,
    hours_back/1
]).

-define(HOUR_SECONDS, 3600).
-define(DAY_SECONDS, 3600 * 24).
-define(RAND_UNIFORM(), rand:uniform()).
-define(RAND_UNIFORM(L), rand:uniform(L)).
-define(RAND_SEED(S1,S2,S3), rand:seed(exs1024, {S1,S2,S3})).

test_large_collection(Context) ->
    delete(40, 10, 1, Context).
    
test_many_generations(Context) ->
    delete(1, 400, 1, Context).

-spec backup(Count, Context) -> any() when
    Count:: integer(),
    Context:: #context{}.
backup(Count, Context) ->
    CreateFun = fun days_back/1,
    Archives = case Count of
        0 -> [];
        _ -> create_test_archives(CreateFun, Count, Context)
    end,
    ArchiveData = rotation_backup_service:archive_data(Archives, Context),
    Job = "database",
    JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
    mod_rotation_backup:dev_debug("ArchiveData=~p", [ArchiveData], Context),
    mod_rotation_backup:dev_debug("JobArchives=~p", [JobArchives], Context),
    case JobArchives of
        [] ->
            [Name, TmpDir] = rotation_backup_create:backup_name_dir(Job, Context),
            mod_rotation_backup:dev_debug("New backup: ~p, ~p", [Name, TmpDir], Context);
        _ -> 
            [Name, TmpDir] = rotation_backup_create:backup_name_dir(Job, Context),
            case rotation_backup_create:check_backup_needed(Job, JobArchives, Context) of
                true ->
                    mod_rotation_backup:dev_debug("New backup: ~p, ~p", [Name, TmpDir], Context);
                false ->
                    mod_rotation_backup:dev_debug("No backup", Context)
            end
    end.


-spec delete(Count, Repeat, DayJumps, Context) -> any() when
    Count:: integer(),
    Repeat:: integer(),
    DayJumps:: integer(),
    Context:: #context{}.
delete(Count, Repeat, DayJumps, Context) ->
    Job = "database",
    JobArchives = case Count > 0 of
        true ->
            CreateFun = fun days_back/1,
            Archives = create_test_archives(CreateFun, Count, Context),
            ArchiveData = rotation_backup_service:archive_data(Archives, Context),
            [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job];
        false -> []
    end,
    
%    mod_rotation_backup:dev_debug("JobArchives:", Context),
%    lists:map(fun(A) ->
%        mod_rotation_backup:dev_debug("\t ~p", [A], Context)
%    end, lists:sort(JobArchives)),
    
    lists:foldl(fun(Counter, JobArchives1) -> 
        % add a (day * DayJumps) for each repeat
        NowSeconds = now_seconds(),
        DaySeconds = NowSeconds + (Counter * DayJumps * ?DAY_SECONDS),
        TestBackup = generate_test_date(Job, DaySeconds, Context),
        [TestBackupData] = rotation_backup_service:archive_data([TestBackup], Context),
        JobArchives2 = [TestBackupData|JobArchives1],
        
        [{to_keep, UniqueToKeep}, {to_remove, _ToRemove}] = rotation_backup_delete:calculate_candidates(Job, JobArchives2, Context),
        mod_rotation_backup:dev_debug("-------", Context),
        UniqueToKeep
    end, JobArchives, lists:seq(1, Repeat)).


-spec create_test_archives(CreateFun, Count, Context) -> list() when
    CreateFun:: fun((_) -> list()),
    Count:: integer(),
    Context:: #context{}.
create_test_archives(CreateFun, Count, Context) ->
    TestDates = CreateFun(Count),
    Jobs = ["database"], %rotation_backup_job:jobs(),
    Archives = lists:foldl(fun(Date, Acc) ->
        lists:foldl(fun(Job, Acc1) ->
            NameData = create_test_archive(Job, Date, Context),
            [[NameData]|Acc1]
        end, Acc, Jobs)
    end, [], TestDates),
    lists:concat(Archives).
   

create_test_archive(Job, DateStr, Context) ->
    Identifier = rotation_backup_archive:identifier(Context),
    Identifier ++ "-" ++ Job ++ "-" ++ DateStr.


-spec months_back(MonthCount) -> list(non_neg_integer()) when
    MonthCount:: non_neg_integer().
months_back(MonthCount) ->
    NowSeconds = now_seconds(),
    lists:map(fun(D) -> 
        RandomRange = ?DAY_SECONDS * 30 * 0.5,
        Random = RandomRange - ?RAND_UNIFORM(round(2 * RandomRange)),
        Seconds = round(NowSeconds - (D * 30.3 * ?DAY_SECONDS) + Random),
        Date = calendar:gregorian_seconds_to_datetime(Seconds),
        format_date(Date)
    end, lists:seq(0, MonthCount)).


-spec days_back(DayCount) -> list(non_neg_integer()) when
    DayCount:: non_neg_integer().
days_back(DayCount) ->
    NowSeconds = now_seconds(),
    lists:map(fun(D) -> 
%        RandomRange = ?DAY_SECONDS * 0.5,
        Random = 0, %RandomRange - ?RAND_UNIFORM(round(2 * RandomRange)),
        Seconds = round(NowSeconds - (D * ?DAY_SECONDS) + Random),
        Date = calendar:gregorian_seconds_to_datetime(Seconds),
        format_date(Date)
    end, lists:seq(0, DayCount)).


-spec hours_back(HourCount) -> list(non_neg_integer()) when
    HourCount:: non_neg_integer().
hours_back(HourCount) ->
    NowSeconds = now_seconds(),
    lists:map(fun(H) -> 
        RandomRange = ?HOUR_SECONDS * 0.5,
        Random = RandomRange - ?RAND_UNIFORM(round(2 * RandomRange)),
        Seconds = round(NowSeconds - (H * ?HOUR_SECONDS) + Random),
        Date = calendar:gregorian_seconds_to_datetime(Seconds),
        format_date(Date)
    end, lists:seq(0, HourCount)).


generate_test_date(Job, DateSeconds, Context) ->
    RandomRange = ?HOUR_SECONDS * 7,
    Random = RandomRange - ?RAND_UNIFORM(round(2 * RandomRange)),
    Seconds = round(DateSeconds - Random),
    Date = calendar:gregorian_seconds_to_datetime(Seconds),
    DateStr = format_date(Date),
    create_test_archive(Job, DateStr, Context).


-spec now_seconds() -> integer().
now_seconds() ->
    Now = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Now).

-spec format_date(Date) -> string() when
    Date:: integer().
format_date(Date) ->
    qdate:to_string("Ymd-His", Date).
