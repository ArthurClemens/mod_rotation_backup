%% @author Arthur Clemens
%% @copyright 2018 Arthur Clemens
%% @doc Deletion of archives

-module(rotation_backup_delete).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    delete/1,
    delete/2,
    
    % for testing:
    calculate_candidates/3
]).

-spec delete(Context) -> any() when
    Context:: #context{}.
delete(Context) ->
    Archives = rotation_backup_service:archives(Context),
    delete(Archives, Context).


-spec delete(Archives, Context) -> any() when
    Archives:: list(),
    Context:: #context{}.
delete(Archives, Context) ->
    Cfg = rotation_backup_service:check_configuration(Context),
    case proplists:get_value(ok, Cfg) of
        true ->
            ArchiveData = rotation_backup_service:archive_data(Archives, Context),
            % handle jobs
            lists:foreach(fun(Job) ->
                JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
                maybe_delete_for_job(Job, JobArchives, Context)
            end, rotation_backup_job:jobs());
        false ->
            mod_rotation_backup:debug("Rotation Backup is not configured properly.", Context)
    end.
    

-spec maybe_delete_for_job(Job, JobArchives, Context) -> any() when
    Job:: string(),
    JobArchives:: list(),
    Context:: #context{}.
maybe_delete_for_job(Job, JobArchives, Context) when JobArchives =:= [] ->
    mod_rotation_backup:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    mod_rotation_backup:debug("No archives found, nothing to delete.", Context);

maybe_delete_for_job(Job, JobArchives, Context) when length(JobArchives) =:= 1 ->
    mod_rotation_backup:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    mod_rotation_backup:debug("Only 1 archive exists, so nothing to delete.", Context);
    
maybe_delete_for_job(Job, JobArchives, Context) ->
    mod_rotation_backup:debug(io_lib:format("Assess delete job '~s'...", [Job]), Context),
    [{to_keep, _UniqueToKeep}, {to_remove, ToRemove}] = calculate_candidates(Job, JobArchives, Context),
    case length(ToRemove) of
        0 ->
            mod_rotation_backup:debug("No backups to remove.", Context);
        _ -> 
            ToRemoveNames = rotation_backup_archive:archive_names(ToRemove),
            do_delete(ToRemoveNames, Context)
    end.
    

-spec calculate_candidates(Job, JobArchives, Context) -> list(tuple()) when
    Job:: string(),
    JobArchives:: list(),
    Context:: #context{}.
calculate_candidates(Job, JobArchives, Context) ->    
    Intervals = rotation_backup_interval:intervals(Job, Context),
    IntervalData = rotation_backup_buckets:create_interval_data(Intervals),    
    Buckets = rotation_backup_buckets:distribute_archives(JobArchives, IntervalData),
    rotation_backup_buckets:debug(Buckets, Context),
    Buckets1 = rotation_backup_buckets:prune(Buckets),
    ToKeep = rotation_backup_buckets:bucket_archives(Buckets1),
    ToRemove = JobArchives -- ToKeep,
    debug_result("To keep", ToKeep, Context),
    debug_result("To remove", ToRemove, Context),
    [{to_keep, ToKeep}, {to_remove, ToRemove}].


-spec do_delete(Files, Context) -> list() when
    Files:: list(),
    Context:: #context{}.
do_delete(Files, Context) -> 
    mod_rotation_backup:debug(io_lib:format("These files will be removed: ~p", [Files]), Context),
    lists:map(fun(File) ->
        rotation_backup_service:remove(File, Context),
        timer:sleep(1)
    end, Files).


debug_result(Title, Archives, Context) ->
    mod_rotation_backup:dev_debug("---", Context),
    mod_rotation_backup:dev_debug(Title, Context),
    lists:map(fun(A) ->
        mod_rotation_backup:dev_debug("\t ~p", [A], Context)
    end, lists:sort(fun sort_by_date/2, Archives)).

% sort newest first
sort_by_date(A, B) ->
    proplists:get_value(date, B) =< proplists:get_value(date, A).


    