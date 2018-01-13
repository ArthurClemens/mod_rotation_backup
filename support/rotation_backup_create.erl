%% @author Arthur Clemens
%% @copyright 2018 Arthur Clemens
%% @doc Creation of archives

-module(rotation_backup_create).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    backup/1,
    backup/2,
    
    % for testing:
    check_backup_needed/3,
    backup_name_dir/2
]).


-spec backup(Context) -> any() when
    Context:: #context{}.
backup(Context) ->
    Archives = rotation_backup_service:archives(Context),
    backup(Archives, Context).


-spec backup(Archives, Context) -> any() when
    Archives:: list(),
    Context:: #context{}.
backup(Archives, Context) ->
    Cfg = rotation_backup_service:check_configuration(Context),
    mod_rotation_backup:dev_debug("backup, Archives=~p", [Archives], Context),
    case proplists:get_value(ok, Cfg) of
        true ->
            ArchiveData = rotation_backup_service:archive_data(Archives, Context),
            mod_rotation_backup:dev_debug("ArchiveData=~p", [ArchiveData], Context),
            % handle jobs
            lists:foldl(fun(Job, Acc) ->
                JobArchives = [JobData || JobData <- ArchiveData, proplists:get_value(job, JobData) =:= Job],
                [maybe_backup_for_job(Job, JobArchives, Context)|Acc]
            end, [], rotation_backup_job:jobs());
        false ->
            mod_rotation_backup:debug("Rotation Backup is not configured properly.", Context)
    end.


-spec maybe_backup_for_job(Job, JobArchives, Context) -> {error, string()} | {ok, string()} when
    Job:: string(),
    JobArchives:: list(),
    Context:: #context{}.
maybe_backup_for_job(Job, JobArchives, Context) when JobArchives =:= []->
    mod_rotation_backup:debug("No archives found. A new archive will be created.", Context),
    [Name, TmpDir] = backup_name_dir(Job, Context),
    do_backup(Name, TmpDir, Job, Context);
    
maybe_backup_for_job(Job, JobArchives, Context) ->
    mod_rotation_backup:debug(io_lib:format("Assess backup job '~s'...", [Job]), Context),
    [Name, TmpDir] = backup_name_dir(Job, Context),
    case check_backup_needed(Job, JobArchives, Context) of
        true ->
            do_backup(Name, TmpDir, Job, Context);
        false ->
            mod_rotation_backup:debug("No backup needed.", Context),
            {ok, "No backup needed"}
    end.
    

-spec check_backup_needed(Job, JobArchives, Context) -> boolean() when
    Job:: string(),
    JobArchives:: list(),
    Context:: #context{}.
check_backup_needed(Job, JobArchives, Context) ->
    Sorted = lists:reverse(lists:sort(JobArchives)),
    [NewestArchive|_Rest] = Sorted,
    
    % take the smallest interval
    {_, IntervalSeconds0} = rotation_backup_interval:smallest(Job, Context),
    IntervalSeconds = round(IntervalSeconds0),
    NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    NewestArchiveSeconds = rotation_backup_archive:date_seconds(NewestArchive),    
    NextBackupSeconds = NewestArchiveSeconds + IntervalSeconds,
    Diff = NewestArchiveSeconds + IntervalSeconds - NowSeconds,
    case (NextBackupSeconds < NowSeconds) of 
        true ->
            mod_rotation_backup:debug(io_lib:format("The smallest interval is set to ~.2f hours. The most recent archive ~p is ~.2f hours older. A new backup is needed.", [IntervalSeconds/3600, proplists:get_value(archive, NewestArchive), abs(Diff/3600)]), Context),
            true;
        false ->
            mod_rotation_backup:debug(io_lib:format("The smallest interval is set to ~.2f hours. The most recent archive ~p is ~.2f hours younger. No new backup is needed.", [IntervalSeconds/3600, proplists:get_value(archive, NewestArchive), abs(Diff/3600)]), Context),
            false
    end.


-spec backup_name_dir(Job, Context) -> list() when
    Job:: string(),
    Context:: #context{}.
backup_name_dir(Job, Context) ->
    Name = rotation_backup_archive:name(Job, Context),
    TmpDir = z_path:files_subdir_ensure("processing", Context),
    [Name, TmpDir].


-spec do_backup(Name, TmpDir, Job, Context) -> {error, string()} | {ok, string()} when
    Name:: string(),
    TmpDir:: string(),
    Job:: string(),
    Context:: #context{}.
do_backup(Name, TmpDir, Job, Context) ->
    TmpPath = create_archive(Name, TmpDir, Job, Context),
    case TmpPath of
        undefined ->
            z:warning(io_lib:format("Could not create temp path for job: ~s", [Job]), [{module, mod_rotation_backup}], Context),
            {error, "Could not create archive"};
        _ -> 
            case rotation_backup_service:store(TmpPath, Context) of
                {ok, Result} -> 
                    z:info(io_lib:format("Completed backup: ~s", [Job]), [{module, mod_rotation_backup}], Context),
                    ok = file:delete(TmpPath),
                    {ok, Result};
                {error, Reason} ->
                    z:info(io_lib:format("Error creating backup: ~s", [Reason]), [{module, mod_rotation_backup}], Context),
                    ok = file:delete(TmpPath),
                    {error, "Could not create archive"}
            end
    end.


-spec create_archive(Name, TmpDir, Job, Context) -> undefined | list() when
    Name:: string(),
    TmpDir:: string(),
    Job:: string(),
    Context:: #context{}.
create_archive(Name, TmpDir, Job, Context) when Job =:= "database" ->
    rotation_backup_create_archive_database:create(Name, TmpDir, Context);
create_archive(Name, TmpDir, Job, Context) when Job =:= "files" ->
    rotation_backup_create_archive_files:create(Name, TmpDir, Context);
create_archive(_Name, _TmpDir, Job, Context) ->
    ?zWarning("Don't know how to backup job '~s'", [Job], Context).
