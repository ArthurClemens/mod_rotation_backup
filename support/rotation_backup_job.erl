%% @author Arthur Clemens
%% @copyright 2018 Arthur Clemens
%% @doc Backup jobs: files and database

-module(rotation_backup_job).
-author("Arthur Clemens").

-export([
    jobs/0
]).

-define(JOBS, ["files", "database"]).

jobs() ->
    ?JOBS.
