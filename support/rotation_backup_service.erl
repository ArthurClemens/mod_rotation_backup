%% @author Arthur Clemens
%% @copyright 2018 Arthur Clemens
%% @doc Command/service related functions

-module(rotation_backup_service).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    check_configuration/1,
    archives/1,
    archive_data/2,
    store/2,
    remove/2
]).


-spec check_configuration(Context) -> list() when
    Context:: #context{}.
check_configuration(Context) ->
    HasDatabaseDumpCmd = has_database_dump_cmd(),
    HasCompressCmd = has_compress_cmd(),
    HasOptionalNiceCmd = case z_convert:to_bool(nice_config_cmd(Context)) of
        false -> true; % not configured, so fine
        true -> has_nice_cmd(Context)
    end,
    Dir = m_config:get_value(
        mod_rotation_backup,
        path,
        Context
    ),
    IsValidDir = filelib:is_dir(Dir),
    [
        {ok, HasDatabaseDumpCmd and HasCompressCmd and HasOptionalNiceCmd and IsValidDir},
        {db_dump_cmd, HasDatabaseDumpCmd},
        {compress_cmd, HasCompressCmd},
        {nice_cmd, HasOptionalNiceCmd},
        {valid_dir, IsValidDir},
        {path, Dir}
    ].


-spec archives(Context) -> list(string()) when
    Context:: #context{}.
%% Returns a list of archive names
archives(Context) ->
    Cfg = rotation_backup_service:check_configuration(Context),
    Dir = z_convert:to_list(m_config:get_value(
        mod_rotation_backup,
        path,
        Context
    )),
    case proplists:get_value(ok, Cfg) of
        true ->
            case file:list_dir(Dir) of
                {ok, Filenames} -> Filenames;
                {error, _Reason} -> []
            end;
        false -> 
            ?zWarning("Problem getting the list of archives.", Context),
            []
    end.


-spec archive_data(Archives, Context) -> list() when
    Archives:: list(string()),
    Context:: #context{}.
%% Takes a list of archive names and returns the parsed data.
archive_data(Archives, Context) ->
    Identifier = rotation_backup_archive:identifier(Context),
    List = rotation_backup_archive:parse_archive_names(Archives, Identifier),
    lists:reverse(lists:sort(List)).

 
-spec store(File, Context) -> string() when
    File:: string(),
    Context:: #context{}.
%% Copy file to destination
store(File, Context) ->
    Dir = z_convert:to_list(m_config:get_value(
        mod_rotation_backup,
        path,
        Context
    )),
    Filename = filename:basename(File),
    Destination = filename:absname_join(Dir, Filename),
    file:copy(File, Destination).


-spec remove(File, Context) -> string() when
    File:: string(),
    Context:: #context{}.
%% Remove archive.
remove(File, Context) ->
    Dir = z_convert:to_list(m_config:get_value(
        mod_rotation_backup,
        path,
        Context
    )),
    Filename = filename:basename(File),
    Path = filename:absname_join(Dir, Filename),
    file:delete(Path).


nice_config_cmd(Context) ->
    HasOptionalNiceCmd = m_config:get_value(
        mod_rotation_backup,
        nice,
        Context
    ),
    case HasOptionalNiceCmd of
        undefined -> undefined;
        _ -> z_convert:to_list(HasOptionalNiceCmd)
    end.


-spec has_cmd(Cmd) -> boolean() when
    Cmd:: string().
has_cmd(Cmd) when Cmd =:= undefined ->
    false;
has_cmd(Cmd) ->
    case (catch rotation_backup_cmd:run(Cmd)) of
        {commandfailed, _} -> false;
        _ -> true
    end.

has_compress_cmd() -> has_cmd("tar --version").
has_database_dump_cmd() -> has_cmd("pg_dump --version").
has_nice_cmd(Context) -> has_cmd(nice_config_cmd(Context) ++ " ls").

