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
    Db = which(db_dump_cmd()),
    Tar = which(archive_cmd()),
    Dir = m_config:get_value(
        mod_rotation_backup,
        path,
        Context
    ),
    IsValidDir = filelib:is_dir(Dir),
    [
        {ok, Db and Tar and IsValidDir},
        {db_dump, Db},
        {valid_dir, IsValidDir},
        {path, Dir},
        {archive, Tar}
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


-spec archive_cmd() -> string().
archive_cmd() ->
    z_convert:to_list(z_config:get(tar, "tar")).


-spec db_dump_cmd() -> string().
db_dump_cmd() ->
    z_convert:to_list(z_config:get(pg_dump, "pg_dump")).


-spec which(Cmd) -> boolean() when
    Cmd:: string().
which(Cmd) ->
    filelib:is_regular(z_string:trim_right(os:cmd("which " ++ z_utils:os_escape(Cmd)))).
