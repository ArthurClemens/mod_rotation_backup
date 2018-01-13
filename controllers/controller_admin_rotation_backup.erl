-module(controller_admin_rotation_backup).

-export([
    is_authorized/2
]).

-include_lib("controller_html_helper.hrl").

is_authorized(ReqData, Context) ->
    z_admin_controller_helper:is_authorized(mod_rotation_backup, ReqData, Context).


html(Context) ->
    Vars = [
        {page_admin_rotation_backup, true},
        {archives, mod_rotation_backup:list_archives(Context)},
        {backup_config, mod_rotation_backup:check_configuration(Context)},
        {backup_in_progress, mod_rotation_backup:backup_in_progress(Context)}
    ],
	Html = z_template:render("admin_rotation_backup.tpl", Vars, Context),
	z_context:output(Html, Context).

