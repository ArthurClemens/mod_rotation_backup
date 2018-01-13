{% extends "admin_base.tpl" %}

{% block title %} {_ Rotation Backup _} {% endblock %}

{% block content %}
    {% with m.acl.is_admin as is_editable %}
        <div class="admin-header">
            <h2>{_ Rotation Backup _}</h2>
        </div>
        <div class="row">
            <div class="col-lg-8 col-md-6">
                <div id="rotation_backup_archives">
                    {% include "_rotation_backup_archives.tpl"
                        archives=archives
                    %}
                </div>                
            </div>
            <div class="col-lg-4 col-md-6">
                {% if backup_config.busy %}
                    <div class="alert alert-warning" role="alert">
                        {_ Backup is working. _}
                    </div>
                {% elseif backup_config.ok %}
                    {# nothing #}
                {% else %}                    
                    <div class="alert alert-danger">
                        <p><strong>{_ Rotation Backup is not correctly configured and cannot run. _}</strong></p>
                        <ul>
                            {% if not backup_config.db_dump_cmd %}<li>{_ The "pg_dump" command was not found in the path. Set the "pg_dump" config key to the path to pg_dump and return to this page. _}</li>{% endif %}
                            {% if not backup_config.compress_cmd %}<li>{_ The "tar" command was not found in the path. Set the "tar" config key to the path to tar and return to this page. _}</li>{% endif %}
                            {% if not backup_config.nice_cmd %}<li>{_ A priority program was defined in config - using key "nice" - but the command cannot be found. Is it written correctly for this OS? _}</li>{% endif %}
                            {% if not backup_config.valid_dir %}<li>{_ No valid path was found in the configuration. Enter a "path" config key and return to this page. _}</li>{% endif %}
                        </ul>
                    </div>
                {% endif %}

                {% if is_editable %}
                    <div class="well">
                        {_ Backups will be created automatically. <a href="https://github.com/ArthurClemens/mod_rotation_backup">Read the module documentation</a> how to change the default settings. _}
                    </div>
                {% endif %}

                {% if `mod_filestore`|member:m.modules.enabled  %}
                    {% if m.filestore.stats.cloud > 0 %}
                        <div class="alert alert-warning">
                            <strong>{_ Warning _}</strong>

                            {{ _"This site has cloud file store enabled, and there are <strong>$1</strong> media files on this system that are only stored in the cloud and not on this machine. These files will not backed up!"|replace_args:[m.filestore.stats.cloud|make_list] }}
                        </div>
                    {% endif %}
                {% endif %}

            </div>
        </div>
    {% endwith %}
{% endblock %}
