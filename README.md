# Rotation Backup

Zotonic module to create backups following a grandfather-father-child rotation scheme

* Uses configurable backup scheme, for example "6h 1d 1w 1m 1y".
* Creates separate backups for database and files, optionally with different backup schemes.
* Automatically removes expired archives.
* Manage backups of database and files to a directory - for instance a NFS/CIFS backup storage space.

Inspired by [Tarsnapper](https://github.com/miracle2k/tarsnapper), with improvements to interval handling.


## The backup scheme

tldr; the default values will maintain 7 daily backups, 4 weekly backups, 12 monthly backups, and after that one backup for each year.

The backup scheme is defined by time interval ranges (default: "1d 1w 1m 1y" - but they can be set to any other time values). The first value defines the frequency of the backups; the other values define how many backups should be kept.

For the default setting, one backup will be made every day.

The interval `1d - 1w` means: keep 1 daily backup up to 1 week, so 7 backups of the past week.

Backups older than the first interval are handles according to the next interval. The interval `1w - 1m` means: keep 1 weekly backup up to 1 month, so 4 backups of the past month.

The same goes for the next interval `1m - 1y`: keep 1 monthly backup up to 1 year, so 12 backups of the past month.

The final interval can be read as "1y until the end of time": keep 1 yearly backup.


* The smallest interval defines when new backups should be created: as soon as the most recent backup is older than this value (default: 1 day).
* You can use  both `120` and `2h` for 2 hours; `3d` for 3 days; `6m` for six months; and so on. The minimum interval is `10` (minutes) to reduce the load on the server and to prevent overlapping backup tasks.


## Archive creation

* Backup names follow the scheme: identifier-job-date-time. For example: `mysite-database-20141231-065959`.
* The backup name does not contain information about the interval it belongs to (f.i. "WEEKLY");  the date in the name is used to infer that information. The date is 'universal time', written as `dddddd-tttttt`.
* Jobs for Zotonic backups are: `database` and `files`.
* A new backup is created as soon as the most recent backup is older than the first interval.
* Backups will not be skipped if you activate the module later in the day: when it detects that the most recent backup is older, a new backup is created.
* Interval settings can be changed at any time.


## Archive expiration
 
* Only archives with the same identifier are considered; archives created for other sites or using different naming schemes are ignored.
* The date in the archive name is used to infer expiration dates. 
* Calculation starts at the longest interval value (default 1 year). The archive that is closest to that date (the current date minus the interval) is marked as "to keep". Proximity is calculated with a range of plus/minus half an interval (in the example plus or minus half a year).
  * If older archives exist, we go further back in time (the interval value); this process continues until no older archives are found.
  * Archives older than that first interval value that are not marked as "to keep" are marked as "to expire".
* Then the second longest interval is used, until all intervals have been processed.
* The most recent archive is always kept.


## Some questions you might have

### I am seeing more archives than I was expecting

tl;dr: These are extra items to preserve archives when they gradually migrate from new to old.

Internally, archives are grouped into time buckets. When an item expires from a time bucket, it will move to the next bucket (for instance from "days" to "weeks"). This newer archive ("new" from the older bucket point of view) will be kept as a next generation item, until a next item comes along. This mechanism preserves a fresh flow from new to old. 


## Configuration

### Path

**REQUIRED**

The backup directory path.

| Module | Key | Default value |
|--------|-----|-------|
| `mod_rotation_backup` | `path` | |

### Intervals

* The interval range is set with config key `interval` for module `mod_rotation_backup`.
* If not set, the default value will be used: `1d 1w 1m 1y`.
* Intervals are default set for all jobs, or can be further specified for each job: `interval_files` and `interval_database`.

These are the default values in /admin/config:

| Module | Key | Default value |
|--------|-----|-------|
| `mod_rotation_backup` | `interval`          | `1d 1w 1m 1y`  |
| `mod_rotation_backup` | `interval_files`    | `1d 1w 1m 1y`  |
| `mod_rotation_backup` | `interval_database` | `1d 1w 1m 1y`  |


### Archive identifier

The default identifier is the site's host name. You can change that with key `identifier`:

| Module | Key | Default value |
|--------|-----|-------|
| `mod_rotation_backup` | `identifier`          | [host name]  |


### Performance

Creating a tar file from a gigabytes-sized directory can bring the server to a standstill. By configuring a priority program, you can run `tar` with a lower priority.

For example, [ionice](https://linux.die.net/man/1/ionice) is a program that sets the io scheduling class and priority for a program ([more background](https://www.askapache.com/optimize/optimize-nice-ionice/#ionice)). You can set it as: `/usr/bin/ionice -c2 -n5`.

Using a low priority will result in slower backups, but the server will no longer be hogged.

| Module | Key | Default value |
|--------|-----|-------|
| `mod_rotation_backup` | `nice`          |   |


### Debug info

If you are running Zotonic in debug mode, let the module write debug info to the console when set to `true`:

| Module | Key | Default value |
|--------|-----|-------|
| `mod_rotation_backup` | `debug`          | `false`  |



## Installation

### Requirements

* Zotonic 0.11 or higher
* A NFS/CIFS backup space
  * For local testing you can use a temporary directory


### Install

    zotonic modules install mod_rotation_backup


### Activate

Activate this module in Admin > System > Modules. 

