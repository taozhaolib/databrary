# Databrary

http://databrary.github.io/databrary

## Layout

* [MVC Application](app/)
* [Master database schema](conf/schema.sql)
* [Database interface](dbrary/)
* [ffmpeg interface](media/)

## Installation

### Postgres

We assume that the server's timezone (for timestamp without timezone values) is
UTC, which means timezone should be unset in postgresql.conf.

Postgres must be installed and listening on localhost:5432, for example with a
line like this in pg\_hba.conf:

    host	sameuser	all	127.0.0.1/32	md5

(JDBC does not support unix sockets for some reason.)  You must also create a
"databrary" user and database:

    CREATE USER "databrary" WITH PASSWORD '<passwd>';
    CREATE DATABASE "databrary" WITH OWNER "databrary";

Then create local.conf with whatever password you used above:

    db.default.password="<passwd>" 

For other settings, see conf/application.conf.

### Security

By default in development mode the site is completely insecure, meaning you can
login with only a username once you have an account, and it uses a public
"secret".  To switch to secure mode, add a line to local.conf:

    echo application.secret=\"`openssl rand -base64 -out /dev/stdout 48`\" >> local.conf

### Object storage

The default configuration stores objects under store/ and cache/, which will
both need to be created or changed to other existing directories.

## License

Copyright (C) 2013 New York University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
