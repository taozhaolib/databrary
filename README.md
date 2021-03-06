# Databrary

http://databrary.github.io/databrary

## Layout

* [MVC Application](app/)
* [Master database schema](conf/schema.sql)
* [Database interface](dbrary/)
* [ffmpeg interface](media/)

## Installation

Requirements:
- PostgreSQL >= 9.3
- ffmpeg >= 2.4 (not libav) with shared libraries
- cracklib with shared library
- activator >= 1.2 (play 2.3)
- Java 7 JDK

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

You also must manually install pgranges after each postgres upgrade:

    make -C dbrary/pgranges install

### Security

To ensure proper application security you must add a secret to local.conf,
e.g.:

    echo application.secret=\"`openssl rand -base64 -out /dev/stdout 48`\" >> local.conf

### Object storage

The default configuration stores objects under store/ and cache/, which will
both need to be created or changed to other existing directories in local.conf.

## Usage

### Code Style

#### HTML

* Always use double-quotes.
* Indent with two spaces.
* IDs and classes are dash-spearated, not under_score, not camelCase.
* Lowercase everything.
* App links should not have target or title.
* External links should always have target and title.

### Stylus

* Indent with two spaces.
* Keep template-specific styles with template.
* Never use #ids to style.
* Only use elements for global styles.
* Put a class on every element you style. 
* Don't use on inheritance.
* Name classes based on context.
* Don't share styles between templates.
* Always use single quotes.
* Use dash-class-names, never under_score, never camelCase.
* If you ever have to override a class-based style, you are doing something wrong.

#### JS

* Always use single quotes.
* Indent with two spaces.
* Use === as often as possible.
* Make sure app.js is remains the first file alphabetically.

#### AngularJS

* Bind everything related to a form to the form object.
* Store form data in form.data, preferably in the structure you'd send to the server.
* Make as few new scopes as possible.

## License

Copyright (C) 2013-2015 New York University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
