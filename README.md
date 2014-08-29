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

To ensure proper application security you must add a secret to local.conf,
e.g.:

    echo application.secret=\"`openssl rand -base64 -out /dev/stdout 48`\" >> local.conf

### Object storage

The default configuration stores objects under store/ and cache/, which will
both need to be created or changed to other existing directories.

## Usage

### Code Style

#### Directory Structure

* app/
  * assets/
    * config/ - angular.module().config
    * controller/ - angular.module().controller, currently empty, probably you want directives
    * decorator/ - angular.module().decorator
    * directive/ - angular.module().directive, generic, sometimes without templates
    * filter/ - angular.module().filter
    * form/ - angular.module().directive, always with templates
    * helper/ - angular.module().[service|factory|provider], services that other services depend on
    * interceptor/ - angular.module().interceptor
    * model/ - angular.module().[service|factory|provider], services that make http requests
    * panel/ - angular.module().directive, always with templates
    * polyfill/ - vanilla js polyfills
    * service/ - angular.module().[service|factory|provider], generic
    * stylus/ - styles framework
      * common/ - common styles based on class, another form of shame
      * mixins/ - all our mixins, generally used by stylus/styles/ of the same name
      * shame/ - all css hacks, browser fixes, and bad practices
      * styles/ - all our generic styles, generally using stylus/mixins of the same name
      * vars.styl - all our variables
    * view/ - angular.module().controller, specific to routes

#### HTML

* Always use double-quotes.
* Indent with two spaces.
* ID the top element of every template.
* Parallel top element ID with identical class (for styling).
* IDs and classes are dash-spearated, not under_score, not camelCase.
* Lowercase everything.
* Never use style="", but maybe use ng-style/bo-style.
* App links should not have target or title.
* External links should always have target and title.
* Never write any inline text. Use conf/messages.

### STYLUS

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

* Always use single quotes. Always. 
* Indent with two spaces.
* Use === as often as possible.
* Make sure app.js is remains the first file alphabetically.

#### AngularJS

* Bind everything related to a form to the form object.
* Store form data in form.data, preferably in the structure you'd send to the server.
* Make as few new scopes as possible.
* Use controllerAs syntax as often as possible.
* Prefer events to $watch and {{bind}}.
* Never use filters in templates


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
