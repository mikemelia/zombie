# zombie

FIXME: description

## Installation

### Install FastMPJ
Download the installation from http://torusware.com/nevonproducts/fastmpj/ and follow the installation instructions.

### Install FastMPJ library in local repo.


    $ cd $FMPJ_HOME
    $ mvn install:install-file -Dfile=lib/mpj.jar -DgroupId=fastmpj -DartifactId=mpj -Dversion=1.0 -Dpackaging=jar
    $ mvn install:install-file -Dfile=lib/xxdev.jar -DgroupId=fastmpj -DartifactId=xxdev -Dversion=1.0 -Dpackaging=jar


## Usage

    $ lein uberjar
    $ fmpjrun -np 20 -jar target/zombie-0.1.0-SNAPSHOT-standalone.jar


## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
