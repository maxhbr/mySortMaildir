mySortMaildir
=============
A small program, which filters mails in **maildir**s by some simple configurable
rules.
In my configuration are the maildirs updated by **offlineimap** and read via
**mutt** but also other programs use maildirs.

I am actually **not sure, if it is safe to use this program**.

Setup
-----
To configure, create a **mySortMaildir.hs** by using
**mySortMaildir.hs.example** and

    $ cabal sandbox init
    $ cabal install

Then build the project

    $ cabal build

or run it directly via

    $ cabal run

or run it without compiling via

    $ cd src
    $ chmod +x mySortMaildir.hs
    $ ./mySortMaildir.hs

TODO:
-----
* try https://hackage.haskell.org/package/email-header
* **Remember already parsed/sorted mails**
  - get config file via getExecutablePath
  - compare MD5 of config, to check for changes (Data.Digest.Pure.MD5)
  - serialize / cache parsed information
* Watch the folders with **System.FSNotify**
* Solve encoding problems
* tui with https://github.com/skogsbaer/hscurses or http://jtdaugherty.github.io/vty-ui/
* use parsec for parsing
* Find better name ({,h}S{o,}rtM{ai,}ld{i,}r)

Infos:
------
about *maildir*: http://cr.yp.to/proto/maildir.html
