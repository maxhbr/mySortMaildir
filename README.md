mySortMaildir
=============
A small program, which filters mails in **maildir**s by some simple configurable
rules.
In my configuration are the maildirs updated by **offlineimap** and read via
**mutt** but also other programs use maildirs.

I am actually **not sure, if it is safe to use this program**.

Setup
-----
To configure, create a **Config.hs** by using **Config.hs.example** and

    $ cabal sandbox init
    $ cabal install

Then build the project

    $ cabal build

or run it directly via

    $ cabal run

or run it without compiling via

    $ ./src/Main.hs

TODO:
-----
* Solve encoding problems
* Mail header as **[(String, String)]̀̀̀̀**?
* use parsec for parsing
* Find better name

Infos:
------
about *maildir*: http://cr.yp.to/proto/maildir.html
