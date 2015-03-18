mySortMaildir
=============
A small program, which sorts (moves) mails in maildirs by some simply
configurable rules.

I am actually **not sure, if it is safe to use this program**.

Setup
-----
To configure, create a **Config.hs** by using **Config.hs.example**.
Then build the project

    $ cabal sandbox init
    $ cabal build

or run it directly via

    $ cabal run

TODO:
-----
* Solve encoding problems
* Mail header as **[(String, String)]̀̀̀̀**?
* use parsec for parsing
* Find better name

Infos:
------
about *maildir*: http://cr.yp.to/proto/maildir.html
