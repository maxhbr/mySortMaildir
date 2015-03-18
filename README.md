mySortMaildir
=============
A small program, which sorts (moves) mails in maildirs by some simply
configurable rules.

To configure, create a **Config.hs** by using **Config.hs.example**. Then build the
project

    $ cabal sandbox init
    $ cabal build

or run it directly via

    $ cabal run

TODO:
-----
* Solve encoding problems
* Mail header as **[(String, String)]̀̀̀̀**?
* Find better name

Infos:
------
about *maildir*: http://cr.yp.to/proto/maildir.html
