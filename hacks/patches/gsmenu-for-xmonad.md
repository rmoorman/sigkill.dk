`gsmenu` support for XMonad
===========================

XMonadContrib has the `XMonad.Actions.GridSelect` module that served
as the inspiration for [gsmenu](/programs/gsmenu).  As the latter has
more features, I created a module by the name of `GSMenuPick` that is
(almost) API-compatible with `XMonad.Actions.GridSelect`, but uses
gsmenu behind the covers.  It also has a few additional features, such
as the ability to close windows by pressing `C-d`, and can of course
be customised further by modifying `gsmenu` itself.

You can peruse my [xmonad.hs](/pub/configs/xmonad/xmonad.hs) if you
want to see it used in practice, but for simple uses, it's identical
to standard GridSelect.

Download and setup
--------

Download [this file](/pub/configs/xmonad/lib/GSMenuPick.hs) and put it
at `~/.xmonad/lib/GSMenuPick.hs`, then replace any instance of
`XMonad.Actions.GridSelect` in your `xmonad.hs` with `GSMenuPick`.  In
case your previous configuration used a customised `GSConfig`, you may
need to adapt (or remove) some of those customisations.
