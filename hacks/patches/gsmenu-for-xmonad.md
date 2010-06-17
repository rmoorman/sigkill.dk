`gsmenu` support for XMonad
===========================

XMonadContrib has the `XMonad.Actions.GridSelect` module that served
as the inpiration for [gsmenu](/programs/gsmenu).  As the latter has
more features, I created a patch for XMonadContrib that enables the
use of `gsmenu` rather than the built-in GridSelect code.  After the
patch is applied, you use it by changing your GridSelect bindings to
use the `gsmenuGSConfig` configuration rather than `defaultGSConfig`.
For example, a binding might look like

    ((controlMask, xK_t), goToSelected gsConfig)

You can peruse my [xmonad.hs](/pub/configs/xmonad/xmonad.hs) if you
want to see it used in practice.

Download
--------

[Here.](/pub/code/xmonad-gsmenu.patch)