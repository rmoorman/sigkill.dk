`gsmenu` - a visual menu program
===============================

Inspired by
[GridSelect](http://blog.clemens.endorphin.org/2008/11/xmonad-gridselect.html)
for [XMonad](http://xmonad.org/), I created a very similar independent
program by the name of `gsmenu`.  As an extension, inspired by
[`dmenu`][dmenu], `gsmenu` also allows filtering of possible matches,
making it useful for selecting among more elements than will fit on
the screen.  See the [manpage][] for more information.

The program is written in Haskell, and is intentionally kept simple in
the interest of robustness, correctness and hackability.
Configuration, if necessary, is by editing the [`GSMenu/Config.hs`][config.hs]
file.

Downloading and installation
----------------------------

The development version of `gsmenu` is always available [on
GitHub][gitrepo], while the latest release is [on hackageDB][hackage].
If you have a functioning Haskell/Cabal setup, you should be able to
install it by invoking `cabal install gsmenu`.  Note that Cabal
installs binaries and manpages in its own subdirectory by default
(`~/.cabal`), and you will thus need something like the following in
your shell initialisation file in order to invoke the program or read
the manpage from a terminal:

    PATH=$PATH:~/.cabal/bin

You probably won't need to set up your `MANPATH` (the list of
directories searched for manpages), as at least on my Debian, the
global `/etc/manpath.config` maps `PATH` entries to corresponding
`MANPATH` entries.  If your system is different, you will need
something like the following:

    export MANPATH=":~/.cabal/share/man/"


[dmenu]: http://tools.suckless.org/dmenu/
[manpage]: manpage
[gitrepo]: http://github.com/Athas/gsmenu
[hackage]: http://hackage.haskell.org/package/gsmenu-1.0
[config.hs]: http://sigkill.dk/pub/code/gsmenu/GSMenu/Config.hs