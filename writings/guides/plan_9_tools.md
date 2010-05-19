Running Plan 9 Userland Tools On Unix
==

This document described how to set up a typical GNU/Linux system for
running `rc` shell scripts that expect Plan 9 userland tools.

Plan 9 is an operating system developed at Bell Labs, able to be
somewhat shallowly described as _Unix done right_.  The standard tools
(`ls`, `cat`, `sed`, etc) seem crude at first, having much less
features than their modern GNU (and often BSD) incarnations, along
with gratuitous shallow incompatibilities, but that merely reflects
the Plan 9 (and Unix) philosophy of highly specialised nonbloated
tools, not an actual technical deficiency.

The [Plan 9 from User Space][1] project has ported a large amount of
the Plan 9 userspace to Unix, including the standard tools, and
constructed "culturally compatible" replacements for some of the parts
that could not reasonably be ported (such as the display system).  If
you are merely interested in running shell scripts expecting a Plan 9
toolset (such as most everything under [/hacks](/hacks), the much more
lightweight [9base][2] package contains only a subset of the full
toolset, namely those most useful for shell scripting.  I attempt to
ensure that all my scripts use only what can be found in 9base.

Setting up 9base
--

Debian contains the 9base package in its archives (although beware
that the version may be out of date and missing important programs).
This package will install most of its contents in `/usr/lib/plan9/`,
with binaries in `/usr/lib/plan9/bin`.  The manual pages will be in
the standard directories, although prefixed with `plan9-` (so you
would invoke `man plan9-sed` to read about the `sed` in 9base).  It
appears a semi-standard to have a `$PLAN9` environment variable point
at `/usr/lib/plan9/` (if you install 9base manually, you should change
this value, of course).  You can set this environment variable
globally by adding the following to `/etc/environment`:

    PLAN9=/usr/lib/plan9

This will have no harmful effects.  On the other hand, globally making
`$PLAN9/bin` the head of your `$PATH` will most likely cause your
system to break, as the system maintenance scripts assume a GNU
userland.  A good `rc` shell script should manually change its path to
prefer `$PLAN9/bin`.

The most important binary in 9base is `rc`, the Plan 9 command shell,
and it will have to be accessible through `$PATH` for shebang-lines
(the `#!/usr/bin/env rc` at the first line of a shell script) to
work.  It may be easiest to simply copy `rc` to `/bin`, although it
should also be safe to add `$PLAN9/bin` to the very end of your
`$PATH`, for example by putting the following in your `.bashrc`:

    PATH=$PATH:$PLAN9/bin

The choice of `.bashrc` over `/etc/environment` means that the setting
will be user-local, although making this the global setting would be
harmless.

[1]: http://swtch.com/plan9port/
[2]: http://tools.suckless.org/9base
