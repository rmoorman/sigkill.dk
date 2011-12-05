Minor scripts, hacks and patches
===========

I put minor hacks and scripts here.  Some may be of use to others but
me, but I expect that the majority will only be of educational or
entertainment value.

Many of the scripts are written in
[`rc`](http://en.wikipedia.org/wiki/Rc), the command shell from the
Plan 9 operating system.  It is similar to classic POSIX bourne shell
script, but significantly simplified and, in my opinion, improved.
Should you wish to run them, you will need a port of `rc` to your
platform.  I personally use [9base](http://tools.suckless.org/9base),
a port of many of the standard Plan 9 tools to the POSIX environment
by the gentlemen and scholars at [suckless.org](http://suckless.org).
*Beware*: the reimplementation of `rc` for Unix by Byron Rakitzis is
incompatible in many fundamental and seemingly arbitrary ways, for
example by using `else` instead of `if not` as in Plan 9 `rc`.  Sadly,
this is the version you will get if you merely install the `rc`
package in Debian.  Install the `9base` package instead, and add
`/usr/lib/plan9/bin` to your path.  Also read [this guide to setting
up Plan 9 userland tools](/writings/guides/plan_9_tools).

%{
hackpath=/var/www/sigkill.dk/pub/scripts
webpath=/pub/scripts

fn get_c_desc {
   awk -F/ '
   BEGIN { indesc=0; descover=0; }
   /^\/\*/ { if (indesc==0) { indesc=1; next; } }
   /^ \*\// { if (indesc==1) { descover=1; next; } }
   // { if (indesc==1 && descover!=1) { print $0 } }' \
   | sed 's/^ \*//' | $formatter
}

fn get_hs_desc {
   awk -F/ '
   BEGIN { indesc=1; descover=0; }
   /^--/ { if (indesc==1) { print; next; } }
   { indesc=0; }' \
   | sed 's/^-- ?//' | $formatter
}

fn get_shell_desc {
   awk -F/ '
   BEGIN { indesc=1; skip=0; }
   /^#!/ { next; skip=1; }
   /^#/ { if (skip>0) { skip = skip - 1; next; } }
   /^#/ { if (indesc==1) { print; next; } }
   { indesc=0; }' \
   | sed 's/^# ?//' | $formatter
}

fn list_hacks {
   echo '<ul>'
   for (p in `{ls $hackpath}) {
       progname=`{basename $p}
       if (! ~ $progname .* && test -r $p) {
          echo '<li><h2 class="progName"><a href="'$webpath'/'$progname'">'$progname'</a></h2>'
          if (~ $progname *.c) {
             cat $p | get_c_desc
          }
          if (~ $progname *.hs) {
             cat $p | get_hs_desc
          }
          if not {
             cat $p | get_shell_desc
          }
          echo '</li>'
       }
   }
   echo '</ul>'
}

list_hacks
%}
