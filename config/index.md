My configuration files
=====

As programmers, we spend large amounts of time in front of our
computers, and it makes as much sense to customise (and optimise) our
digital work environment as our physical one.  Here is a list of
configuration files tailored to my own preferences.  If you look
around, you may find some inspiration for your own.

%{
cfgpath=/var/www/sigkill.dk/pub/configs
webpath=/pub/configs

fn list_cfgs {
   echo '<ul>'
   for (d in `{ls $cfgpath}) {
       progname=`{basename $d}
       if (! ~ `{basename $d} .* && test -d $d) {
          echo '<li>'$progname'<ul>'
          for (f in `{ls $d}) {
              filename=`{basename $f}
              echo '<li><a href="'$webpath'/'$progname'/'$filename'">'$filename'</a></li>'
          }
          echo '</ul></li>'
       }
   }
   echo '</ul>'
}

list_cfgs
}%
