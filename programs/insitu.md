`insitu` - modify files in place with shell commands
====================================================

A shell pipeline cannot both read and write to the same file; for
example

    cat foo | grep bar > foo

will merely delete the contents of `foo`.

A clumsy (and in some cases, slow or unworkable) workaround is to pipe
the output to a temporary file, then move that file over the original.
This requires free disk space equivalent to the size of the new file,
however.

`Insitu` wraps a shell command, connecting its standard input and
standard output to the given file.  If _command_ finishes
successfully, _file_ will be truncated (or extended) to the size of
the output produced by _command_.  If _command_ returns a non-zero
exit status, _file_ will not be truncated (unless _-t_ is passed), but
contain the output read from the command, followed by the rest of the
original contents of _file_.  The part of _file_ overwritten by the
command output will be lost.

With `insitu`, the above would be written as

    insitu foo 'grep bar'

Another handy use is to prepend something to an existing file:

    insitu foo 'echo whatever; cat'

`Insitu` runs the command with `$SHELL -c command`, substituting
`/bin/sh` for `$SHELL` if the latter is not set.

Download
--------

  * There is a [Github repository](http://github.com/Athas/insitu).
  * And a tarball containing the 1.0 release: [insitu-1.0.tar.gz](/pub/dl/insitu-1.0.tar.gz)
