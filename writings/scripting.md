Shell Scripting Tricks
==

I like writing shell scripts, even when they are not really
appropriate.  I will attempt to document a number of tips and tricks
on this page.  Unless otherwise noted, I will be writing about
POSIX-compatible shells, although much can probably be adapted to `rc`
without too much trouble.  None of this is cutting edge or
particularly unknown, but it's a handy list of solutions that weren't
immediately obvious to me when I first encountered the problems they
solve.

Changing a file in-place
--

A common task in shell scripts is to change some file in place.  A
shell pipeline is not directly suitable, as for example

    cat foo | grep bar > foo

will truncate the file `foo` before running the pipeline, thus merely
deleting its contents.  There's a workaround exploiting shell
evaluation order, though:

    (rm foo && grep bar > foo) < foo

When running the above, the shell first opens the file `foo` for
reading, then begins executing the subshell.  `rm foo` unlinks the
file from the file system, but Unix file semantics means that the
shell will still have it open for reading (it won't be finally erased
until any open file descriptors have been closed).  When the shell
then evalutes `grep bar > foo`, `foo` will be a _newly_ created file,
and thus not affect the contents of the "old" `foo` that is still
open.  You could also use the `sponge` program from the
[moreutils](http://kitenet.net/~joey/code/moreutils/) package,
although I believe the above is perfectly elegant.

Both the subshell method and `sponge` suffer from the same problem,
namely that the contents of `foo` will be entirely replaced by the
output of the command, even if the command fails.  Consider using
[`insitu`](/programs/insitu), which can detect whether the command
fails, and not truncate in that case.  If you want to be completely
safe against data loss, you need to produce all output to a temporary
file and overwrite the original at the end, though.

Literal sublines in files
--

You often need to do something to those lines of a file that contain
some precise string.  If you needed to delete such a line, you'd be
tempted to use `sed`, but then you will have to escape the string in
order to embed it within a regular expression.  You might eye `fgrep
-v`, but if you don't need to delete the line, but just change it in
some way, that's still not good enough.  My preference so far is to
bite the bullet and the biggest hammer in the toolbox:

    awk 'index($0,STR)!=-1 { /* do whatever */ }' 'STR=any string that you can dream of'

This exploits a slightly obscure Awk facility for defining variables
(in this case, `STR`) from the command line.

