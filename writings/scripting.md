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
[`insitu`](/programs/insitu.html), which can detect whether the
command fails, and not truncate in that case.  If you want to be
completely safe against data loss, you need to produce all output to a
temporary file and overwrite the original at the end, though.

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

Multiple pipes
--

Sometimes you need to start a program with specific file descriptors
open.  This is easy if you need them bound to files, as you can simply
provide the optional file descriptor parameter to the input
redirection operator:

    prog 3</path/to/file 4</path/to/other_file

In other cases, you need a program to continously receive the output
of several other programs, but a standard Unix shell pipeline will
connect standard outputs to standard inputs, which means that a
process cannot more than a single piped input stream.  You can work
around this by having producers write to named pipes (`mkfifo`), which
can be used like files as above, but this results in the hassle and
complexity of file system cleanup.  An alternative is to exploit the
file descriptor duplication operator (`<&`) to copy the standard input
file descriptor before starting the next stage of the pipeline.  The
result will be every stage of the pipeline being available in a
different file descriptor.  An demonstration to clarify:

    loop() {
      while true; do echo $1; sleep 1; done
    }
    loop foo | (loop bar | (loop baz | (cat <&3 & cat <&4 & cat <&5) 5<&0 0<&-) 4<&0 0<&-) 3<&0 0<&-

At each right-hand side of a file descriptor, the standard input
stream is copied to another file desciptor, and then closed (with
`0<&-`), which may be necessary to make reading from the new file
descriptor reliable, depending on whether any of the programs touch
standard input.  The innermost program (`cat <&3 & cat <&4 & cat <&5`)
has access to the output of the three producing programs via file
descriptors 3, 4 and 5.  The parentheses are necessary to ensure the
file redirection operators work in the proper subshells.

Of course, the above is rather ugly and cumbersome.  The following
shell function provides a nicer syntax, but requires you to define
each step in the pipeline as a function by itself.

    multipipe() {
        if [ $# -eq 1 ]; then
            $1
        else
            cmd=$1
            shift
            fd=$1
            shift
            $cmd | eval multipipe '"$@"' "$fd<&0" "0<&-"
        fi
    }

    loop() {
        while true; do echo $1; sleep 1; done
    }

    loopfoo() {
        loop foo
    }

    loopbar() {
        loop bar
    }

    loopbaz() {
        loop baz
    }

    body() {
        cat <&3 & cat <&4 & cat <&5
    }

    multipipe loopfoo 3 loopbar 4 loopbaz 5 body
