Climacs
=======

I used to hack a lot on [Climacs][], an Emacs-like editor
written in Common Lisp.  I still feel it is an interesting project:
GNU Emacs has a lot of fundamental weaknesses and rapidly turns into a
byzantine tower of complexity if you try to cover them up, partially
due to the limitations of Emacs Lisp compared to more advanced
languages.

I did not start the Climacs project, nor did I write all the current
code, but I did rewrite major subsystems, like the redisplay engine
(see my [blog post][] for a description of what I did), which was not
only fun, but very educational as well.  My proudest feat was managing
to create a syntax-highlighting redisplay engine that supported
embedded images and variable-width fonts, and with somewhat acceptable
performance, although in retrospect I should have spent more time
studying the techniques employed by other editors, rather than just
inventing the algorithms from scratch.

The best way to get Climacs is from its CVS repository, as described
on the [Climacs installation guide][].

[climacs]: http://common-lisp.net/project/climacs/
[blog post]: http://old.sigkill.dk/blog/archives/297-I-can-put-what-in-my-buffer.html
[climacs installation guide]: http://common-lisp.net/project/climacs/#installation