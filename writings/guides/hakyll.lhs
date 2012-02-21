Opinionated Hakyll Tutorial
===

This article (which is also a Literate Haskell program) describes
basic Hakyll metaphors in a way that I would have found useful, using
a working example site to illustrate the concepts.  The mechanism
described in this article are not necessarily an isomorphism of the
technical reality of Hakyll, but are rather a glimpse of the metaphors
that I have found useful in practise when working with it.  Also, it
should probably be noted that I was using Hakyll 3.2.6.1 when writing
this article.

Introduction
---

[Hakyll](http://jaspervdj.be/hakyll/) is the static web site generator
used to create this site.  You use it by defining the behaviour and
structure of your site as a Haskell program that uses various
facilities exposed by the Hakyll modules, a style familiar to users of
[Xmonad](http://xmonad.org/).  A number of
[tutorials](http://jaspervdj.be/hakyll/tutorials.html) are available
on the Hakyll website, but when I was trying to learn Hakyll, I was
sorely missing a guide aimed at experienced Haskell programmers, one
that defined the basic abstractions and metaphors in terms of the data
types actually exposed by Hakyll.  After much trial and error, I
eventually came to understand what was going on, and decided to
document it as the kind of tutorial that I would have found useful.  I
still recommend skimming the other tutorials, as I will probably skip
things that I don't find very interesting.  In fact, this should
probably be considered an "advanced tutorial" (that certainly sounds
much better than "stream-of-consciousness snapshot").

Modules
---

Hakyll is used by compiling and running a program in the directory
containing the input files, which then generates the site as a set of
output files.  Hence, we define a `Main` module that exports a `main`
function and imports all the modules we'll need.  The
`OverloadedStrings` language extension is a small convenience lets us
write `"*.md"` instead of `parseGlob "*.md .

> {-# LANGUAGE OverloadedStrings #-}
> module Main(main) where
> import Hakyll
> import Control.Arrow
> import Control.Monad
> import Data.Maybe

Some Prelude definitions are defined more generally in
`Control.Category`, particularly in that they work better with arrows.

> import Control.Category
> import Prelude hiding (id)

`id` is now both the identity function and the identity arrow (since
functions are arrows and hence categories).

Basic Operation
---

Fundamentally, Hakyll is extremely simple: it is an association
between [Identifier][1]s and [Compiler][2]s, and everything else is
just scaffolding around that.  An identifier is just that: a name
consisting of an optional "group" and a mandatory "path", which need
not be a file path, but often is.  A compiler is an arrow from some
input to some output (read [Programming with Arrows][arrows] for an
excellent introduction).  When the site is generated, the compiler for
each identifier is executed.  A compiler can in principle do anything
to generate its output, but one interesting thing it can do is ask for
the compilation result some other (known) identifier.  Hakyll will
automatically run the compilers in the proper order such that these
requirements are satisfied, although cycles are forbidden.  Since you
cannot in general know the type of values generated by the compiler of
some arbitrary identifier, you can get type classes at runtime (there
are some typeclass constraints that I'll get into later).  Hakyll
identifiers and patterns are tagged with phantom types to help with
maintaining type safety, but we won't be making much use of that in
this tutorial.

[arrows]: http://www.cs.chalmers.se/~rjmh/afp-arrows.pdf

The output from a compiler is not necessarily put anywhere unless
another compiler asks for it, but it is possible to define a
"[route][3]" for a compiler, which determines where to put the result.
The route is not part of the compiler itself, but defined as an
association of the identifier.

[1]: http://hackage.haskell.org/packages/archive/hakyll/3.2.6.1/doc/html/Hakyll-Core-Identifier.html
[2]: http://hackage.haskell.org/packages/archive/hakyll/3.2.6.1/doc/html/Hakyll-Core-Compiler.html
[3]: http://hackage.haskell.org/packages/archive/hakyll/3.2.6.1/doc/html/Hakyll-Core-Routes.html

Rules
---

When working with Hakyll in practise, you will define compilers though
a monadic DSL incarnated in the [Rules][4] monad.  This monad makes it
easy to apply similar compilers to all files matching some given
pattern.  For example, let us define a rule that copies the
`style.css` file verbatim into our site.

> compileCss :: Rules
> compileCss = match "style.css" $ do
>   route   idRoute
>   _ <- compile copyFileCompiler
>   return ()

The `return ()` is to make the type `Rules`, rather than `RulesM
(Pattern CopyFile)` -- `Rules` is just `RulesM ()`.  The `match`
function runs the given rules for each file in its match, although the
above will only match exactly one file, namely `style.css` in the same
directory as our Hakyll source file.  In general terms, the `compile`
action associates the given compiler with each of the identifiers
matching the current pattern.  The set of possible identifiers (which
are then filtered by the pattern) is taken from the *files* (not
directories) in the directory from which Hakyll is run.  The compiler
given to `compile` must take a `Resource` as its input, which is
really just a wrapper around an identifier guaranteed to refer to a
file.

[4]: http://hackage.haskell.org/packages/archive/hakyll/3.2.6.1/doc/html/Hakyll-Core-Rules.html

A Menu
---

Let's do something more complicated: we want our site to contain a
bunch of pages, but all of them should contain a complete list (a
menu) of all pages on the site.  This sounds like a problem: in order
to generate one page, we must already have seen every other page,
which violates the requirement that compiler dependencies must be
acyclic.  The solution is to use two logical passes: first run a set
of compilers that somehow generate a list of all pages, then actually
generate the content for each page while including that information.
This is possible since the menu does not need to know the *content* of
each page, but only its eventual location on the site, which in Hakyll
terms is the *route* for each *identifier* corresponding to an input
file.  So far, so good, but how do we easily generate such lists?  To
begin with, we define a pattern that matches all the content pages on
our site.

> content :: Pattern a
> content = "**.md"

This pattern matches all `.md` files, including those in
subdirectories to any depth.  When we try to define rules for storing
menu information, we may run into the problem that the compiler we
pass to `compile` is really quite restricted in its output type: it
has to implement various type classes permitting serialisation, as it
could in principle be required to write its result to a file.  In this
case we're in luck, as plain `String`s implement the required
instances.  A second problem is that we now wish to associate two
compilers with each input file - one that generates its data for the
menu, and one that actually renders the page to HTML.  The solution is
to tag the identifiers related to the menu compilers with a group
(here, `"menu"`), which makes them different from the identifiers used
for the actual content, which have no group.

> compileMenu :: Rules
> compileMenu = match content $ group "menu" $ do
>                 res <- resources
>                 forM_ res $ \r -> do
>                   create r destination

The `resources` action returns the list of identifiers matching the
current pattern.  For each such identifier, we use `create` to
manually associate a compiler (`destination`, defined below).  The
`group` combinator is used to ensure that all identifiers are in the
`"menu"` group, as `create` ignores the group of the identifier it is
given.

> destination :: Compiler a String
> destination = getIdentifier
>               >>> arr (setGroup Nothing)
>               >>> getRouteFor
>               >>> arr (fromMaybe "")

The `destination` compiler does not use its original input for
anything, but instead immediately discards it for obtaining the
identifier being compiled, setting the group of that identifier to
`Nothing`, getting the route for the resulting identifier, and if that
identifier doesn't exist (the route is `Nothing`), returning the empty
string.  This is a a bit of a hack, but we don't really expect
getRouteFor to ever return `Nothing`, as that would mean we have been
asked to add a menu entry for a file that will not exist on the site.
Since the only different between the identifiers used for generating
the menu and the content is that the latter are in the `Nothing`
group, this will compute the output path of the compiler responsible
for generating the content for the respective file.  The reason this
works is because you do not need to run the compiler in order to
determine where it will put its output - that is defined in the
`Rules` DSL, and hence available simply by querying the identifier.

Using the Menu
---

The rule for defining our content pages is quite simple.  We replace
the existing extension (`md` according to the `content` pattern) with
`html`, then pass the page through a four-step compiler that first
converts the page from Markdown to HTML, then adds the menu, then
applies an HTML template, then finally converts absolute URLs into
relative URLs so the resulting files can be put anywhere (don't worry
about this last stage, it's not important).

> compilePages :: Rules
> compilePages = match content $ do
>                  route $ setExtension "html"
>                  _ <- compile $ pageCompiler
>                                 >>> addMenu
>                                 >>> applyTemplateCompiler "template.html"
>                                 >>> relativizeUrlsCompiler
>                  return ()

To understand how `addMenu` works, we first have to understand Hakyll
templates and the `Page` data type.  Templates are simply files in
which variables can be written as `$var$`.  When applying the
template, each such instance is replaced with the value of the
corresponding variable.  A `Page` contains, apart from some data, a
mapping between variable names and values (both strings).  Our
`template.html` file will contain the text `$menu$` where we intend
our menu to show up, and hence we need to make the `addMenu` compiler
accept a `Page` as its input, and output the same page, but with its
`"menu"`-variable set to an HTML-rendering of the site.

> addMenu :: Compiler (Page a) (Page a)
> addMenu = id &&& getMenu >>> setFieldA "menu" id

The `getMenu` compiler is the one that actually produces the menu.
The real trick here is the use of `requireAll_`, which lets us obtain
a list of all compiler outputs for identifiers in the `"menu"` group.
That means a list of all routes for our content pages!

> getMenu :: Compiler a String
> getMenu = this &&& items >>> arr (uncurry showMenu)
>   where items = requireAll_ $ inGroup $ Just "menu"
>         this = getRoute >>> arr (fromMaybe "")

`showMenu` itself is just a plain Haskell function that produces an
HTML list with the current page highlighted.  In practice, we'd use a
proper HTML combinator library, but let's stick with strings for
simplicity.

> showMenu :: FilePath -> [FilePath] -> String
> showMenu this items = "<ul>"++concatMap li items++"</ul>"
>   where li item = "<li><a href=\"/"++item++"\">"++name item++"</a></li>"
>         name item | item == this = "<strong>"++item++"</strong>"
>                   | otherwise    = item

In order to use a template, we have to tell Hakyll it exists.  That's
what the `templateCompiler` is about.  This is not a very interesting
definition.

> compileTemplates :: Rules
> compileTemplates = match "template.html" $ do
>                      _ <- compile templateCompiler
>                      return ()

Finally, the Hakyll main function is written in a rather stylised
manner, with `hakyll` being given a `Rules` monad.

> main :: IO ()
> main = hakyll $ do
>          compileCss
>          compileMenu
>          compilePages
>          compileTemplates

That's all there is to it.  To try it out, download [some input
files](/files/hakyll-tutorial.tar.gz), put this
[hakyll.lhs](hakyll.lhs) into the directory, then run

    $ ghc --make hakyll.lhs && ./hakyll build && ./hakyll preview 8080

and point your web browser at `localhost:8080`.  The result should be
something similar to [this](hakyll_tutorial_result/).  If you change
the code, remember to run `./hakyll clean` as well, as Hakyll's cache
system might otherwise not realise that something is different.

You may also want to look at [sigkill.lhs](/programs/sigkill.html),
the program generating my own website.