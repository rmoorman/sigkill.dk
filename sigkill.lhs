The Sigkill.dk generator
===

This is the Hakyll program for generating sigkill.dk.  Look at [this
Git repository](https://github.com/Athas/sigkill.dk) for the data
files as well.  The most defining trait of the site is the tree menu
at the top, which contains every content page on the site.  Apart from
that, I also do a lot of small hacks to generate various bits of the
site.

> {-# LANGUAGE OverloadedStrings, Arrows #-}
> module Main(main) where

I want an improved identify function.  Yes, really!  The one from
`Control.Category` also works for identity arrows.

> import Control.Category (id)
> import Prelude hiding (id)

The remaining imports are not very interesting.

> import Control.Arrow
> import Control.Monad
> import Data.Char
> import Data.List hiding (group)
> import Data.Ord
> import Data.Maybe
> import Data.Monoid
> import System.FilePath

> import Text.Blaze.Html5((!))
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Renderer.String

> import Hakyll

Hierarchical menu.
---

We are going to define a data type and associated helper functions for
generating a menu.  Conceptually, the site is a directory tree, with a
page being a leaf of the tree.  The menu for a given page will
illustrate the path taken from the root to the page, namely which
intermediary directories were entered.

A level (or "line", if you look at its actual visual appearance) of
the menu consists of two lists: the elements preceding and succeeding
the *focused element*.  The focused element itself is the first
element of the `aftItems` list.  This definition ensures that we have
at most a single focused element per menu level.

> data MenuLevel = MenuLevel { prevItems :: [(FilePath,String)]
>                            , aftItems  :: [(FilePath,String)]
>                            }
>
> allItems :: MenuLevel -> [(FilePath, String)]
> allItems l = prevItems l ++ aftItems l

> emptyMenuLevel :: MenuLevel
> emptyMenuLevel = MenuLevel [] []

First, let us define a function for inserting an element into a sorted
list, returning the original list if the element is already there.

> insertUniq :: Ord a => a -> [a] -> [a]
> insertUniq x xs | x `elem` xs = xs
>                 | otherwise = insert x xs

We can use this function to insert a non-focused element into a
`MenuLevel`.  We take care to put the new element in its proper sorted
position relative to the focused element, if any.

> insertItem :: MenuLevel -> (FilePath, String) -> MenuLevel
> insertItem l v = case aftItems l of
>                    []     -> atPrev
>                    (x:xs) | v < x     -> atPrev
>                           | otherwise -> l { aftItems = x:insertUniq v xs }
>   where atPrev = l { prevItems = insertUniq v (prevItems l) }

When inserting a focused element, we have to split the elements into
those that go before and those that come after the focused element.

> insertFocused :: MenuLevel -> (FilePath, String) -> MenuLevel
> insertFocused l v = MenuLevel bef (v:aft)
>   where (bef, aft) = partition (<v) (delete v $ allItems l)

Finally, a menu is just a list of menu levels.

> newtype Menu = Menu { menuLevels :: [MenuLevel] }
>
> emptyMenu :: Menu
> emptyMenu = Menu []

I am using the [BlazeHTML](http://jaspervdj.be/blaze/) library for
HTML generation, so the result of printing a menu is an `H.Html`
value.

> showMenu :: Menu -> H.Html
> showMenu = zipWithM_ showMenuLevel [0..] . menuLevels

> showMenuLevel :: Int -> MenuLevel -> H.Html
> showMenuLevel d m =
>   H.ul (mapM_ H.li elems) ! A.class_ (H.toValue $ "menu" ++ show d)
>   where showElem (p,k) = H.a (H.toHtml k) ! A.href (H.toValue p)
>         showFocusElem (p,k) = showElem (p,k) ! A.class_ "thisPage"
>         elems = map showElem (prevItems m) ++
>                 case aftItems m of []     -> []
>                                    (l:ls) -> showFocusElem l :
>                                              map showElem ls

> relevant :: FilePath -> FilePath -> [FilePath]
> relevant this other = relevant' (splitPath this) (splitPath other)
>   where relevant' (x:xs) (y:ys) = y : if x == y then relevant' xs ys else []
>         relevant' [] (y:_) = [y]
>         relevant' _ _ = []

> extendMenu :: FilePath -> Menu -> (FilePath, String) -> Menu
> extendMenu this m (path, name) =
>   if path' `elem` ["./", "/", ""] then m else
>     Menu $ add (menuLevels m) (relevant this' path') "/"
>   where add ls [] _ = ls
>         add ls (x:xs) p
>           | x `elem` focused = insertFocused l (p++x,name') : add ls' xs (p++x)
>           | otherwise        = insertItem l (p++x,name') : add ls' xs (p++x)
>           where (l,ls') = case ls of []  -> (emptyMenuLevel, [])
>                                      k:ks -> (k,ks)
>                 name' = if hasTrailingPathSeparator x then x else name
>         focused = splitPath this'
>         path' = normalise path
>         this' = normalise this

> buildMenu :: FilePath -> [FilePath] -> Menu
> buildMenu this = foldl (extendMenu this) emptyMenu
>                  . map (first dropIndex . (id &&& dropExtension . takeFileName))

> addToMenu :: Rules
> addToMenu = group "menu" $ mapM_ (`create` normalDest) =<< resources

> getMenu :: Compiler a String
> getMenu = this &&& items >>> arr (renderHtml . showMenu . uncurry buildMenu)
>   where items = requireAll_ $ inGroup $ Just "menu"
>         this = getRoute >>> arr (fromMaybe "/")

> addMenu :: Compiler (Page a) (Page a)
> addMenu = id &&& getMenu >>> setFieldA "menu" id

Extracting descriptive texts from small programs.
---

> shDocstring :: String -> String
> shDocstring = unlines
>               . map (drop 2)
>               . takeWhile ("#" `isPrefixOf`)
>               . dropWhile (all (`elem` "# "))
>               . dropWhile ("#!" `isPrefixOf`)
>               . lines

> cDocstring :: String -> String
> cDocstring = unlines
>              . map (dropWhile (==' ')
>                     . dropWhile (=='*')
>                     . dropWhile (==' '))
>              . maybe [] lines
>              . (return . reverse . cut . reverse
>                 <=< find ("*/" `isSuffixOf`) . inits
>                 <=< return . cut
>                 <=< find ("/*" `isPrefixOf`) . tails)
>   where cut s | "/*" `isPrefixOf` s = cut $ drop 2 s
>               | otherwise = dropWhile isSpace s

> hsDocstring :: String -> String
> hsDocstring = unlines
>               . map (drop 3)
>               . takeWhile ("--" `isPrefixOf`)
>               . dropWhile ("#!" `isPrefixOf`)
>               . lines

> hackCompiler :: Compiler Resource (Page String)
> hackCompiler = proc r -> do
>   res <- getResourceString -< r
>   desc <- byExtension (arr shDocstring)
>           [(".c", arr cDocstring)
>           ,(".hs", arr hsDocstring)] -< res
>   ident <- getIdentifier -< ()
>   name <- arr (takeFileName . toFilePath) -< ident
>   dest <- normalDest -< ()
>   arr (fromBody
>        . writePandoc
>        . uncurry (readPandoc Markdown))
>         -< (Just ident, "[`"++name++"`](/"++dest++")\n" ++"---\n"++desc)

> addHacks :: Compiler (Page String) (Page String)
> addHacks = requireAllA (inGroup $ Just "hacks") (arr asList)
>   where asList (p,hs) =
>           p { pageBody = pageBody p ++ renderHtml (H.ul $ mapM_ asLi hs) }
>         asLi = H.li . H.preEscapedString . pageBody

Listing configuration files.
---

> groupPaths :: [Page FilePath] -> [[(FilePath,FilePath)]]
> groupPaths = map collapse . groupBy samedir . sortBy (comparing dir)
>   where samedir x y = dir x == dir y && dir x /= ["./"]
>         dir = take 1 . splitPath . addTrailingPathSeparator . takeDirectory . pageBody
>         collapse = map (pageBody &&& getField "url")

> addConfigs :: Compiler (Page String) (Page String)
> addConfigs = requireAllA (inGroup $ Just "configs")
>              (arr (second groupPaths) >>> arr addList)
>   where addList (p,cs) =
>           p { pageBody = pageBody p ++ renderHtml (H.ul $ mapM_ asLi cs) }
>         asLi l = case progname l of
>                    Nothing -> ""
>                    Just k | "." `isPrefixOf` k -> return ()
>                           | otherwise -> H.li $ do
>                               H.toHtml k >> H.ul (mapM_ disp l)
>         disp (c,u) = H.li $ H.a (H.toHtml $ filename c)
>                           ! A.href (H.toValue $ '/':u)
>         filename c = case splitPath c of
>                        []     -> ""
>                        [x]    -> x
>                        (_:xs) -> joinPath xs
>         progname []        = Nothing
>         progname ((x,_):_) = Just $ dropTrailingPathSeparator
>                                   $ joinPath $ take 1 $ splitPath
>                                   $ takeDirectory x

Including file sources
---

> addSourceButton :: Compiler (Page a) (Page a)
> addSourceButton = proc p -> do
>   sd <- destInGroup $ Just "source" -< ()
>   returnA -< case sd of Nothing -> p
>                         Just sd' -> changeField "topitems" (++button sd') p
>   where button u = renderHtml $ H.li $ H.a "source"
>                    ! A.href (H.toValue $ "/" ++ u)

> complement :: Pattern a -> Pattern a
> complement p = predicate (not . matches p)

> main :: IO ()
> main = hakyllWith config $ do
>   _ <- match "css/*" $ do
>     route   idRoute
>     compile compressCssCompiler

>   _ <- match "files/**" static

>   _ <- match "pubkey.asc" static

>   let inContentDir x = any (`matches` x)
>                        ["config/**", "writings/**", "hacks/**"
>                        , "programs/**", "projects/**", "*.md"]
>       content = predicate inContentDir `mappend` nothidden
>       nothidden = mconcat [complement "**/.**", complement ".*/**"]
>       contentPages = content `mappend` regex "\\.(md|lhs|man)$"
>       contentData = content `mappend` complement contentPages

>   _ <- match contentData static

>   _ <- match contentPages $ do
>     route $ setExtension "html"
>     addToMenu
>     compile $ extPageCompiler
>               >>> byPattern id [("hacks/index.md", addHacks)
>                                ,("config/index.md", addConfigs)]
>               >>> finalizePage

>   _ <- group "source" $ match (contentPages `mappend` "**lhs") static

>   _ <- group "hacks" $ match "hacks/scripts/*" $ compile hackCompiler

>   _ <- group "configs" $ match ("config/configs/**" `mappend` nothidden) $
>     compile $ getIdentifier
>               >>> arr (joinPath . drop 2 . splitPath . toFilePath)
>               >>> normalDest &&& arr fromBody
>               >>> arr (uncurry $ setField "url")

>   match "templates/*" $ compile templateCompiler

> config :: HakyllConfiguration
> config = defaultHakyllConfiguration
>   { deployCommand = "rsync --chmod=Do+rx,Fo+r --checksum -ave 'ssh -p 22' \
>                      \_site/* --exclude pub athas@sigkill.dk:/var/www/sigkill.dk"
>   }

> static :: Rules
> static = route idRoute >> compile copyFileCompiler >> return ()

> destInGroup :: (Maybe String) -> Compiler a (Maybe String)
> destInGroup g = getIdentifier >>> arr (setGroup g) >>> getRouteFor

> normalDest :: Compiler a String
> normalDest = destInGroup Nothing >>> arr (fromMaybe "/")

> finalizePage :: Compiler (Page String) (Page String)
> finalizePage = arr (trySetField "topitems" "")
>                >>> addMenu
>                >>> addSourceButton
>                >>> setTitle
>                >>> applyTemplateCompiler "templates/default.html"
>                >>> relativizeUrlsCompiler

> setTitle :: Compiler (Page b) (Page b)
> setTitle = id &&& getIdentifier >>> setFieldA "title" (arr title)
>   where title = dropIndex . dropExtension . toFilePath

> extPageCompiler :: Compiler Resource (Page String)
> extPageCompiler = byPattern pageCompiler [("**.man", manCompiler)]

> manCompiler :: Compiler Resource (Page String)
> manCompiler = getResourceString
>               >>> unixFilter "groff" (words "-m mandoc -T utf8")
>               >>> unixFilter "col" ["-b"]
>               >>> arr (fromBody . renderHtml . H.pre . H.toHtml)

> dropIndex :: FilePath -> FilePath
> dropIndex p | takeBaseName p == "index" = dropFileName p
>             | otherwise                 = p
