Solutions to exercises in [Programming with Arrows][1]
===

Since John Hughes wrote his paper, Arrows have become part of the
Haskell standard library.  The now-standard version differs
incompatibly from the one described in the paper, in that Arrows have
to be instances of the
[Category](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Category.html#t:Category)
typeclass, so these solutions have been updated to run under the
modern, standard definition of Haskell Arrows.  The program depends on
a similarly updated `Sim` module, which can be found [here](Sim.lhs).
The [updated Circuits module](Circuits.lhs) might also be of interest.
The code has been written with explicit type signatures for top-level
bindings and with an attempt to remove all compiler warnings (although
GHC changes so rapidly that it may well complain about something new
by the time you read this).

[1]: http://www.cs.chalmers.se/~rjmh/afp-arrows.pdf

Formalia
---

The Arrows language pragma enables the pointed Arrow notation in GHC.

> {-# LANGUAGE Arrows #-}
>
> import Control.Arrow
> import Control.Category
> import Control.Monad
> import Control.Monad.Fix
> import Data.Either

Function composition and the identity operation are both defined more
generally in `Control.Category` than in the Prelude, and we hide the
names `and` and `or` so we can use them to name circuit components
without getting compiler warnings.

> import Prelude hiding ((.), id, and, or)

We prefer the definition of `nor` from the `Sim` module.  While
`Circuits.nor` works with any Arrow, `Sim.nor` simulates gate delay,
which is useful when we actually need to use `nor`.

> import Circuits hiding (nor)
> import Sim

The SF arrow
---

Copied from the paper, apart from factoring part of the Arrow
instance out as the Category instance.

> newtype SF a b = SF { runSF :: [a] -> [b] }
>
> instance Category SF where
>   id = SF id
>   SF f . SF g = SF (f . g)
>
> instance Arrow SF where
>   arr f = SF (map f)
>   first (SF f) = SF (unzip >>> first f >>> uncurry zip)
>
> instance ArrowChoice SF where
>   left (SF f) = SF $ \xs -> combine xs (f $ lefts xs)
>     where combine (Left _:xs) (z:zs) = Left z : combine xs zs
>           combine (Right y:xs) zs = Right y : combine xs zs
>           combine [] _ = []
>           combine _  _ = error "Something impossible happened in ArrowChoice SF"
>
> instance ArrowLoop SF where
>   loop (SF f) = SF $ \as ->
>     let (bs,cs) = unzip (f (zip as (stream cs))) in bs
>       where stream ~(x:xs) = x:stream xs
>
> instance ArrowCircuit SF where
>   delay x = SF (init . (x:))

Exercise 2.5-1
---

See Exercise 3.5-3 below for a prettier version using pointed arrow
notation.  This implementation is merely a desugared version of
that one.

> filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
> filterA p = arr listcase >>> -- Check if list is empty.
>             (arr (const []) ||| -- Empty, so stop.
>              ((arr fst >>> p) &&& id -- Check if head of list fulfills predicate.
>               >>> arr check -- Convert tuple with bool to Left/Right.
>               >>> ((arr snd >>> filterA p) ||| -- Discard head.
>                    (arr id *** filterA p >>> arr (uncurry (:)))))) -- Keep head.
>   where listcase [] = Left ()
>         listcase (x:xs) = Right (x,xs)
>         check (False, v) = Left v
>         check (True, v)  = Right v

Exercise 2.5-2
---

> data SP a b = Put b (SP a b) | Get (a -> SP a b)
>
> runSP :: SP a b -> [a] -> [b]
> runSP (Put b s) as = b:runSP s as
> runSP (Get k) (a:as) = runSP (k a) as
> runSP (Get _) [] = []
>
> instance Category SP where
>   id = Get $ flip Put id
>   Put v f . g = Put v (f . g)
>   Get f . Put v g = f v . g
>   Get f . Get g = Get $ \x -> Get f . g x


Defining first presents a problem, as consuming one element of
input does not necessarily produce exactly one element of output.
The synchronisation function I have implemented opts to never throw
away and never duplicate data, but that implies that the lists of
"delayed" I/O may grow with no upper bound.  It is not hard to
modify the function to follow some other strategy with constant
upper bounds on buffering requirements.  And of course, the use of
lists here is highly inefficient, a proper deque (Data.Sequence,
for example) should be used.

> instance Arrow SP where
>   arr f = Get $ \x -> Put (f x) (arr f)
>   first f = sync f [] []
>
> sync :: SP a c -> [a] -> [b] -> SP (a, b) (c, b)
> sync (Put v f) xs (y:ys) = Put (v,y) (sync f xs ys)
> sync (Put v f) xs []     = Get $ \(x,y) -> Put (v,y) (sync f (xs++[x]) [])
> sync (Get f)   (x:xs) ys = sync (f x) xs ys
> sync (Get f)   [] ys     = Get $ \(x,y) -> sync (f x) [] (ys++[y])
>
> instance ArrowChoice SP where
>   left (Put v f) = Put (Left v) (left f)
>   left (Get f) = Get g
>     where g (Left x)  = left $ f x
>           g (Right x) = Put (Right x) (left $ Get f)

We traverse down the stream function to look for an 'initial' state,
then run the function with that state.  This depends critically on
lazy evaluation.

> instance ArrowLoop SP where
>   loop f = let (f',s) = next f s in f'
>     where next (Put (v,s) f') _ = (Put v (fst $ next f' s), s)
>           next (Get f')       s = (Get $ \x -> let (f'',s') = next (f' (x,s')) s
>                                                in f'', s)
>
> instance ArrowCircuit SP where
>   delay v = Put v id

Exercise 3.5-1
---

> halfAdd :: Arrow arr => arr (Bool,Bool) (Bool, Bool)
> halfAdd = arr $ \(x,y) -> (x&&y, x/=y)
>
> fullAdd :: Arrow arr => arr (Bool, Bool, Bool) (Bool, Bool)
> fullAdd = proc (x,y,c) -> do
>             (c1,s1) <- halfAdd -< (x,y)
>             (c2,s2) <- halfAdd -< (s1, c)
>             returnA -< (c1||c2,s2)

It is instructive to see the desugared version:

> fullAddNoSugar :: Arrow arr => arr (Bool, Bool, Bool) (Bool, Bool)
> fullAddNoSugar = arr (\(x,y,c) -> ((x,y), c))
>                  >>> first halfAdd
>                  >>> arr (\((c1,s1),c) -> ((s1,c), c1))
>                  >>> first halfAdd
>                  >>> arr (\((c2,s2),c1) -> (c1||c2,s2))

The *n*-bit adder takes an argument, `m`, which is the number of bits
in the inputs, and the adder will fail if the input lists are not each
of length `m`.  The helper function `more` constructs the `m` adders,
connecting the carry-out of one to the carry-in of the next.  The
carry-in of the first adder is always wired to `False`, and the
carry-out of the last one is passed out of the entire adder.

> adder :: Arrow arr => Int -> arr ([Bool], [Bool]) ([Bool],Bool)
> adder m = proc (x, y) -> more m -< (x,y,False)
>   where more 0 = proc (_,_,cout) -> returnA -< ([],cout)
>         more n = proc (x':xs,y':ys,cin) -> do
>           (carry,b) <- fullAdd -< (x',y',cin)
>           (z,cout) <- more (n-1) -< (xs,ys,carry)
>           returnA -< (b:z, cout)

Exercise 3.5-2
---

> bsadd :: ArrowCircuit arr => arr (Bool,Bool) Bool
> bsadd = proc (x,y) -> do
>           rec (cout,b) <- fullAdd -< (x,y,cin)
>               cin      <- delay False -< cout
>           returnA -< b

Exercise 3.5-3
---

(a)
---

> filterA' :: ArrowChoice arr => arr a Bool -> arr [a] [a]
> filterA' p = proc xs ->
>   case xs of
>     [] -> returnA -< []
>     x:xs' -> do c <- p -< x
>                 l <- filterA' p -< xs'
>                 if c then arr (uncurry (:)) -< (x,l)
>                      else returnA -< l

(b)
---

I admit that I do not yet have much intuition for command combinators.

> filterC :: ArrowChoice arr => arr (env,a) Bool -> arr (env,[a]) [a]
> filterC p = proc (env, xs) ->
>   case xs of
>     [] -> returnA -< []
>     x:xs' -> do c <- p -< (env,x)
>                 l <- filterC p -< (env,xs')
>                 if c then arr (uncurry (:)) -< (x,l)
>                      else returnA -< l

Exercise 3.5-4
---

(a)
---

> rowC :: Arrow arr =>
>         Int -> arr (env,(a,b)) (c,a) -> arr (env,(a,[b])) ([c],a)
> rowC 0 _ = proc (_,(a,[])) -> returnA -< ([],a)
> rowC n f = proc (env,(a,b:bs)) -> do
>              (c,a') <- f -< (env,(a,b))
>              (cs,a'') <- rowC (n-1) f -< (env, (a',bs))
>              returnA -< (c:cs,a'')

(b)
---

> counter1bit :: ArrowCircuit arr => arr Bool (Bool,Bool)
> counter1bit = proc b -> do
>                 rec sumin <- delay False -< sum
>                     (carry,sum) <- halfAdd -< (sumin,b)
>                 returnA -< (sum, carry)

(c)
---

> counternbit :: ArrowCircuit arr => Int -> arr Bool ([Bool],Bool)
> counternbit n = proc bs ->
>   (|(rowC n) (\(b,_) -> counter1bit -< b)|) (bs, [0..n-1])

(d)
---

> counter1bit' :: ArrowCircuit arr => arr (Bool,Bool) (Bool,Bool)
> counter1bit' = proc (b,rst) -> do
>                  rec sumin <- delay False -< sum
>                      (carry,sum) <- halfAdd -< (not rst && sumin,not rst && b)
>                  returnA -< (sum, carry)
>
> counternbit' :: ArrowCircuit arr => Int -> arr (Bool,Bool) ([Bool],Bool)
> counternbit' n = proc (bs,rsts) ->
>   (|(rowC n) (\(b,rst) -> counter1bit' -< (b,rst))|) (bs, replicate n rsts)

Exercise 5.6-1
---

Note that since Sim is an Arrow, the trivial solution to this exercise
is to simple reuse the previous definitions.  Instead, I will define
"realistic" circuits using NOR logic.  Since the `nor` gate simulates
gate delay, this means all derived gates will also have proper delays.

> and :: Monad m => Sim m (Bool,Bool) Bool
> and = proc (a,b) -> do
>         a' <- nor -< (a,a)
>         b' <- nor -< (b,b)
>         nubA <<< nor -< (a',b')
>
> or :: Monad m => Sim m (Bool,Bool) Bool
> or = proc (a,b) -> do
>         a' <- nor -< (a,b)
>         b' <- nor -< (a,b)
>         nubA <<< nor -< (a',b')
>
> lnot :: Monad m => Sim m Bool Bool
> lnot = nubA <<< nor <<< id &&& id
>
> xor :: Monad m => Sim m (Bool, Bool) Bool
> xor = proc (a,b) -> do
>         c <- and -< (a,b)
>         d <- nor -< (a,b)
>         nubA <<< nor -< (c,d)
>
> halfAddSim :: Monad m => Sim m (Bool,Bool) (Bool,Bool)
> halfAddSim = proc (x,y) -> do
>              c <- and -< (x,y)
>              s <- xor -< (x,y)
>              nubA -< (c,s)

From here on, the definitions are mostly trivial substitutions of the
previous ones, as they are all ultimately built on top of half-adders.
In fact, rowC is not changed at all.  nubA has been used to remove
glitches.  Note that the circuits are now highly sensitive to timing
issues and should be extended with a clock input to be useful for
anything.

> fullAdd' :: Monad m => Sim m (Bool,Bool,Bool) (Bool,Bool)
> fullAdd' = proc (x,y,c) -> do
>              (c1,s1) <- halfAddSim -< (x,y)
>              (c2,s2) <- halfAddSim -< (s1,c)
>              c'      <- or -< (c1,c2)
>              nubA -< (c',s2)
>
> bsadd' :: MonadFix m => Sim m (Bool,Bool) Bool
> bsadd' = proc (x,y) -> do
>            (b,_) <- (|afix (\ ~(_,cin) -> do
>                               cin' <- initially False nubA -< cin
>                               (cout,b) <- fullAdd' -< (x,y,cin')
>                               returnA -< (b,cout))|)
>            returnA -< b
>
> counter1bit'' :: Sim IO (Bool,Bool) (Bool,Bool)
> counter1bit'' = proc (b,rst) ->
>                 (|afix (\ ~(sumin,_) -> do
>                           sumin' <- initially False nubA -< sumin
>                           x <- and <<< first lnot <<< delay1 0.2 -< (rst,sumin')
>                           y <- and <<< first lnot <<< nubA -< (rst,b)
>                           (c,s) <- halfAddSim -< (x,y)
>                           returnA -< (s,c))|)
>
> counternbit'' :: Int -> Sim IO (Bool,Bool) ([Bool],Bool)
> counternbit'' n = proc (bs,rsts) ->
>   (|(rowC n) (\(b,rst) -> counter1bit'' -< (b,rst))|) (bs, replicate n rsts)

Note that the following is an orphan instance, since it is defined
outside the modules where Sim or ArrowChoice is defined.

> instance Monad m => ArrowChoice (Sim m) where
>   left (Sim f) = sim left'
>     where left' (Left b) = do (c,s) <- f b
>                               return (Left c,leftState b s)
>           left' (Right d) = return (Right d, untilLeft f)
>
> untilLeft :: Monad m => (b -> m (c, State m b c))
>           -> (State m (Either b d) (Either c d))
> untilLeft f = Wait infinity undefined $ \e ->
>               case value e of
>                 Right d -> Ready (Event (time e) (Right d)) (untilLeft f)
>                 Left  b -> Lift $ do
>                              (c,s) <- f b
>                              return $ Ready (Event (time e) (Left c)) (leftState b s)
>
> leftState :: Monad m => b -> State m b c -> State m (Either b d) (Either c d)
> leftState b (Ready e s) = Ready (e {value = Left (value e)}) (leftState b s)
> leftState b (Lift m) = Lift (liftM (leftState b) m)
> leftState b (Wait t s k) = Wait t (leftState b s) $ \e ->
>                            case value e of
>                              Left b' -> leftState b' $ k e { value = b' }
>                              Right d -> Ready (Event (time e) (Right d))
>                                         (leftState b $ k e { value = b })
>
> maybeneg :: Monad m => Sim m (Bool,Bool) Bool
> maybeneg = proc (x,neg) -> if neg then returnA -< not x
>                            else returnA -< x

