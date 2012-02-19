Simulation arrows
===

Simple update of the simulation arrows from *Programming with Arrows*
to work with the modern Arrow libraries.

> {-# LANGUAGE Arrows #-}
> module Sim where
>
> --  discrete event simulation library.
> -- This time, every channel always carries a value, and an *initial* value must
> -- be supplied before simulation starts.
>
> import Control.Category
> import Control.Monad
> import Control.Monad.Fix
> import Control.Arrow
> import Data.IORef
>
> import Prelude hiding ((.), id)
>
> type Time = Double
>
> infinity :: Time
> infinity = 1/0
>
> data Event a = Event {time::Time, value::a}
>
> instance Show a => Show (Event a) where
>   show t = show (value t)++"@"++show (time t)
>
> -- The simulation arrow: given initial value of input signal, deliver initial
> -- value of output signal and a running simulation.
> -- invariant: no output event should precede the first input.
>
> newtype Sim m a b = Sim (a -> m (b, State m a b))
>
> sim :: Monad m => (a -> m (b, State m a b)) -> Sim m a b
> sim f = Sim $ \a -> do
>   (b,s) <- f a
>   return (b,quiescent s)
>
> quiescent :: Monad m => State m a b -> State m a b
> quiescent (Lift m) = Lift (liftM quiescent m)
> quiescent (Wait t s k) = wait t (quiescent s) k
> quiescent (Ready _ _) = error "Trying to output before first input"
>
> -- running simulations.
> -- invariant: output events are in non-decreasing time order,
> --            output events do not precede inputs or timeouts they depend on,
> --            enforced by smart constructors
>
> data State m a b = Ready (Event b) (State m a b)
>                  | Lift (m (State m a b))
>                  | Wait Time (State m a b) (Event a -> State m a b)
>
> ready :: Monad m => Event a1 -> State m a a1 -> State m a a1
> ready e r = Ready e (checkSequence ("Ready "++show (time e)) (time e) r)
>
> lift :: Monad m => m (State m a b) -> State m a b
> lift = Lift
>
> wait :: Monad m =>
>         Time -> State m a b -> (Event a -> State m a b) -> State m a b
> wait t f k = Wait t (checkSequence ("Wait "++show t) t f)
>                 (\e -> checkSequence
>                          ("Wait "++show t++" \\"++show (time e)++" ->")
>                          (time e) (k e))
>
> -- ensure all outputs occur no earlier than t
>
> {-
> -- checkSequence is a version of causal which maintains a trace of events
> -- to report on an eventual failure. Useful for debugging new arrows.
> -- If debugging is unnecessary, it can be replaced by causal.
>
> checkSequence s t (Ready e f) | t <= time e = Ready e f
> checkSequence s t (Lift m) =
>   Lift (liftM (checkSequence (s++"\nLift") t) m)
> checkSequence s t (Wait t' f k) =
>   Wait t' (checkSequence (s++"\nWait "++show t') t f)
>     (\e -> checkSequence
>              (s++"\nWait "++show t'++" \\"++show (time e)++" ->")
>              t (k e))
> checkSequence s t (Ready e f) =
>   error $ "checkSequence: "++show t++" > "++show (time e)++"\n"++s++
>                           "\nReady "++show (time e)
> -}
>
> checkSequence :: Monad m => t -> Time -> State m a b -> State m a b
> checkSequence _ = causal
>
> causal :: Monad m => Time -> State m a b -> State m a b
> causal t (Ready e f) | t <= time e = Ready e f
>                      | otherwise = error "Violation of causality"
> causal t (Lift m) = Lift (liftM (causal t) m)
> causal t (Wait t' s k) = Wait t' (causal t s) (causal t.k)
>
> -- run function supplies initial value and input events, and runs simulation
> -- in the underlying monad.
> runSim :: Monad m => Sim m t t1 -> t -> [Event t] -> m ()
> runSim (Sim f) a as = do
>   (_,r) <- f a
>   runState r as
>
> runState :: Monad m => State m t t1 -> [Event t] -> m ()
> runState (Ready _ s) as = runState s as
> runState (Lift m) as = do s <- m
>                           runState s as
> runState (Wait t s _) []
>   | t==infinity = return ()             -- infinity never comes
>   | otherwise   = runState s []         -- timeout
> runState (Wait t s k) (a:as)
>   | t <= time a = runState s (a:as)     -- timeout
>   | otherwise   = runState (k a) as     -- receive event
>
> -- Transition function when a simulation receives an input
> after :: Monad m => State m a a1 -> Event a -> State m a a1
> Ready b s  `after` a = ready b (s `after` a)
> Lift m     `after` a = lift (liftM (`after` a) m)
> Wait t s k `after` a
>   | t <= time a = s `after` a
>   | otherwise   = k a
>
> instance Monad m => Category (Sim m) where
>   id = simArr id
>   (.) = simComp
>
> instance Monad m => Arrow (Sim m) where
>   arr = simArr
>   first = simFirst
>
> simArr :: Monad m => (a -> b) -> Sim m a b
> simArr f = sim $ \a -> return (f a, s)
>   where s = waitInput (\a -> ready (Event (time a) (f (value a))) s)
>
> waitInput :: Monad m => (Event a -> State m a b) -> State m a b
> waitInput k = wait infinity undefined k
>
> simComp :: Monad m => Sim m t1 b -> Sim m t t1 -> Sim m t b
> Sim g `simComp` Sim f = sim $ \a -> do
>   (b,sf) <- f a
>   (c,sg) <- g b
>   return (c,sf `stateComp` sg)
>
> stateComp :: Monad m => State m t1 t -> State m t a1 -> State m t1 a1
> sf `stateComp` Ready c sg = ready c (sf `stateComp` sg)
> sf `stateComp` Lift m = lift (liftM (sf `stateComp`) m)
> Ready b sf `stateComp` sg = sf `stateComp` (sg `after` b)
> Lift m `stateComp` sg = lift (liftM (`stateComp` sg) m)
> Wait tf sf kf `stateComp` Wait tg sg kg =
>   wait (min tf tg) timeout (\a -> kf a `stateComp` Wait tg sg kg)
>   where timeout | tf<tg     = sf `stateComp` Wait tg sg kg
>                 | tf>tg     = Wait tf sf kf `stateComp` sg
>                 | otherwise = sf `stateComp` sg
>
> simFirst :: Monad m => Sim m a b -> Sim m (a, c) (b, c)
> simFirst (Sim f) = sim $ \(a,c) -> do
>   (b,s) <- f a
>   return ((b,c), stateFirst b c s)
>
> stateFirst :: Monad m => b -> c -> State m a b -> State m (a, c) (b, c)
> stateFirst b c (Ready b' s) =
>   wait (time b')
>        (ready (Event (time b') (value b',c)) (stateFirst (value b') c s))
>        (\(Event t' (a,c')) ->
>          ready (Event t' (b,c'))
>                (stateFirst b c' (ready b' (s `after` (Event t' a)))))
> stateFirst b c (Lift m) = Lift (liftM (stateFirst b c) m)
> stateFirst b c (Wait t s k) =
>   wait t (stateFirst b c s) $ \(Event t' (a,c')) ->
>     ready (Event t' (b,c')) (stateFirst b c' (k (Event t' a)))
>
> -- Can we define a loop?
>
> instance MonadFix m => ArrowLoop (Sim m) where
>   loop = simLoop
>
> simLoop :: MonadFix m => Sim m (t, t1) (b, t1) -> Sim m t b
> simLoop (Sim f) = sim $ \a -> do
>   ((b,c),s) <- mfix (\(~((_,c),_)) -> f (a,c))
>   return (b,stateLoop a c [] s)
>
> -- stateLoop a c q s
> --   a = initial value of input
> --   c = initial value of state
> --   q = queue of future state changes
> --   s = running simulation (a,c) to (b,c)
> -- result is a running simulation from a to b, where state changes are
> -- fed back at the appropriate times.
>
> stateLoop :: Monad m =>
>              a -> t -> [(Time, t)] -> State m (a, t) (b, t) -> State m a b
> stateLoop a c q (Ready (Event t (b,c')) s) =
>   ready (Event t b) (stateLoop a c (q++[(t,c')]) s)
> stateLoop a c q (Lift m) = lift $ liftM (stateLoop a c q) m
> stateLoop a c ((t',c'):q) (Wait t s k) =
>   wait (min t t') timeout $ \(Event t'' a') ->
>     stateLoop a' c ((t',c'):q) (k (Event t'' (a',c)))
>   where timeout | t'<t      = stateLoop a c' q (k (Event t' (a,c')))
>                 | t'>t      = stateLoop a c ((t',c'):q) s
>                 | otherwise = stateLoop a c' q (s `after` Event t (a,c'))
> stateLoop a c [] (Wait t s k) =
>   wait t (stateLoop a c [] s) $ \(Event t' a') ->
>     stateLoop a' c [] (k (Event t' (a',c)))
>
> -- arrM lifts a monadic function into a Sim arrow.
> arrM :: Monad m => (a -> m b) -> Sim m a b
> arrM f = sim $ \a -> do
>   b <- f a
>   return (b,s)
>   where s = waitInput $ \(Event t a) -> lift $ do
>               b <- f a
>               return (ready (Event t b) s)
>
> --printA prints all events that pass through
> printA :: Show b => [Char] -> Sim IO b b
> printA name = sim $ \a -> do
>   message (show a++"@init")
>   return (a,s)
>   where s = waitInput $ \a -> Lift $ do
>               message (show a)
>               return (ready a s)
>         message a = if null name then putStrLn a else putStrLn (name++": "++a)
>
> --delay1 d delays events by d, removing events at the same time
> delay1 :: Monad m => Time -> Sim m b b
> delay1 d = sim (\a -> return (a,r))
>   where r = waitInput go
>         go (Event t a) =
>           wait (t+d) (ready (Event (t+d) a) r) $ \(Event t' a') ->
>             if t==t'
>               then go (Event t' a')
>               else ready (Event (t+d) a) (go (Event t' a'))
>
> initially :: Monad m => b -> Sim m t b -> Sim m t b
> initially x (Sim f) = Sim $ \a -> do (_,s) <- f a
>                                      return (x,s)
>
> --nubA filters out events that repeat values
> nubA :: (Eq a, Monad m) => Sim m a a
> nubA = sim $ \a -> return (a,go a)
>   where go a = waitInput $ \(Event t a') ->
>                  if a==a' then go a else ready (Event t a') (go a')
>
> -- cutoff t s stops a simulation after time t
> cutoff :: Monad m => Time -> Sim m t b -> Sim m t b
> cutoff t (Sim f) = sim $ \a -> do
>                      (b,r) <- f a
>                      return (b, cutoffState t r)
>
> cutoffState :: Monad m =>
>                Time -> State m a a1 -> State m a a1
> cutoffState t (Ready b s)
>   | time b<=t = ready b (cutoffState t s)
>   | otherwise = stop
>   where stop = waitInput (const stop)
> cutoffState t (Lift m) = lift (liftM (cutoffState t) m)
> cutoffState t (Wait t' s k)
>   | t'<=t     = wait t' (cutoffState t s) (cutoffState t.k)
>   | otherwise = wait infinity undefined (cutoffState t.k)
>
> -- Experiments with arrow notation
>
> nor :: Monad m => Sim m (Bool,Bool) Bool
> nor = proc (a,b) -> do
>         (a',b') <- delay1 0.1 -< (a,b)
>         returnA -< not (a'||b')
>
> afix :: (MonadFix m, Eq b) => Sim m (a,b) b -> Sim m a b
> afix f = loop (f >>> nubA >>> arr id &&& arr id) >>> nubA
>
> flipflop :: MonadFix m => Sim m (Bool,Bool) (Bool,Bool)
> flipflop = proc (reset,set) ->
>                 (|afix (\ ~(x,y)->do
>                                 x' <- initially False nor -< (reset,y)
>                                 y' <- initially True nor -< (set,x)
>                                 returnA -< (x',y'))|)
>
> oscillator :: MonadFix m => Sim m Bool Bool
> oscillator = proc enable ->
>   (|afix (\x -> nor -< (enable,x))|)
>
>
> -- probe counts the transitions on a channel
> -- this is useful for estimating power consumption
>
> probe :: Metric a => String -> (Sim IO a a -> IO b) -> IO b
> probe s k = do r <- newIORef 0
>                ans <- k (probeArr r)
>                n <- readIORef r
>                putStrLn (s++": "++show n++" transitions")
>                return ans
>   where probeArr r = sim $ \a -> return (a, stateProbe r a)
>         stateProbe r a = waitInput $ \(Event t b) ->
>                              lift $ do
>                                modifyIORef r (+distance a b)
>                                return (ready (Event t b) (stateProbe r b))
>
> class Metric a where
>   distance :: a -> a -> Double
>   bound :: a -> Double  -- the distance between any two points is below bound
>                         -- bound does not evaluate its argument
>
> instance Metric Bool where
>   distance a b = if a==b then 0 else 1
>   bound _ = 1
>
> instance (Metric a, Metric b) => Metric (a,b) where
>   distance (a,b) (c,d) = distance a c+distance b d
>   bound ~(a,b)= bound a + bound b
>
> instance (Metric a, Metric b) => Metric (Either a b) where
>   distance (Left a) (Left a') = distance a a'
>   distance (Right b) (Right b') = distance b b'
>   distance x _ = 1 + (bound a `max` bound b)
>     where Left a = x
>           Right b = x
>   bound x = 1 + (bound l `max` bound r)
>     where Left l = x
>           Right r = x
>
> instance Metric a => Metric [a] where
>   distance [] [] = 0
>   distance (x:xs) (y:ys) = distance x y + distance xs ys
>   distance [] (y:ys) = (bound y+1)*(fromInteger (toInteger (length ys))+1)
>   distance xs [] = distance [] xs
>   bound _ = infinity
