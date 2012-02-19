Circuit arrows
===

Simple update of the circuit arrows from *Programming with Arrows* to
work with the modern Arrow libraries.

> module Circuits where
>
> import Control.Arrow
> import Data.List
>
> class ArrowLoop a => ArrowCircuit a where
>   delay :: b -> a b b
>
> nor :: Arrow a => a (Bool,Bool) Bool
> nor = arr (not.uncurry (||))
>
> flipflop :: ArrowCircuit a => a (Bool,Bool) (Bool,Bool)
> flipflop = loop (arr (\((a,b),(c,d)) -> ((a,d),(b,c))) >>>
>                  nor *** nor >>>
>                  delay (False,True) >>>
>                  arr id &&& arr id)
>
> class Signal a where
>   showSignal :: [a] -> String
>
> instance Signal Bool where
>   showSignal bs = concat top++"\n"++concat bot++"\n"
>     where (top,bot) = unzip (zipWith sh (False:bs) bs)
>           sh True True = ("__","  ")
>           sh True False = ("  ","|_")
>           sh False True = (" _","| ")
>           sh False False = ("  ","__")
>
> instance (Signal a,Signal b) => Signal (a,b) where
>   showSignal xys = showSignal (map fst xys)++showSignal (map snd xys)
>
> instance Signal a => Signal [a] where
>   showSignal = concatMap showSignal . transpose
>
> sig :: [(Int, a)] -> [a]
> sig = concatMap (uncurry replicate)
>
> flipflopInput :: [(Bool, Bool)]
> flipflopInput = sig
>         [(5,(False,False)),(2,(False,True)),(5,(False,False)),
>          (2,(True,False)),(5,(False,False)),(2,(True,True)),
>          (6,(False,False))]
