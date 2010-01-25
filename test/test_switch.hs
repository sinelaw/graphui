{-# LANGUAGE Arrows #-}

import qualified FRP.Yampa as Yampa


import Debug.Trace(trace)
traceId x = trace (show x) x

main :: IO ()
main = do
  Yampa.reactimate initial sense actuate processor
  return ()
  

initial :: IO Double
initial = do
  return 0
  
sense :: Bool -> IO (Yampa.DTime, Maybe Double)
sense _ = do
  res <- getLine
  return (0.1, Just (read res))
  
actuate :: Bool -> Double -> IO Bool
actuate updated out = do
  print out
  return False

processor :: Yampa.SF Double Double
processor = proc (s) -> do 
              let e' = Yampa.Event s
              r <- dP -< e'
              Yampa.returnA -< r
              --Yampa.returnA -< "BLA"
    where dP = dProcessor linearInterp 0 0

linearInterp a b = proc (t) -> do
                     let res = lInterp a b t 
                         lInterp a b t | t <= 0 = a
                                    | t >= 1 = b
                                    | otherwise = t/(b-a)
                     Yampa.returnA -< res


--dProcessor :: (a -> a -> Yampa.SF a c) -> a -> a -> Yampa.SF (Yampa.Event a) c
dProcessor interpolator a b = (Yampa.time Yampa.&&& (eventPairs Yampa.NoEvent)) Yampa.>>> (eventSource interpolator b) Yampa.>>> (Yampa.rSwitch (interpolator a b))
  
-- dSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
eventSource :: (b -> c -> a) -> c -> Yampa.SF (d, Yampa.Event (b, c)) (d, Yampa.Event a)
eventSource interp a_init = proc (d, x) -> do
                              let ev' = case x of
                                          Yampa.Event (old,new) -> (d, Yampa.Event (interp old new))
                                          Yampa.NoEvent -> (d, Yampa.NoEvent)
                              Yampa.returnA -< (ev')

eventPairs :: (Yampa.Event (a,a)) -> Yampa.SF (Yampa.Event a) (Yampa.Event (a,a))
eventPairs a_init = sscanPrim' f a_init 
  where f (Yampa.Event (older, old)) (Yampa.Event new) = Just (Yampa.Event (old, new))
        f (Yampa.NoEvent) (Yampa.Event new) = Just (Yampa.Event (new, new))
        f _ _ = Nothing
                      


sscanPrim' :: (b -> a -> Maybe b) -> b -> Yampa.SF a b
sscanPrim' f b_init = Yampa.sscanPrim f' b_init b_init
    -- changes Just x into Just (x, x)
    where f' b a = case (f b a) of
            Nothing -> Nothing
            Just x -> Just (x,x)
