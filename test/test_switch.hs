{-# LANGUAGE Arrows #-}

import qualified FRP.Yampa as Yampa


import Debug.Trace(trace)
traceId x = trace (show x) x

main :: IO ()
main = do
  Yampa.reactimate initial sense actuate processor
  return ()
  

initial :: IO String
initial = do
  return ""
  
sense :: Bool -> IO (Yampa.DTime, Maybe String)
sense _ = do
  res <- getLine
  return (1, Just res)
  
actuate :: Bool -> String -> IO Bool
actuate updated out = do
  print out
  return False

processor :: Yampa.SF String String
processor = proc (s) -> do 
              let e' = Yampa.Event s
              (r1,r2,r3) <- dP -< e'
              Yampa.returnA -< r1++" "++r2++" "++r3
              --Yampa.returnA -< "BLA"
    where dP = dProcessor "A" "B"

dProcessor :: a -> a -> Yampa.SF (Yampa.Event a) (a, a, a)
dProcessor a b = (eventPairs Yampa.NoEvent) Yampa.>>> (eventSource interpolator b) Yampa.>>> (Yampa.rSwitch (interpolator a b))
  
-- dSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
eventSource :: (b -> c -> a) -> c -> Yampa.SF (Yampa.Event (b, c)) (c, Yampa.Event a)
eventSource interp a_init = proc (x) -> do
                              let ev' = case x of
                                          Yampa.Event (old,new) -> (new, Yampa.Event (interp old new))
                                          Yampa.NoEvent -> (a_init, Yampa.NoEvent)
                              Yampa.returnA -< (ev')

eventPairs :: (Yampa.Event (a,a)) -> Yampa.SF (Yampa.Event a) (Yampa.Event (a,a))
eventPairs a_init = sscanPrim' f a_init 
  where f (Yampa.Event (older, old)) (Yampa.Event new) = Just (Yampa.Event (old, new))
        f (Yampa.NoEvent) (Yampa.Event new) = Just (Yampa.Event (new, new))
        f _ _ = Nothing
                      

interpolator :: a -> b -> Yampa.SF c (a, b, c)
interpolator a b = Yampa.arr (\x -> (a,b,x))

sscanPrim' :: (b -> a -> Maybe b) -> b -> Yampa.SF a b
sscanPrim' f b_init = Yampa.sscanPrim f' b_init b_init
    -- changes Just x into Just (x, x)
    where f' b a = case (f b a) of
            Nothing -> Nothing
            Just x -> Just (x,x)
