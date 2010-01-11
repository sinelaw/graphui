{-# LANGUAGE Arrows #-}


import qualified FRP.Yampa as Yampa

main :: IO ()
main = do
  Yampa.reactimate initial sense actuate processor
  return ()
  

initial :: IO String
initial = do
  return "Initial"
  
sense :: Bool -> IO String
sense _ = do
  return (1, Just "sense")
  
actuate :: Bool -> Int -> IO Bool
actuate updated out = do
  print out
  return False

processor :: Yampa.SF String Int
processor = proc (s) -> do 
              Yampa.returnA -< 3

  