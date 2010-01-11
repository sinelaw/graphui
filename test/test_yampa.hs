{-# LANGUAGE Arrows #-}


import qualified FRP.Yampa as Yampa

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
              Yampa.returnA -< s ++ " <--"

  