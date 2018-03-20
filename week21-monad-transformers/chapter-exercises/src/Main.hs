module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

{- 1 -}
rDec :: Num a => Reader a a
rDec = reader (\x -> x - 1)

{- 2 -}
rDec2 :: Num a => Reader a a
rDec2 = reader (subtract 1)

{- 3, 4 -}
rShow :: Show a => ReaderT a Identity String
rShow = reader show

{- 5 -}
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  env <- ask
  liftIO $ putStrLn $ "Hi: " ++ show env
  return (env + 1)

{- 6 -}
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  s <- get
  liftIO $ putStrLn $ "Hi: " ++ show s
  put (s + 1)
  return (show s)

{- 7 -}
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

main :: IO ()
main = do
  print $ runReader rDec (20 :: Int)
  print $ runReader rDec2 (20 :: Int)
  print $ runReader rShow (20 :: Int)
  print =<< runReaderT rPrintAndInc (20 :: Int)
  print =<< runStateT sPrintIncAccum (20 :: Int)
