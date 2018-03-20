{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config
  -- that's one, one click!
  -- two...two clicks!
  -- Three BEAUTIFUL clicks! ah ah ahhhh
  { counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = undefined

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    cfg <- lift ask
    let key' = mappend (prefix cfg) unprefixed
    liftIO $ modifyIORef (counts cfg) (M.insertWith (+) key' 1)
    m <- liftIO $ readIORef (counts cfg)
    let newInteger = fromMaybe 1 (M.lookup key' m)
    html $
      mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR = flip runReaderT config
  scottyT 3000 runR app
