module Main where

import qualified Network.Mosquitto as M
import qualified Data.Conduit.Mosquitto as CM

import           Control.Monad.Trans(liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit

main :: IO ()
main = do
    M.withInit $ M.withMosquitto Nothing $ \mosquitto -> do
        M.connect mosquitto "localhost" 1883 500
        M.subscribe mosquitto "#" 1
        runResourceT $ CM.sourceMosquitto mosquitto $$ conduit =$ CM.sinkMosquitto (const False)
        M.disconnect mosquitto
        return ()
  where
    conduit = awaitForever (liftIO . print)

