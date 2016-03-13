module Main where

import qualified Network.Mosquitto as M
import qualified Data.Conduit.Mosquitto as CM

import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.ByteString as BS

main :: IO ()
main = do
    M.withInit $ M.withMosquitto Nothing $ \mosquitto -> do
        M.connect mosquitto "localhost" 1883 500
        (_, mid) <- M.publish mosquitto "mqtt/sample" (BS.pack [97..122]) 1 False
        runResourceT $ CM.sourceMosquitto mosquitto $$ CM.sinkMosquitto (isPublished mid)
        M.disconnect mosquitto
        return ()
  where
    isPublished waitingMessageId (M.Published mid) = waitingMessageId == mid
    isPublished _                _                 = False

