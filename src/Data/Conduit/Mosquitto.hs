module Data.Conduit.Mosquitto (
    sourceMosquitto
  , sinkMosquitto
  ) where

import           Data.Conduit
import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Control.Monad.Trans.Resource
import           Control.Exception
import qualified Network.Mosquitto as M

sourceMosquitto :: (MonadResource m) => M.Mosquitto -> Source m M.Event
sourceMosquitto mosquitto = do
    (result, events) <- liftIO $ M.getNextEvents mosquitto 500
    mapM_ yield events
    if result == 0
        then sourceMosquitto mosquitto
        else liftIO $ do
            str <- M.strerror result
            throwIO $ AssertionFailed $ "M.getNextEvent failed (" ++ show result ++ "): " ++ str

sinkMosquitto :: (MonadResource m) => (M.Event -> Bool) -> Sink M.Event m ()
sinkMosquitto predicate = loop
  where
    loop :: (MonadResource m) => Sink M.Event m ()
    loop = await >>= maybe (return ()) (\event -> unless (predicate event) loop)

