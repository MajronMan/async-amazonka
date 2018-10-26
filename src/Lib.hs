{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
    ( someFunc
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Time as Time
import Control.Exception
import Control.DeepSeq
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad



import Future

action1 :: IO Int
action1 = do
    threadDelay 500000 -- just to make it interesting
    return 5

action2 :: IO String
action2 = do
    threadDelay 1000000
    return "action2 result"

-- someFunc :: IO ()
-- someFunc = do
--   start <- Time.getZonedTime
--   r2 <- efuture action1
--   mid <- Time.getZonedTime
--   v <- status r2
--   print(v)
--   rs <- force r2
--   end <- Time.getZonedTime

--   print $ Time.diffUTCTime (Time.zonedTimeToUTC mid) (Time.zonedTimeToUTC start)
--   print $ Time.diffUTCTime (Time.zonedTimeToUTC end) (Time.zonedTimeToUTC start)

tryAnyIO :: IO a -> IO (Either SomeException a)
tryAnyIO action = withAsync action waitCatch

tryAny :: MonadBaseControl IO m => m a -> m (Either SomeException a)
tryAny action =
    -- MAGIC!
    liftBaseWith (\runInIO -> tryAnyIO (runInIO action)) >>=
    either (return . Left) (liftM Right . restoreM)

catchAny :: MonadBaseControl IO m => m a -> (SomeException -> m a) -> m a
catchAny action onE = tryAny action >>= either onE return

tryAnyDeep :: (MonadBaseControl IO m, NFData a)
           => m a
           -> m (Either SomeException a)
tryAnyDeep action = tryAny $ do
    res <- action
    return $!! res -- here's the magic

catchAnyDeep :: (MonadBaseControl IO m, NFData a)
             => m a
             -> (SomeException -> m a)
             -> m a
catchAnyDeep action onE = tryAnyDeep action >>= either onE return

dangerous :: Monad m => m Int
dangerous = return $ error "Unevaluated!"

someFunc :: IO ()
someFunc = flip runReaderT () $ do
    res <- catchAnyDeep dangerous (const $ return (-1))
    liftIO $ putStrLn "About to print the result"
    liftIO $ putStrLn $ "Result: " ++ show res
    liftIO $ putStrLn "Hmm... does this ever get printed?"