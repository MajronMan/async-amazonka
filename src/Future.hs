module Future where

import           Control.Concurrent      (ThreadId, forkIO, threadDelay)
import           Control.Monad.Catch
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           UnliftIO.STM


data FutureStatus = Running | Succeeded | Failed deriving (Show)

type Promise a = Either String a
type Future a = TMVar (Promise a)


handleError :: (MonadUnliftIO m, MonadIO m) => SomeException -> m (Promise a)
handleError e = return $ Left $ show e

performAction :: (MonadUnliftIO m, MonadIO m, MonadCatch m) => m a -> Future a -> m()
performAction act fut = do
  val <- catchAll (act >>= return . Right) handleError
  atomically $ putTMVar fut val

future :: (MonadUnliftIO m, MonadIO m, MonadCatch m) => m a -> m (Future a)
future act = do
  fut <- newEmptyTMVarIO
  withRunInIO $ \runInIO ->
    forkIO . runInIO $ performAction act fut
  return fut

applyCallbacks :: (MonadUnliftIO m, MonadIO m) => (a -> m b) -> (String -> m b) -> Promise a -> m b
applyCallbacks _ onFailure (Left s)  = onFailure s
applyCallbacks onSuccess _ (Right v) = onSuccess v


await :: MonadIO m => Future a -> m (Promise a)
await fut = atomically $ do
  v <- takeTMVar fut
  putTMVar fut v
  return v

tryTake :: MonadIO m => Future a -> m (Maybe (Promise a))
tryTake = atomically . tryReadTMVar

determineStatus :: Promise a -> FutureStatus
determineStatus (Left _)  = Failed
determineStatus (Right _) = Succeeded

status :: MonadIO m => Future a -> m FutureStatus
status fut = do
  empty <- atomically $ isEmptyTMVar fut
  if empty
    then return Running
    else await fut >>= return . determineStatus


breakOrPerform :: (MonadUnliftIO m, MonadIO m, MonadCatch m) => (a -> m b) -> Future b -> Promise a -> m()
breakOrPerform _ next (Left l)    = atomically $ putTMVar next (Left l)
breakOrPerform act next (Right v) = performAction (act v) next

asyncPerform ::(MonadUnliftIO m, MonadIO m, MonadCatch m) => Future a -> Future b -> (a -> m b) -> (m() -> IO()) -> IO(ThreadId)
asyncPerform fut next act runInIO = forkIO . runInIO $ await fut >>= breakOrPerform act next

flatMap :: (MonadUnliftIO m, MonadIO m, MonadCatch m) => (a -> m b) -> Future a -> m (Future b)
flatMap act fut = do
  next <- newEmptyTMVarIO
  withRunInIO $ asyncPerform fut next act
  return next

flatMapMany :: (MonadUnliftIO m, MonadIO m, MonadCatch m) => [a -> m a] -> Future a -> m (Future a)
flatMapMany [] fut      = return fut
flatMapMany (act:t) fut = flatMap act fut >>= flatMapMany t

-- TEST

-- a <- future $ fmap (read :: String -> Int) getLine
-- acts = map (return .) [(20 `subtract`), (*5), (+3)]
-- b <- flatMapMany acts a
-- status b
-- await b

-- badOp = getLine >>= error
-- c <- future badOp
-- Left x <- await c
-- putStrLn x

-- d <- future $ threadDelay 5000000 >> return 18
-- status d
-- status d
