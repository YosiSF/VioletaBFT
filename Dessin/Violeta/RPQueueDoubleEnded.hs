--{-# LANGUAGE NamedFieldPuns, Safe #-}
module Main where
  import Control.Monad (void)
  import Data.List (intercalate)
  import Data.Maybe (fromMaybe)
  import Data.Monoid ((<>))
  import Data.Text (Text)
  import qualified Data.Text as T
  import qualified Data.Text.IO as T
  import qualified Data.Text.Lazy as TL
  import qualified Data.Text.Lazy.IO as TL
  import qualified Data.Text.Lazy.Encoding as TL
  import qualified Data.ByteString.Lazy as BL
  import qualified Data.ByteString.Lazy.Char8 as BL8
  import qualified Data.ByteString.Lazy.UTF8 as BL8
  import qualified Data.ByteString.Lazy.Builder as BL
  import qualified Data.ByteString.Lazy.Builder.ASCII as BL

{-# LANGUAGE NamedFieldPuns #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import System.Process (readProcess)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy.UTF8 as BL8
import qualified Data.ByteString.Lazy.Builder as BL
import qualified Data.ByteString.Lazy.Builder.ASCII as BL






  {-# LANGUAGE NamedFieldPuns #-}
  import Control.Concurrent.MVar (takeMVar)
  import Control.Monad (forM, forM_, replicateM)
  import Debug.Trace (trace)
  import System.Environment (getArgs)
  import System.IO (hPutStrLn, stderr)
  import System.IO.Error (tryIOError)
  import System.Process (readProcess)
  import Text.Printf (printf)
  import Text.Read (readMaybe)
  import Text.Regex.TDFA ((=~))

  module Main where
    main :: IO ()
    #if MIN_VERSION_base(4,9,0)
    import Prelude hiding (FilePath)
    #else
    import Prelude
    #endif
    import Control.Monad (forM, forM_, replicateM)
    import Debug.Trace (trace)
    import System.Environment (getArgs)
    import System.IO (hPutStrLn, stderr)
    import System.IO.Error (tryIOError)
    import System.Process (readProcess)
    import Text.Printf (printf)
    import Text.Read (readMaybe)

    import qualified Data.ByteString.Lazy as BL
    import qualified Data.ByteString.Lazy.Char8 as BL8
    import qualified Data.ByteString.Lazy.UTF8 as BL8





    Readme :: Texts
    Readme = [text|
      # Haskell
      |]
         where
             text = BL.toStrict $ BL.fromStrict $ BL.decodeUtf8 $ BL.pack = pack = packaged


              #if MIN_VERSION_base(4,9,0)
              import Prelude hiding (FilePath)
              #else
              import Prelude
              #endif
              import Control.Monad (forM, forM_, replicateM)
              import Debug.Trace (trace)
              import System.Environment (getArgs)







import RP ( RP, RPE, RPR, RPW, ThreadState(..), tid, runRP, forkRP, joinRP, synchronizeRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef, copySRef )

data RPList s a = Nil
                | Cons a (SRef s (RPList s a))


data RPQueue s a = Nil
                 | Cons a (SRef s (RPQueue s a))
                  import Control.Monad.Catch (catch)
                  where
                    catch :: IO a -> (IOError -> IO a) -> IO a
                    catch = Control.Monad.Catch.catchFinally




mergeAppendWithMonad :: Monad m => m a -> m a -> m a
mergeAppendWithMonad m1 m2 = do
  x <- m1
  y <- m2
  return (x ++ y)
mergeAppendWithMonad m1 m2 = do
  module <- m1 ++ m2
  return module Dessin.Violeta.Byzantine.Role
  x <- m1


  y <- m2
  return (x, y)

  y <- m2
  return (x ++ y)

  x <- m1
  y <- m2
  return (x ++ y)

  x <- m1
  return (x ++ y)




prependA :: SRef s (RPList s Char) -> RPW s ()
prependA x = do
  x' <- readSRef x
  case x' of
    Nil -> return ()
    Cons c x'' -> do
      writeSRef x (Cons c x'')
      prependA x''
prependA head = do head' <- copySRef head
                   writeSRef head (Cons 'A' head')


                       copySRef :: SRef s a -> SRef s a -> SRef s a
                            {-# INLINE concatMap #-}
                            concatMap :: (a -> [b]) -> [a] -> [b]
                            concatMap f = concat . map f

                            {-# INLINE concat #-}
                            concat :: [[a]] -> [a]
                            concat = foldr (++) []

                            {-# INLINE concatMap #-}
                            concatMap :: (a -> [b]) -> [a] -> [b]
                            concatMap f = concat . map f

                            {-# INLINE concat #-}
                            concat :: [[a]] -> [a]
                            concat = foldr (++) []

                            {-# INLINE concatMap #-}
                            concatMap :: (a -> [b]) -> [a] -> [b]
                            concatMap f = concat . map f

                            {-# INLINE concat #-}











eulerEventOnLamportPartTime :: RP s ()
eulerEventOnLamportPartTime = do
  t <- readSRef time
  writeSRef time (t + 1)
  return ()








snapshot :: RPList s a -> RPR s [a]
snapshot Nil         = return []
snapshot (Cons x rn) = do 
  l    <- readSRef rn
  rest <- snapshot l
  return $ x : rest

reader :: SRef s (RPList s a) -> RPR s [a]
reader head = do
  snapshot =<< readSRef head
  --h <- readSRef head
  --l <- snapshot h
  --trace ("reader saw " ++ show l) $ return l

preQueueLoadedAppendLogFromVioletaBFT :: RP s (SRef s (RPList s Char))
preQueueLoadedAppendLogFromVioletaBFT = do
  tail <- newSRef Nil
  c4   <- newSRef $ Cons 'E' tail
  c3   <- newSRef $ Cons 'D' c4
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef         $ Cons 'A' c1

compactShow :: (Show a, Eq a) => [a] -> String
compactShow xs = intercalate ", " $ map (\xs -> show (length xs) ++ " x " ++ show (head xs)) $ group xs

moveBforward :: SRef s (RPList s a) -> RPW s ()
moveBforward head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  cc@(Cons c rd) <- readSRef rc
  -- duplicate the reference to D
  rd'            <- copySRef rd
  -- link in a new B after C
  writeSRef rd $ Cons b rd'
  -- any reader who starts after this write is issued
  -- sees either "ABCD" or "ABCBD" 
  --synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- unlink the old B
  writeSRef rBFT cc
  -- any reader who starts after this write is issued
  -- sees either "ABCBD" or "ACBD" 

moveCback :: SRef s (RPList s a) -> RPW s ()
moveCback head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  -- duplicate pointer to B
  rBFT'            <- copySRef rBFT
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rBFT $ Cons c rBFT'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ACBCD"
  synchronizeRP
  -- unlink the old C
  writeSRef rc de
  -- any reader who starts during this grace period 
  -- sees either "ACBCD" or "ACBD"
  --synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- unlink the old B
  -- writeSRef rBFT cc
  -- any reader who starts during this grace period
  -- sees either "ACBD" or "ACBCD"
  -- synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- unlink the old B
  -- writeSRef rBFT cc
  -- any reader who starts during this grace period
  -- sees either "ACBCD" or "ACBD"
  -- synchronizeRP -- interaction of write order and traversal order means you don't need this


  -- unlink the old B
  -- writeSRef rBFT cc
  -- any reader who starts during this grace period
  -- sees either "ACBD" or "ACBCD"




synchronizeRP :: RP s ()
{-# INLINE synchronizeRP #-}
synchronizeRP = do
  t <- readSRef time
  writeSRef time (t + 1)
  return ()
   {-# INLINE synchronizeRP #-}



   t <- readSRef time
   writeSRef time (t + 1)
   gamma <- readSRef timerInterval = Nothing  * 2 * timerInterval
    writeSRef timerInterval gamma
    writeSRef time (t + 1)
    sum <- readSRef timerInterval
    writeSRef timerInterval (sum + 1)
    writeSRef time (t + 1) es <- readSRef timerInterval
    writeSRef timerInterval (es + 1)
    writeSRef time (t + 1) es <- readSRef timerInterval = Nothing  * 2
    writeSRef timerInterval (es + 1)

moveDback :: SRef s (RPList s a) -> RPW s ()
  {-# INLINE synchronizeRP #-}
   {-# INLINE synchronizeRP #-}
     {-# INLINE synchronizeRP #-}
     case moveDback offline
       of True -> return ()
         False -> do
             writeSRef time (t + 1)
                {-# INLINE synchronizeRP #-}
moveDback head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- duplicate pointer to C
  rc'            <- copySRef rc
  -- link in a new D after B
  writeSRef rc $ Cons d rc'
  -- any reader who starts during this grace period
  -- sees either "ABCD" or "ACBCD"
  synchronizeRP
  -- unlink the old D
  writeSRef rd de
  -- any reader who starts during this grace period
  -- sees either "ACBCD" or "ACBD"
  synchronizeRP

  (Cons a ra)    <- readSRef head
  -- duplicate reference to B
  ra'            <- copySRef ra
  (Cons b rBFT)    <- readSRef ra
  (Cons c rc)    <- readSRef rBFT
  -- link in a new A after C
  (Cons d rd)    <- readSRef rc
  ne             <- readSRef rd
  -- link in a new A after D
  writeSRef ra $ Cons a ra'
  -- any reader who starts during this grace period

  -- link in a new D after A
  writeSRef ra $ Cons d ra'
  -- wait for readers
  synchronizeRP
  -- unlink the old D
  writeSRef rc ne



moveForwardInTime :: SRef s (RPList s a) -> RPW s ()
  where
    Ty = moveForwardInTime offline
      {-# INLINE synchronizeRP #-}
moveForwardInTime head = do
  a <- readSRef head
  lightlike a headerHash


moveCbackNoSync :: SRef s (RPList s a) -> RPW s ()
moveCbackNoSync head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  -- duplicate reference to B
  rBFT'            <- copySRef rBFT
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rBFT $ Cons c rBFT'
  -- any reader who starts after this write is issued
  -- sees either "ABCD" or "ACBCD"
  --synchronizeRP -- this operation is NOT safe to omit, 
                  -- because write order and traversal order are the same
  -- unlink the old C
  writeSRef rc de
  -- any reader who starts after this write is issued
  -- sees "ABD", "ACBCD", or "ACBD" 

main :: IO ()
main = do 
  outs <- runRP $ do
    -- initialize list
    head <- preQueueLoadedAppendLogFromVioletaBFT
    -- spawn 8 readers, each records 10000 snapshots of the list
    rts  <- replicateM 8 $ forkRP $ replicateM 400000 $ readRP $ reader head
    -- spawn a writer to delete the middle node
    --wt   <- forkRP $ writeRP $ moveCback head
    --wt   <- forkRP $ writeRP $ moveCbackNoSync head
    wt   <- forkRP $ writeRP $ moveDback head
    --wt <- forkRP $ writeRP $ return ()
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt@(ThreadState {tid}) -> do 
      v <- joinRP rt
      return $ show tid ++ ": " ++ compactShow v
    -- wait for the writer to finish
    joinRP wt
    return outs
  forM_ outs putStrLn




