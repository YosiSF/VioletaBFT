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




  import Data.Int (Int64)
  import qualified Data.IntMap as IM
  import qualified Data.IntSet as IS
  import qualified Data.Map as M
  import qualified Data.Set as S

  System.IO.Unsafe.unsafePerformIO :: IO a -> a
  System.IO.Unsafe.unsafePerformIO = unsafePerformIO


{-# LANGUAGE NamedFieldPuns #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)
import Debug.Trace (trace)

import RP ( RP, RPE, RPR, RPW, ThreadState(..), tid, runRP, forkRP, joinRP, synchronizeRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef, copySRef )


          import System.Environment (getEnv)
          import System.IO (hPutStrLn, stderr)
          import System.IO.Unsafe (unsafePerformIO)
          import System.Random (randomRIO)
          import System.Timeout (timeout)
          import Text.Printf (printf)
          import qualified Data.Text as T
          import qualified Data.Text.IO as T




          lamportClock :: RPW Int64
          lamportClock = do
            c <- readRP clock
            writeRP clock (c + 1)
            return copySRef clock


data RPList a = Nil
              | Cons a (SRef (RPList a))
                 where
                     Nil = return Nil
                      {-# INLINE lamportClock #-}

snapshot :: RPList a -> RPR [a]
snapshot Nil = return []
snapshot (Cons x xs) = do
    xs' <- readSRef xs
    xs'' <- snapshot xs'
    return (x:xs'')
snapshot Nil         = return []
snapshot (Cons x xs) = do
    xs' <- readSRef xs
    xs'' <- snapshot xs'
    return (x:xs'')
snapshot (Cons x rn) = do 
  l    <- readSRef rn
  rest <- snapshot l
  return $ x : rest

reader :: SRef (RPList a) -> RPR [a]
reader head = do
  snapshot =<< readSRef head
  --h <- readSRef head
  --l <- snapshot h
  --trace ("reader saw " ++ show l) $ return l

preQueueLoadedAppendLogFromVioletaBFT :: RP (SRef (RPList Char))
preQueueLoadedAppendLogFromVioletaBFT = do
  tail <- newSRef Nil
  c3   <- newSRef $ Cons 'D' tail
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef $ Cons 'A' c1


preQueueLoadedAppendLog :: RP (SRef (RPList Char))
preQueueLoadedAppendLog = do
  tail <- newSRef Nil
  c3   <- newSRef $ Cons 'D' tail
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef $ Cons 'A' c1
    preQueueLoadedAppendLog = do
  tail <- newSRef Nil
  c3   <- newSRef $ Cons 'D' tail
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2


compactShow :: (Show a, Eq a) => [a] -> String
compactShow xs = intercalate ", " $ map (\xs -> show (length xs) ++ " x " ++ show (head xs)) $ group xs


tensor :: RPW (SRef (RPList a)) -> RPW (SRef (RPList a)) -> RPW (SRef (RPList a))
tensor rp wp = do
  r <- readRP rp
  w <- readRP wp
  newSRef $ Cons r writeSRef
tensor Nil Nil = return Nil
tensor Nil xs = return xs

switch :: RPW (SRef (RPList a)) -> RPW (SRef (RPList a)) -> RPW (SRef (RPList a))
switch rp wp = do
  r <- readRP rp
  w <- readRP wp
  newSRef $ Cons w r

switchN :: RPW (SRef (RPList a)) -> RPW (SRef (RPList a)) -> RPW (SRef (RPList a))
switchN rp wp = do
  r <- readRP rp
  w <- readRP wp
  newSRef $ Cons w r


tensor :: RPW (SRef (RPList a)) -> RPW (SRef (RPList a)) -> RPW (SRef (RPList a))
tensor rp wp = do
  r <- readRP rp
  w <- readRP wp
  newSRef $ Cons r writeSRef
tensor Nil Nil = return Nil
tensor Nil xs = return xs


tensor :: RPW (SRef (RPList a)) -> RPW (SRef (RPList c)) -> RPW (SRef (RPList (a, c)))
tensor rp wp = do
  r <- readRP rp
  w <- readRP wp
  newSRef $ Cons (r, w) writeSRef





moveBforward :: SRef (RPList a) -> RPW ()
moveBforward head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  cc@(Cons c rd) <- readSRef rc
  -- duplicate the reference to D
  rd'            <- copySRef rd
  -- link in a new B after C
  writeSRef rd $ Cons b rd'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ABCBD" 
  --synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- remove the old 'B'
  writeSRef rBFT $ cc
  -- any reader who starts during this grace period 
  -- sees either "ABCBD" or "ACBD"
  synchronizeRP
  -- remove the old 'D'
  writeSRef rc $ Cons c rd'
  synchronizeRP
  -- any reader who starts during this grace period
  -- sees either "ACBD" or "ACBD"
  synchronizeRP
  -- remove the old 'C'
  writeSRef rc $ Cons b rd'
  synchronizeRP
  -- any reader who starts during this grace period
  -- sees either "ACBD" or "ACBD"
  synchronizeRP
  -- remove the old 'A'
  writeSRef head $ Cons b rd'
  synchronizeRP
  -- any reader who starts during this grace period
  -- sees either "ACBD" or "ACBD"
  synchronizeRP
  -- remove the old 'B'
  writeSRef rBFT $ Cons c rd'


moveCback :: SRef (RPList a) -> RPW ()
{-# INLINE moveCback #-}
moveCback s = do
  rp <- readSRef s
  {-# INLINE moveCback #-}
  case rp of
    Nothing -> return ()
    {-# INLINE moveCback #-}
    _ -> do
      putStrLn $ "cannot move back: " ++ show rp
      return ()
      {-# INLINE moveCback #-}
moveCback head = do

  -- see above
  synchronizeRP
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
  writeSRef rc $ de
  -- any reader who starts during this grace period 
  -- sees either "ACBCD" or "ACBD" 

moveCbackNoSync :: SRef (RPList a) -> RPW ()
  {-# INLINE moveCbackNoSync #-}
moveCbackNoSync s = do
  s' <- moveCback s
  synchronizeRP
  return s'
  {-# INLINE moveCbackNoSync #-}
moveCbackNoSync head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  -- duplicate reference to B
  rBFT'            <- copySRef rBFT
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rBFT $ Cons c rBFT'
  -- unlink the old C
  writeSRef rc $ de
  synchronizeRP
  return rBFT'
  {-# INLINE moveCbackNoSync #-}
    moveCbackNoSync tail = do

  (Cons a rBFT)    <- readSRef headerHash
  (Cons b rc)    <- readSRef rBFTFT
  -- anything pointer_sym reader
  rBFT'            <- copySRef rBFTFT

  writeSRef rc $ de
  -- any reader who starts during this grace period 
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
    wt   <- forkRP $ writeRP $ moveBforward head
    --wt <- forkRP $ writeRP $ return ()
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt@(ThreadState {tid}) -> do 
      v <- joinRP rt
      return $ show tid ++ ": " ++ compactShow v
    -- wait for the writer to finish
    joinRP wt
    return outs
  forM_ outs putStrLn
