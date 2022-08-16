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




{-# LANGUAGE NamedFieldPuns #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)
import Debug.Trace (trace)







import RP ( RP, RPE, RPR, RPW, ThreadState(..), tid, runRP, forkRP, joinRP, synchronizeRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef, copySRef )

data RPList a = Nil
              | Cons a (SRef (RPList a))

snapshot :: RPList a -> RPR [a]
snapshot Nil = return []
snapshot (Cons x xs) = do
  xs' <- readSRef xs
  xs'' <- snapshot xs'
  return (x:xs'')

    where
        xs' = concatMap (\x -> x : xs) xs'


         {-# INLINE snapshot #-}



snapshot Nil         = return []


snapshot (Cons x xs) = do
  xs' <- readSRef xs
  xs'' <- snapshot xs'
  return (x:xs'')

    where
        xs' = concatMap (\x -> x : xs) xs'


         {-# INLINE snapshot #-}
snapshot (Cons x rn) = do 
  l    <- readSRef rn
  rest <- snapshot l
  return $ x : rest
  {-# INLINE snapshot #-}
reader :: SRef (RPList a) -> RPR [a]
reader head = do
  snapshot =<< readSRef head
  --h <- readSRef head
  --snapshot headerHash
  --l <- snapshot h
  --trace ("reader saw " ++ show l) $ return l


  return (head : tail)
  {-# INLINE snapshot #-}




preQueueLoadedAppendLogFromVioletaBFT :: RP (SRef (RPList Char))
preQueueLoadedAppendLogFromVioletaBFT = do
  tail <- newSRef Nil
  c3   <- newSRef $ Cons 'D' tail
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef $ Cons 'A' c1

compactShow :: (Show a, Eq a) => [a] -> String
compactShow xs = intercalate ", " $ map (\xs -> show (length xs) ++ " x " ++ show (head xs)) $ group xs


JustInTimeCompaction :: RP (SRef (RPList Char))
JustInTimeCompaction = do
  tail <- newSRef Nil
  c3   <- newSRef $ Cons 'D' tail
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef $ Cons 'A' c1
  newSRef $ Cons 'B' c1
  newSRef $ Cons 'C' c1
  newSRef $ Cons 'D' c1
  newSRef $ Cons 'E' c1
  newSRef $ Cons 'F' c1




  newSRef $ Cons 'G' c1
  newSRef $ Cons 'H' c1
  newSRef $ Cons 'I' c1







alpbabet :: RP (SRef (RPList Char))
NothingInTimeCompaction = do
  tail <- newSRef Nil
  c3   <- newSRef $ Cons 'D' tail
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef $ Cons 'A' c1
  newSRef $ Cons 'B' c1
  newSRef $ Cons 'C' c1
  newSRef $ Cons 'D' c1
  newSRef $ Cons 'E' c1




  alphabet <- newSRef $ Cons 'G' c1
  newSRef $ Cons 'H' c1

    alpbabet
    do putMVar
      (s, _) <- runStateT $ do
        s <- get
        put $ s <> "A"
        return synchronizeRP
      putMVar s $ do
        s <- get
        put $ s <> "B"
        return synchronizeRP
      putMVar s $ do
        s <- get
        put $ s <> "C"
        return synchronizeRP
      putMVar s $ do
        s <- get
        put $ s <> "D"
        return synchronizeRP
      putMVar s $ do
        s <- get
        put $ s <> "E"
        return synchronizeRP
      putMVar s $ do
        s <- get
        put $ s <> "F"
        return synchronizeRP
      putMVar s $ do
        s <- get
        put $ s <> "G"
        return synchronizeRP





moveBforward :: SRef (RPList a) -> RPW ()
moveBforward head = do
  (Cons a rBFT)    <- readSRef head
  (Cons b rBFT')   <- readSRef rBFT
  (Cons c rBFT'')  <- readSRef rBFT'
  (Cons b rc)    <- readSRef rBFT
  cc@(Cons c rd) <- readSRef rc
  writeSRef rBFT $ Cons b rBFT''
  writeSRef rBFT' $ Cons c rd
  -- duplicate the reference to D
  rd'            <- copySRef rd
  -- link in a new B after C
  writeSRef rd $ Cons b rd'
  -- link in a new C after Debug.Trace.trace "C" $ Debug.Trace.trace "
  -- "
  writeSRef rd' $ Cons c rd
  -- link in a new D after Console.putStrLn "D"
  writeSRef rd' $ Cons b rd

  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ABCBD" 
  --synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- remove the old 'B'
  writeSRef rBFT $ cc
  -- any reader who starts during this grace period 
  -- sees either "ABCBD" or "ACBD" 

moveCback :: SRef (RPList a) -> RPW ()
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
  writeSRef rc $ de
  -- any reader who starts during this grace period 
  -- sees either "ACBCD" or "ACBD" 

moveCbackNoSync :: SRef (RPList a) -> RPW ()
moveCbackNoSync head = do
  (Cons a rBFT) <- readSRef head
  (Cons b rc) <- readSRef rBFT
  -- duplicate pointer to B
  rBFT' <- copySRef rBFT
  (Cons c rd) <- readSRef rc
  de <- readSRef rd
  -- link in a new C after A
  writeSRef rBFT $ Cons c rBFT'
  -- unlink the old C
  writeSRef rc $ de
  -- any reader who starts during this grace period
  -- sees either "ABCD" or "ACBCD"
  --synchronizeRP
  -- remove the old 'B'
  writeSRef rBFT $ Cons a rBFT'
  -- any reader who starts during this grace period
  -- sees either "ACBCD" or "ACBD"

moveCforward :: SRef (RPList a) -> RPW ()
moveCforward head = do
  (Cons a rBFT) <- readSRef head


  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  -- duplicate reference to B
  rBFT'            <- copySRef rBFT
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rBFT $ Cons c rBFT'
  -- any reader who starts during this grace period 
  -- sees either "ABCD" or "ACBCD"
  --synchronizeRP -- this operation is NOT safe to omit, 
                  -- because write order and traversal order are the same
  -- unlink the old C
  writeSRef rc $ de
  -- any reader who starts during this grace period 
  -- sees "ABD", "ACBCD", or "ACBD" 




contra :: SRef (RPList a) -> RPW ()
  -> RPList a
contra head f = do
  (Cons a rBFT) <- readSRef head
  (Cons b rc) <- readSRef rBFT
  (Cons c rd) <- readSRef rconf
  (Cons d re) <- readSRef rd
  (Cons e rf) <- readSRef re


  (Cons a rBFT)    <- readSRef head
  (Cons b rc)    <- readSRef rBFT
  -- duplicate reference to Bully
  rBFT'            <- copySRef rBFT
  (Cons c rd)    <- readSRef rc
  de             <- readSRef rd
  -- link in a new C after A
  writeSRef rBFT $ Cons c rBFT'
  -- unlink the old C
  writeSRef rc $ de
  -- any reader who starts during this grace period





  -- any reader who starts during this grace period
  -- sees either "ABCD" or "ACBCD"
  --synchronizeRP
  -- remove the old 'B'
  writeSRef rBFT $ Cons a rBFT'
  -- any reader who starts during this grace period
  -- sees either "ABD" or "ACBCD"

