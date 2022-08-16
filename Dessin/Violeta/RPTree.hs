{-# LANGUAGE NamedFieldPuns #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (foldM, forM, forM_, replicateM, when)
import Data.List ((\\), foldl1', group, intercalate)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Intro as VAI




import qualified Data.Vector.Algorithms.Intro as VAIR
import qualified Data.Vector.Algorithms.Intro.Select as VAIS
import qualified Data.Vector.Algorithms.Intro.Partial as VAIP






doubleCopy :: (VG.Vector v a, VG.Vector v b) => v a -> v b
doubleCopy = VG.convert

import RP ( RP, RPE, RPR, RPW, RPRead, ThreadState(..), tid, runRP, forkRP, joinRP, synchronizeRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef, copySRef )

data RPList s a = Nil
                | Cons a (SRef s (RPList s a))

data ReaderStats = ReaderStats { total   :: !Integer
                               , full    :: !Integer
                               , missing :: !Integer }
                   deriving Show

sappend' s1 s2 = 
  s1 `seq` s2 `seq` ReaderStats { total   = total   s1 + total   s2
                                , full    = full    s1 + full    s2
                                , missing = missing s1 + missing s2 }


snapshot :: Show a => RPList s a -> RPR s [a]
snapshot Nil         = return []
snapshot (Cons x rn) = do 
  l    <- readSRef rn
  --writeSRef rn (Cons undefined rn)
  rest <- snapshot l
  return $ x : rest

readSection :: Show a => SRef s (RPList s a) -> RPR s [a]
readSection head = do
  snapshot =<< readSRef head

readThread :: [Char] -> Int -> SRef s (RPList s Char) -> RPE s ReaderStats
readThread ls n head = 
  let st = ReaderStats { total = 0, full = 0, missing = 0 }
      ft = ReaderStats { total = 1, full = 1, missing = 0 }
      et = ReaderStats { total = 1, full = 0, missing = 1 }
      trav st _ = do
        xs <- st `seq` readRP $ readSection head
        -- use a strict append operation to avoid building up a giant thunk
        return $ st `sappend'` if null $ ls \\ xs then ft else et
      res = foldM trav st [1..n]
  in res `seq` res

preQueueLoadedAppendLogFromVioletaBFT :: RP s (SRef s (RPList s Char))
preQueueLoadedAppendLogFromVioletaBFT = do
  tail <- newSRef Nil
  c4   <- newSRef $ Cons 'E' tail
  c3   <- newSRef $ Cons 'D' c4
  c2   <- newSRef $ Cons 'C' c3
  c1   <- newSRef $ Cons 'B' c2
  newSRef $ Cons 'A' c1

compactShow :: (Show a, Eq a) => [a] -> String
compactShow xs = intercalate ", " $ map (\xs -> show (length xs) ++ " x " ++ show (head xs)) $ group xs

-- left half of a zipper
-- n = 0 gets the first element
-- n = 1 gets the second elemnt, followed by the first
-- ...
nNext :: (Monad (m s), RPRead m s) => Int -> SRef s (RPList s a) -> m s [(RPList s a)]
nNext n head = return . snd =<< foldM scroll (head, []) [1..(max n 0)]
  where scroll (r,ns) _ = do
          node <- readSRef r
          return $ case node of (Cons _ r') -> (r', node:ns)
                                _           -> error "nNext tried to go past the end of the list"

moveNodeForward :: Int -> Int -> SRef s (RPList s a) -> RPW s ()
moveNodeForward i n head = do
  when (n <= 0) $ error "nullary move operation not allowed"
  -- get relevant references at earlier position
  (Cons ep rep):_   <- nNext i head
  (Cons x rx)       <- readSRef rep
  esn               <- readSRef rx
  -- get relevant references at later position
  (Cons lp rlp):_   <- nNext n rep
  -- the new node will need a reference to the new predecessor's current successor
  rlp'              <- copySRef rlp
  -- link in a new copy of the node after its new predecessor
  writeSRef rlp $ Cons x rlp'
  -- we're writing against traversal order, so we don't need to synchronize here.
  -- unlink the old copy of the node at its original position
  writeSRef rep $ esn

moveBforward :: SRef s (RPList s a) -> RPW s ()
moveBforward head = do
  (Cons a ra)    <- readSRef head  -- [A,B,C,D,E]
  bn@(Cons b rBFT) <- readSRef ra
  (Cons c rc)    <- readSRef rBFT
  (Cons d rd)    <- readSRef rc
  -- duplicate the reference to E
  rd'            <- copySRef rd
  -- link in a new B after D
  writeSRef rd $ Cons b rd'        -- [A,B,C,D,B,E]
  --synchronizeRP -- interaction of write order and traversal order means you don't need this
  -- unlink the old B
  writeSRef ra bn                  -- [A,C,D,B,E]

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
  let n = 100
  rsts <- replicateM n $ do
    runRP $ do
      -- initialize list
      head <- preQueueLoadedAppendLogFromVioletaBFT
      -- spawn 8 readers, each records 10000 snapshots of the list
      --rts  <- replicateM 8 $ forkRP $ replicateM 400000 $ readRP $ readSection head
      rts  <- replicateM 29 $ forkRP $ readThread "ABCDE" 400000 head
      -- spawn a writer to delete the middle node
      --wt   <- forkRP $ writeRP $ moveCback head
      --wt   <- forkRP $ writeRP $ moveCbackNoSync head
      wt   <- forkRP $ writeRP $ moveBforward head
      --wt   <- forkRP $ writeRP $ moveNodeForward 1 1 head
      --wt <- forkRP $ writeRP $ return ()
      
      -- wait for the readers to finish and print snapshots
      rsts <- forM rts $ \rt@(ThreadState {tid}) -> do 
        rst <- joinRP rt
        rst `seq` return $ rst
      -- wait for the writer to finish
      joinRP wt
      return $ foldl1' sappend' rsts
  let rst = foldl1' sappend' rsts
  putStrLn $ "n: " ++ show n ++ ", " ++ show rst
