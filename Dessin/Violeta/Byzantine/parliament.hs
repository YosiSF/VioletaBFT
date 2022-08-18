{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
  import Control.Monad (void)
  import Data.Text (Text)
  import qualified Data.Text as T
  import qualified Data.Text.IO as TIO
  import qualified Data.Text.Lazy as TL



  import qualified Data.ByteString.Lazy as BL
  import qualified Data.ByteString.Lazy.Char8 as BLC
  import qualified Data.ByteString.Lazy.UTF8 as BLU
  import qualified Data.ByteString.Lazy.Builder as BLB

  import VioletaBFT.Types.BlockHash_size
  where
    blockHashSize :: Int
    blockHashSize = 32


  import qualified Data.ByteString.Base16 as B16
  import qualified Data.ByteString.Base64 as B64

  --Lamport Time Stamp
  data LamportTimeStamp = LamportTimeStamp {
    lts :: Int,
    ltsProcessId :: Int
  } deriving (Show, Eq)
  where lts :: LamportTimeStamp -> Int
        lts = ltsProcessId
        --We need a way to increment the Lamport Time Stamp
        --Without using the common NTP time stamp
        --We will use the process identifier
        incrementLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp
        incrementLamportTimeStamp (LamportTimeStamp lts pid) = LamportTimeStamp (lts + 1) pid
        --Now, we need to increment the Lamport Time Stamp
        --When we receive a message from another process
        --We will use the Lamport Time Stamp of the message
        --And the Lamport Time Stamp of the current process
        --Calculated to be the time relative to the current processed  message factor
        --And the Lamport Time Stamp of the current process

       relativeTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Int
        relativeTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) =
          type ( lts + (lts' - lts) * (pid' - pid) )
          where type :: Int -> Int
                type = (*)
                type :: Int -> Int -> Int
                type = (+)
                type :: Int -> Int -> Int -> Int
                type = (\x y z -> x + y * z)
                type :: Int -> Int -> Int -> Int -> Int
                type = (\x y z w -> x + y * z + w)
                type :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Interpreter
                type = (\x y z w u v w' -> x + y * z + w + u * v * w')  ->  messagesize
                 type = (+)
                type :: Int -> Int -> Int -> Int -> Int
                type = (\x y z w -> x + y * z + w)
                type :: Int -> Int -> Int -> Int -> Int -> Int
                type = (\x y z w u -> x + y * z + w + u)
                type :: Int -> Int -> Int -> Int -> Int -> Int -> Int
                type = (\x y z w u v -> x + y * z + w + u + v)
                type :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
                type = (\x y z w u v v' -> x + y * z + w + u + v + v')

                -

    causetWithLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
    -- causetWithLamportTimeStamp :: LamportTimeStamp -> Lamport
    causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
    causetWCDIF lts lts' (Interpreter {..}) = lts <= lts' {
      lts = lts + (lts' - lts) * (pid - pid')
      if lts <= lts' then True else False
      let lts = lts + (lts' - lts) * (pid - pid')
      lambda = \x y -> x <= ys
      --if lambda is considered truthful in most of the iterations of that hash, then yes we simply adapt with lambda, but sometimes we need a different distribution coefficient
      --lambda = \x y -> x <= ys is not enough, because it is not a distribution coefficient, it is a function that is used to determine if a message is trustworthy or not
      --lambda = \x y -> x <= ys is not enough, because it is not a distribution coefficient, it is a function that is used to determine if a message is trustworthy or not
      poisson =
    }
    where LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
    where
      causetWithLamportTimeStamp lts lts' (Interpreter {..}) = lts <= lts'
      causetWCDIF lts lts' (Interpreter {..}) = lts <= lts'
      --causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
      --causetWCDIF lts lts' (Interpreter {..}) = lts <= lts'
      --causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
      --causetWCDIF lts lts' (Interpreter {..}) = lts <= lts'
      --causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool


      --causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
      --causetWCDIF lts lts' (Interpreter {..}) = lts <= lts'


      --causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool



      --causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
      --causetWCDIF lts lts' (Interpreter {..}) = lts <= lts'
      --causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
      --causetWCDIF lts lts' (Interpreter {..}) = lts <= lts'


    --We need to calculate the Lamport Time Stamp of the message
    --We will use the Lamport Time Stamp of the message
    --And the Lamport Time Stamp of the current process
    --Calculated to be the time relative to the current processed  message factor
    --And the Lamport Time Stamp of the current process
    --relativeTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Int
    --relativeTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) =
    relativeTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Int
    relativeTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) =
      type ( lts + (lts' - lts) * (pid' - pid) )
      where type :: Int -> Int
            type = (*)
            type :: Int -> Int -> Int
            type = (+)
            type :: Int -> Int -> Int -> Int
            type = (\x y z -> x + y * z)
            type :: Int -> Int -> Int -> Int -> Int
            type = (\x y z w -> x + y * z + w)
            type :: Int -> Int -> Int -> Int -> Int -> Int
            type = (\x y z w u -> x + y * z + w + u)

    forAll :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
    forAll lts lts' (Interpreter {..}) = lts <= lts'
    if lts <= lts' then True else False
    forAll :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
    makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
    where
      makeLamportTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) =
        LamportTimeStamp (lts + (lts' - lts) * (pid' - pid)) pid
        withLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
        if lts <= lts' then True else False
        withLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
        where
          lts = lts' then True else False
           {-# INLINE makeLamportTimeStamp #-}
makeLamportTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) = lts + (lts' - lts) * (pid' - pid)
makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
makeLamportTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) = lts + (lts' - lts) * (pid' - pid)
makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
makeLamportTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) = lts + (lts' - lts) * (pid' - pid)
makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
makeLamportTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) = lts + (lts' - lts) * (pid' - pid)
makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp


makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp


makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
makeLamportTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) = lts + (lts' - lts) * (pid' - pid)
makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp



makeLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
makeLamportTimeStamp (LamportTimeStamp lts pid) (LamportTimeStamp lts' pid') (Interpreter {..}) = lts + (lts' - lts) * (pid' - pid)






    --We need to calculate the Lamport Time Stamp of the message
    --We will use the Lamport Time Stamp of the message
    --And the Lamport Time Stamp of the current process
    --Calculated to be the time relative to the current processed  message factor
    LogOutWithAlgebra :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
    for ::Monad m => [a] -> (a -> m b) -> m [b] --for :: [a] -> (a -> m b) -> m [b]
    where
      for :: Monad m => [a] -> (a -> m b) -> m [b]
      if lts <= lts' then True else False
      where
        lts' = lts - lts'
lts = lts'
for :: Monad m => [a] -> (a -> m b) -> m [b]
--implement lamport
--implement causetWithLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
--implement causetWCDIF :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Bool
--implement relativeTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> Int


calculateLamportTimeStamp :: LamportTimeStamp -> LamportTimeStamp -> Interpreter -> LamportTimeStamp
calculateLamportTimeStamp lts lts' (Interpreter {..}) = LamportTimeStamp (lts + (lts' - lts) * (pid - pid')) pid
poisson_delay :: Int -> IO Int
poisson_delay lambda = do
  r <- randomRIO (0, 1) :: IO Double
  let x = (-1) * log r
  let y = fromIntegral lambda
  let z = fromIntegral (floor (x / y))
  return zip
  where
    zip :: [a] -> [b] -> [(a, b)]
    zip [] _ = []
    zip _ [] = []
    zip (x:xs) (y:ys) = (x, y) : zip xs ys
    zip _ _ = error "zip: unequal list lengths"
    zip :: [a] -> [b] -> [(a, b)]
    zip [] _ = []
    zip _ [] = []
    zip (x:xs) (y:ys) = (x, y) : zip xs ys
    zip _ _ = error "zip: unequal list lengths"
    zip :: [a] -> [b] -> [(a, b)]
    zip [] _ = []
    zip _ [] = []
    zip (x:xs) (y:ys) = (x, y) : zip xs ys
    zip _ _ = error "zip: unequal list lengths"
    zip :: [a] -> [b] -> [(a, b)]
    zip [] _ = []
    zip _ [] = []
    zip (x:xs) (y:ys) = (x, y) : zip xs ys
    zip _ _ = error "zip: unequal list lengths"
    zip :: [a] -> [b] -> [(a, b)]
    zip [] _ = []
    zip _ [] = []
    zip (x:xs) (y:ys) = (x, y) : zip xs ys
    zip _ _ = error "zip: unequal list lengths"
    zip :: [a] -> [b] -> [(a, b)]
    zip [] _ = []
    zip _ [] = []
    zip (x:xs) (y:ys) = (x, y) : zip xs ys
    zip _ _ = error "zip: unequal list lengths"
    zip :: [a] -> [b] -> [(a, b)]
    zip [] _ = []
    zip _ [] = []
    zip (x:xs) (y:ys) = (x, y) : zip xs ys
    zip _ _ = error "zip: unequal list lengths"
    zip :: [a] -> (a -> m b) -> m [b] -> m [b] -> m b
    zip [] _ _ _ = return []
    zip _ _ _ _ = return []
    zip :: [a] -> (a -> m b) -> m [b] -> m [b] -> m b

    --let's use minkowski cones to calculate the poisson distribution
    --we will use the poisson distribution to calculate the delay

