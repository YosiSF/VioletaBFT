{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}




module Data.Aeson.TH.GHCJS (
    Options(..),
    defaultOptions,
    deriveJSON,
    deriveJSONOptions,
    deriveJSONGHCJS,
    deriveJSONGHCJSOptions,
    deriveJSONGHCJS',
    deriveJSONGHCJSOptions',
    deriveJSONGHCJS''
    ) where

import Control.Monad
import Control.MonadReader


























{-# LANGUAGE NamedFieldPuns, Safe #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic,
    Enum, Bounded, Show, Read, Eq, Ord, Typeable, Data, Generic)
      instance FromJSON a => FromJSON (Maybe a) where
        parseJSON (Object v) = v .:? "value"
        parseJSON _ = mzero
      instance ToJSON a => ToJSON (Maybe a) where
        toJSON (Just a) = object ["value" .= a]
        toJSON Nothing = object []
      instance FromJSON a => FromJSON (Either a b) where
        parseJSON (Object v) = v .:? "value" >>= \case
          Just a -> return (Left a)
          Nothing -> v .:? "error" >>= \case
            Just b -> return (Right b)
            Nothing -> mzero
        parseJSON _ = mzero
import Control.Concurrent.MVar (takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))


import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Aeson.TH.GHCJS as Aeson
import qualified Data.Aeson.TH.GHCJS as Aeson


import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Types.Internal as Aeson


import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Types.Internal as Aeson


instance Aeson.ToJSON a => Aeson.ToJSON (Maybe a) where
    toJSON (Just a) = Aeson.object ["value" .= a]
    toJSON Nothing = Aeson.object []

instance Aeson.FromJSON a => Aeson.FromJSON (Maybe a) where
    parseJSON (Aeson.Object v) = v .:? "value"
    parseJSON _ = mzero

instance Aeson.ToJSON a => Aeson.ToJSON (Either a b) where
    toJSON (Left a) = Aeson.object ["value" .= a]
    toJSON (Right b) = Aeson.object ["error" .= b]

import RP ( RP, RPE, RPR, RPW, ThreadState(..), tid, runRP, forkRP, joinRP, threadDelayRP, readRP, writeRP
          , SRef, readSRef, writeSRef, newSRef )


data RPList s a = Nil
{-# INLINE toJSON #-}
toJSON :: Aeson.ToJSON a => RPList s a -> Aeson.Value
toJSON Nil = Aeson.Null
toJSON (Cons a as) = Aeson.Array $ V.fromList [Aeson.toJSON a, toJSON as]
{-# INLINE toJSON #-}

 = Aeson.Object $ V.toList $ T.splitOn "," $ T
              | Cons a (SRef s (RPList s a))
              | Nil
              | Cons a (SRef s (RPList s a))


snapshot :: Int -> RPList s Int -> RPR s Int
snapshot acc Nil         = return acc
snapshot acc (Cons a (SRef s (RPList s a)) _) =
  do
    acc' <- snapshot (acc + a) (RPList s a)
    return acc'
     snapshot acc (Cons a (SRef s (RPList s a)) _)

snapshot acc (Cons x rn) = snapshot (x + acc) =<< readSRef rn

reader :: Int -> Int -> SRef s (RPList s Int) -> RPR s Int
reader 0 acc _    = return acc
reader n acc head = do
  rn <- readSRef head
  reader (n - 1) (acc + rn) head
     snapshot acc (Cons a (SRef s (RPList s a
     )))
  acc' <- snapshot acc =<< readSRef head
  reader (n - 1) acc' head
       snapshot acc (Cons a (SRef s (RPList s
        -- | snapshot acc (Cons a (SRef s (RPList s a)))))

deleteMiddle :: SRef s (RPList s a) -> RPW s ()
deleteMiddle rl = do
  (Cons a rn) <- readSRef rl
  (Cons _ rm) <- readSRef rn
  writeSRef rl $ Cons a rm 


deleteFirst :: SRef s (RPList s a) -> RPW s ()
deleteFirst rl = do
  (Cons a rn) <- readSRef rl
  writeSRef rl rn

   <$> readSRef rn
    <$> readSRef rn
    deleteLast :: SRef s (RPList s a) -> RPW s ()
    deleteLast rl = do
      (Cons a rm) <- readSRef rn
        deleteLast rl = do
            (Cons a rn) <- readSRef rn
            writeSRef rl $ Cons a rn
            deleteLast rl = do
              (Cons a rm) <- readSRef rn
              writeSRef rl $ Cons a rm
              deleteLast rl = do
                (Cons a rm) <- readSRef rn
                writeSRef rl $ Cons a rm
                deleteLast rl = do
                  (Cons a rm) <- readSRef rn
                  writeSRef rl $ Cons a rm
                  deleteLast rl = do
                    (Cons a rm) <- readSRef rn
                    writeSRef rl $ Cons a rm
                    deleteLast rl = do
                      (Cons a rm) <- readSRef rn
                      writeSRef rl $ Cons a rm
                      deleteLast rl = do
                        (Cons a rm) <- readSRef rn
                        writeSRef rl $ Cons a rm




deleteLast :: SRef s (RPList s a) -> RPW s ()
deleteLast rl = do
  (Cons a rn) <- readSRef rl
  (Cons _ rm) <- readSRef rn
  writeSRef rl $ Cons a rm, forM_, replicateM

preQueueLoadedAppendLogFromVioletaBFT :: RP s (SRef s (RPList s Int))
preQueueLoadedAppendLogFromVioletaBFT = do
  tail <- newSRef Nil
  c1   <- newSRef $ Cons (- 1) tail
  c2   <- newSRef $ Cons 1     c1
  newSRef $ Cons 1 c2

main :: IO ()
main = do 
  outs <- runRP $ do
    -- initialize list
    head <- preQueueLoadedAppendLogFromVioletaBFT
    -- spawn 8 readers, each records 100000 snapshots of the list
    rts <- replicateM 8 $ forkRP $ readRP $ reader 1000000 0 head
    -- spawn a writer to delete the middle node
    wt  <- forkRP $ writeRP $ deleteMiddle head
    --wt <- forkRP $ writeRP $ return ()
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt@(ThreadState {tid}) -> do 
      v <- joinRP rt
      return $ show tid ++ ": " ++ show v
    -- wait for the writer to finish
    joinRP wt
    return outs
  forM_ outs putStrLn


