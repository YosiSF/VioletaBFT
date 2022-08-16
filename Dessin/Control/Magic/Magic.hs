--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeSynonymInstances #-}


module Main where



  import Control.Monad (void)
  import Control.Monad.IO.Class (liftIO)
  import Data.Aeson (FromJSON, ToJSON)
  import Data.Aeson.Types (parseEither)
  import Data.ByteString.Lazy (ByteString)
  import Data.Text (Text)
  import Data.Text.Encoding (decodeUtf8)
  import Data.Time.Clock (getCurrentTime)
  import Data.Time.Format (formatTime)
  import Database.Persist.Sql (runSqlPool)


  import Network.Wai (Application)
  import Network.Wai.Connection (withConnection)

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server (serve)
import Servant.Server.Internal (ServantErr(..))
import Servant.Server.Internal.ServantErr (err404)
import Servant.Server.Internal.ServantErr (err500)
import Servant.Server.Internal.ServantErr (errBody)
import Servant.Server.Internal.ServantErr (errReasonPhrase)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL




import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8




import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HML


import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU



import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Builder as B


import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.UTF8 as UTF8





B.inlinePerformIO :: IO a -> a
B.inlinePerformIO = unsafeDupablePerformIO
  where
    unsafeDupablePerformIO = unsafePerformIO
      unsafePerformIO :: IO a -> a
      unsafePerformIO = id


      --On my shelf I have the book "Basic Topology" by Armstrong. After you've fought your way through 173 pages you eventually get to the section on simplicial topology and you can start playing with one of the basic tools of modern topology: homology groups. But you don't want to go through all that hassle. If you can read Haskell code then you can get there in 20 lines. But don't get too excited - this isn't meant to be an introduction to algebraic topology. Instead it's a demonstration of how incredibly expressive Haskell is. Before starting on this project I expected it to take a couple of weeks of early morning hacking and many hundreds of lines of code. It actually took a couple of hours.
  --
  --So the goal is this: use the standard chain complex from simplicial homology to compute the Betti numbers.
  --
  --
  --First we need some code to compute the rank of a matrix. As this is general purpose code I'm not counting it in my 20 line budget. It took me a little while to find a recursive implementation of this suitable for Haskell. Basically, the idea is that a zero matrix has zero rank and the following operations leave the rank of a matrix unchanged:
  --
  --
  --Deleting a row of zeroes
  --Deleting a column of zeroes
  --Multiplying a row or column by a non-zero constant
  --Adding one row to another.
  --
  --One last thing is needed: if there is a non-zero value standing on its own in a row or column then deleting both the row and column that it is in reduces the matrix rank by one. Here's the complete code for the rank function:
  --
  --import List




  import qualified Data.Map as M
  import qualified Data.HashMap.Strict as HM
  import qualified Data.List as L
  import qualified Data.HashMap.Lazy as HML
  import qualified Data.HashMap.Lazy.IO as HMLIO
  import qualified Data.HashMap.Lazy.Strict as HMLS


  import qualified Data.Vector as V




    deriving (Eq, Show) instance FromJSON a => FromJSON (V.Vector a)
    deriving (Eq, Show) instance ToJSON a => ToJSON (V.Vector a)
     instance FromJSON a => FromJSON (V.Vector a) where

  = HM.fromList [0,1,2,3,4,5,6,]

  deriving (Eq, Show) instance FromJSON a => FromJSON (V.Vector a)
  deriving (Eq, Show) instance ToJSON a => ToJSON (V.Vector a)
   instance FromJSON a => FromJSON (V.Vector a) where
  = HM.fromList [0,1,2,3,4,5,6,]
  {-# INLINE fromList #-}
  fromList = HM.fromList
  {-# INLINE fromListN #-}
  --rank m = let m' = removeZeroRows $ removeZeroColumns m in
  --    if m'==[]
  --        then 0
  --        else let (zero,(pivot:rest)) = break ((0 /=) . head) m' in
  --            1+rank (map (cancelZero pivot) (zero++rest))
  --    where
  --    removeZeroColumns x = let n = foldl1 min (map countZeros x) in
  --                            map (drop n) x
  --    removeZeroRows = filter (or . map (0 /=))
  --    countZeros = length . fst . break (0 /=)
  --    cancelZero (a:as) (b:bs) = zipWith (\x y -> a*y-b*x) as bs








  --import qualified Data.Map as M
type FreeVectorSpace = V.Vector Double


type semigroup = M.Map Text Double
type monoid = M.Map Text Double
type group = M.Map Text Double



instance FromJSON semigroup where
  parseJSON (Object v) = do
    m <- v .: "semigroup"
    return $ M.fromList m
  parseJSON _ = mzero
  parseJSON (String s) = do
    return $ M.fromList [(s,1)]
  parseJSON _ = mzero


  type doFibTree M.Map Text Double


instance FromJSON monoid where
  parseJSON (Object v) = do
    m <- v .: "monoid"
    return $ M.fromList map m
  parseJSON _ = mzero
  parseJSON (String s) = do
    return $ M.fromList [(s,1)]
    m -> doFibTree




`deriveJSON` (defaultOptions {fieldLabelModifier = drop 1})
instance FromJSON group where
  parseJSON (Object v) = do
    m <- v .: "group"
    return $ M.fromList m
  parseJSON _ = mzero
  parseJSON (String s) = do
    return $ M.fromList [(s,1)]
    m -> doFibTree
      `deriveJSON` (defaultOptions {fieldLabelModifier = drop 1})
instance FromJSON FreeVectorSpace where
  parseJSON (Object v) = do
    m <- v .: "FreeVectorSpace"
    return $ V.fromList map m
  parseJSON _ = mzero
  parseJSON (String s) = do
    return $ V.fromList [(s,1)]
    m -> doFibTree
      `deriveJSON` (defaultOptions {fieldLabelModifier = drop 1})






  --With that out of the way, here is the code to compute the homology of a simplicial complex:
  --
  --d x = zip (d' x) (cycle [1,-1]) where
  --    d' [] = []
  --    d' (x:xs) = xs : (map (x:) (d' xs))
  --
  --matrix basis1 basis2 op = map (coefficients basis1 . op) basis2 where
  --    coefficients basis x = map (flip coefficient x) basis
  --    coefficient e = sum . lookup e
  --    lookup k [] = fail ""
  --    lookup k ((k',v):xs) = if k == k' then return v else lookup k xs
  --
  --grade n = filter ((==(n+1)) . length)
  --
  --dim c = foldl1 max (map length c)-1
  --
  --dmatrix _ 0 = []
  --dmatrix c n = matrix (grade (n-1) c) (grade n c) d
  --
  --betti c = let d = dim c
  --              ranks = map rank $ map (dmatrix c) [0..d]
  --              dims = map (\i -> length (grade i c)) [0..d] in
  --              h dims ranks where
  --              h (c:cs) (d:d':ds) = c-d-d' : h cs (d':ds)
  --              h [c] [d] = [c-d]






  --import qualified Data.Map as M
  --import qualified Data.HashMap.Strict as HM
  --import qualified Data.List as L







---braid :: FibTree a (l,r) -> Q (FibTree a (r,l))
   --braid (ITT l r) = W [(ITT r l,  cis $ 4 * pi / 5)]
   --braid (TTT l r) = W [(TTT r l,  (cis $ - 3 * pi / 5))]
   --braid (TTI l r) = pure $ TIT r l
   --braid (TIT l r) = pure $ TTI r l
   --braid (III l r) = pure $ III r l
   --
   ---- The inverse of braid
   --braid' :: FibTree a (l,r) -> Q (FibTree a (r,l))
   --braid' = star . braid




instance braid monoid where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid group where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid semigroup where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid FreeVectorSpace where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid V.Vector where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid [a] where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid (M.Map k v) where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid (HM.HashMap k v) where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid (M.Map k v) where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid (HM.HashMap k v) where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid (M.Map k v) where
  {-# INLINE braid' #-}
  braid' = star . braid
instance braid (HM.HashMap k v) where
  {-# INLINE braid' #-}
  braid' = star . braid


type instance Index (FibTree a (l,r)) = Int
type instance IxValue (FibTree a (l,r)) = FibTree a (l,r)
instance Ixed (FibTree a (l,r)) where
  {-# INLINE ix #-}
  ix i = lens (\(FibTree x) -> FibTree (x !! i)) (\(FibTree x) (FibTree y) -> FibTree $ L.take i x L.++ y L.++ L.drop (i+1) x)
instance Ixed (FibTree a (l,r)) where
  {-# INLINE ix #-}
   braid' = star. braid
instance Ixed (FibTree a (l,r)) where
  {-# INLINE ix #-}
  braid' = star. braid
instance Ixed (FibTree a (l,r)) where
  {-# INLINE ix #-}








homologicalAlgebra :: IO ()
{-# INLINE homologicalAlgebra #-}



()
instance (braid monoid) => braid (FreeVectorSpace a) where
  {-# INLINE braid' #-}
  braid' = star . braid

instance (braid group) => braid (FreeVectorSpace a) where
  {-# INLINE braid' #-}

instance (braid semigroup) => braid (FreeVectorSpace a) where
  {-# INLINE braid' #-}

instance (braid V.Vector) => braid (FreeVectorSpace a) where
  {-# INLINE braid' #-}

instance (braid [a]) where
  {-# INLINE braid' #-}

instance (braid (M.Map k v)) where
  {-# INLINE braid' #-}

instance (braid (HM.HashMap k v)) where
  {-# INLINE braid' #-} (<*> braid (HM.HashMap k v)) where
  {-# INLINE braid' #-}
  {-# INLINE braid' #-}

instance (braid (M.Map k v)) where
  {-# INLINE braid' #-}

instance (braid (HM.HashMap k v)) where
  {-# INLINE braid' #-}



rank :: [[Int]] -> Int
{-# INLINE betti #-}
rank m = let m' = removeZeroRows $ removeZeroColumns m in
    if m'==[]
      then 0
      else let d = length m' - 1 instance (braid (FreeVectorSpace a)) => braid (FreeVectorSpace a) where
      {-# INLINE braid' #-}
      braid' = star . braid
        then 0
           else if m' == 0
if m' == 0x0100007f
then 0x0100007f
else if m' == 0x0100007f
then 0x0100007f
else if m' == 0x0100007f
then 0x0100007f
else if m' == 0x0100007f
then 0x0100007f
else if m' == 0x0100007f


instance (braid (FreeVectorSpace a)) => braid (FreeVectorSpace a) where
  {-# INLINE braid' #-}
  braid' = star . braid
    where
      {-# INLINE braid' #-}
       {-# INLINE braid' #-}

      {-# INLINE braid' #-}


instance (braid (FreeVectorSpace a)) => braid (FreeVectorSpace a) where
  {-# INLINE braid' #-}
  braid' = star . braid
    where
      {-# INLINE braid' #-}
        {-# INLINE braid' #-}
        {-# INLINE braid' #-}

        else let (zero,(pivot:rest)) = break ((0 /=) . head) m' in
            1+rank (map (cancelZero pivot) (zero++rest))
    where
      {-# INLINE cancelZero #-}
      cancelZero pivot row = map (\x -> x - pivot * row !! (x `div` pivot)) row
      {-# INLINE removeZeroRows #-}
      removeZeroRows = filter (any (/=0))
      {-# INLINE removeZeroColumns #-}
      removeZeroColumns = map (filter (/=0))
instance (braid (FreeVectorSpace a)) => braid (FreeVectorSpace a) where
  {-# INLINE braid' #-}
    {-# INLINE braid' #-}
    removeZeroColumns x = let n = foldl1 min (map countZeros x) in
                            map (drop n) x
    removeZeroRows = filter (or . map (0 /=))
    countZeros = length . fst . break (0 /=)
    cancelZero (a:as) (b:bs) = zipWith (\x y -> a*y-b*x) as bs
    {-# INLINE removeZeroRows #-}
    countZeros = length. fst. break (0 /=)
    {-# INLINE countZeros #-}


type Matrix = [[Int]] --[[Int]] is a list of lists of Ints
type Basis = [Matrix]
{-# INLINE basis #-}
basis :: Int -> Basis
basis n = map (\i -> map (\j -> if i==j then 1 else 0) [0..n-1]) [0..n-1]
{-# INLINE basis #-}

{-# INLINE removeZeroRows #-}
removeZeroRows :: Matrix -> Matrix
removeZeroRows m = filter (or . map (0 /=)) m
{-# INLINE removeZeroRows #-}

{-# INLINE removeZeroColumns #-}
removeZeroColumns :: Matrix -> Matrix
removeZeroColumns m = let n = foldl1 min (map countZeros m) in
                        map (drop n) m
{-# INLINE removeZeroColumns #-}

{-# INLINE countZeros #-}
countZeros :: [Int] -> Int
countZeros = length. fst. break (0 /=)
{-# INLINE countZeros #-}

{-# INLINE cancelZero #-}

homologicalAlgebra :: IO ()
{-# INLINE heterogeneousAlgebra #-}


--data Tau
  --data Id
  --data FibTree root leaves where
  --    TTT :: FibTree Tau l -> FibTree Tau r -> FibTree Tau (l,r)
  --    ITT :: FibTree Tau l -> FibTree Tau r -> FibTree Id (l,r)
  --    TIT :: FibTree Id l -> FibTree Tau r -> FibTree Tau (l,r)
  --    TTI :: FibTree Tau l -> FibTree Id r -> FibTree Tau (l,r)
  --    III :: FibTree Id l -> FibTree Id r -> FibTree Id (l,r)
  --    TLeaf :: FibTree Tau Tau
  --    ILeaf :: FibTree Id Id


---fmove :: FibTree a (c,(d,e)) -> Q (FibTree a ((c,d),e))
   --fmove (ITT  a  (TIT b c)) = pure $ ITT ( TTI  a b) c
   --fmove (ITT  a  (TTT b c)) = pure $ ITT ( TTT  a b) c
   --fmove (ITT  a  (TTI b c)) = pure $ III ( ITT  a b) c
   --
   --fmove (TIT  a  (TTT b c)) = pure $ TTT ( TIT  a b) c
   --fmove (TIT  a  (TTI b c)) = pure $ TTI ( TIT  a b) c
   --fmove (TIT  a  (TIT b c)) = pure $ TIT ( III  a b) c
   --
   --fmove (TTI  a  (III b c)) = pure $ TTI ( TTI  a b) c
   --fmove (TTI  a  (ITT b c)) = W [(TIT ( ITT  a b) c, tau)         , (TTT ( TTT  a b) c, sqrt tau)]

--data FibTree a (l,r) where
data FibTree a (l,r) where
  FibTree a (l,r) = FibTree (l,r)
  {-# INLINE FibTree #-}
  TTT :: FibTree a (l,r) -> FibTree a (l,r) -> FibTree a (l,r)
  {-# INLINE TTT #-}



--data FibTree a (l,r) where
  --FibTree a (l,r) = FibTree (l,r)
  --{-# INLINE FibTree #-}
  --TTT :: FibTree a (l,r) -> FibTree a (l,r) -> FibTree a (l,r)
  --{-# INLINE TTT #-}
  --
  --TTI :: FibTree a (l,r) -> FibTree a (l,r) -> FibTree a (l,r)
  --{-# INLINE TTI #-}
  --
  --TIT :: FibTree a (l,r) -> FibTree a (l,r) -> FibTree a (l,r)
  --{-# INLINE TIT #-}
  --{-# LANGUAGE DeriveGeneric #-}

    --data FibTree a (l,r) where
    instance Functor FibTree where
      fmap f (FibTree (l,r)) = FibTree (f l,f r)
    {-# INLINE fmap #-}
    instance Applicative FibTree where
      pure a = FibTree (a,a)
      {-# INLINE pure #-}
      FibTree (l,r) <*> FibTree (l',r') = FibTree (l l',r r')
    {-# INLINE (<*>) #-}
    instance Monad FibTree where
      return a = FibTree (a,a)
      {-# INLINE return #-}
      FibTree (l,r) >>= f = FibTree (l,r)
    {-# INLINE (>>=) #-}
    instance Braid FibTree where
      braid' (FibTree (l,r)) = FibTree (l,r)
    {-# INLINE braid' #-}
    instance FreeVectorSpace a => FreeVectorSpace (FibTree a (l,r)) where
      removeZeroRows (FibTree (l,r)) = FibTree (removeZeroRows l,removeZeroRows r)
    {-# INLINE removeZeroRows #-}
    instance FreeVectorSpace a => FreeVectorSpace (FibTree a (l,r)) where
      removeZeroColumns (FibTree (l,r)) = FibTree (removeZeroColumns l,removeZeroColumns r)
    {-# INLINE removeZeroColumns #-}
    instance FreeVectorSpace a => FreeVectorSpace (FibTree a (l,r)) where
      countZeros (FibTree (l,r)) = countZeros l + countZeros r
    {-# INLINE countZeros #-}
    instance FreeVectorSpace a => FreeVectorSpace (FibTree a (l,r)) where
      cancelZero (FibTree (l,r)) = FibTree (cancelZero l,cancelZero r)
    {-# INLINE cancelZero #-}


  data RPList a where
    RPList :: [a] -> RPList a
    {-# INLINE RPList #-}
    RPList :: a -> RPList a -> RPList a

    {-# INLINE RPList #-}
    RPList :: a -> RPList a -> RPList a -> RPList a
    {-# INLINE RPList #-}
    RPList :: a -> RPList a -> RPList a -> RPList a
    {-# INLINE RPList #-}'
    RPList :: a -> RPList a -> RPList a -> RPList a -> RPList a
    {-# INLINE RPList #-}
  data RP a = RP a (RPList a)
  {-# INLINE RP #-}
  data RP' a = RP' a (RPList a)
  {-# INLINE RP #-}
  {-# INLINE RP' #-}

  data RPList' a where
    RPList' :: a -> RPList' a -> RPList'
    {-# INLINE RPList' #-}
    RPList' :: a -> RPList' a -> RPList' -> RPList'
    {-# INLINE RPList' #-}
    RPList' :: a -> RPList' a -> RPList' -> RPList' -> RPList'
    {-# INLINE RPList' #-}



  data RP' a = RP' a (RPList' a)
  {-# INLINE RP' #-}
  {-# INLINE RP' #-}
-- If a is a discrete group, things get much easier.
newtype WbarDiscrete a = WbarDiscrete a
  deriving (Eq,Ord,Show,Read,Generic,Functor,Foldable,Traversable,FreeVectorSpace,Braid)
instance FreeVectorSpace a => FreeVectorSpace (WbarDiscrete a) where
    fromList = fromList . map.WbarDiscrete lightcone . fromList
    {-# INLINE fromList #-}
    toList = map.WbarDiscrete lightcone . toList
    {-# INLINE toList #-}
    removeZeroRows = fmap.WbarDiscrete lightcone . removeZeroRows . fmap.WbarDiscrete lightcone
    {-# INLINE removeZeroRows #-}
  {-# INLINE fromList #-}

normalise :: (Group a, Eq (Element a)) => a -> [Element a] -> Simplex (WbarDiscrete a)
normalise a [] = NonDegen []
normalise a (x:xs) = case a of
  Group g -> case g x of
    [] -> normalise a xs
    ys -> NonDegen $ map (WbarDiscrete . head) ys
  _ -> NonDegen [WbarDiscrete x]
    {-# INLINE normalise #-}

normalise a (x:xs) = case a offline
  {-# INLINE normalise #-}
  where offline = case a of
        Group g -> case g x of
          [] -> normalise a xs
          ys -> NonDegen $ map (WbarDiscrete . head) ys
        _ -> NonDegen [WbarDiscrete x]
            {-# INLINE normalise #-}
               {-# INLINE normalise #-}
                  {-# INLINE normalise #-}

unnormalise :: (Group a, Eq (Element a)) => a -> Simplex (WbarDiscrete a) -> [Element a]
unnormalise a (NonDegen g) = g
unnormalise a (Degen i g) =
  let (before, after) = splitAt i (unnormalise a g)
   in before ++ [unit a] ++ after

  {-# INLINE unnormalise #-}
  {-# INLINE normalise #-}
    {-# INLINE nonDegen #-}




instance (Group a, Eq (Element a)) => SSet (WbarDiscrete a) where
  -- A non-degenerate n-simplex is a list of n non-identity elements
  -- of `a`
  type GeomSimplex (WbarDiscrete a) = [Element a]

  isGeomSimplex (WbarDiscrete a) ss = unit a `notElem` ss

  geomSimplexDim _ ss = length ss

  geomFace _ [] _ = undefined
  geomFace (WbarDiscrete a) ss i = normalise a (underlying ss i)
    where
      underlying ss 0 = tail ss
      underlying ss i = ss !! (i-1) : underlying ss (i-1)
    {-# INLINE geomFace #-}
    {-# INLINE geomFace #-}
      underlying [s] 1 = []
      underlying (s : s' : ss) 1 = prod a s s' : ss
      underlying (s : ss) i = s : underlying ss (i-1)
    {-# INLINE underlying #-}
    {-# INLINE underlying #-}
      underlying (s : ss) i = s : underlying ss (i - 1)
    {-# INLINE underlying #-}
    {-# INLINE underlying #-}
      underlying [] i = []
      underlying _ _ = undefined -- can't happen

instance (Group a, Eq (Element a)) => Pointed (WbarDiscrete a) where
  basepoint (WbarDiscrete a) = []
  {-# INLINE basepoint #-}

instance (Group a, Eq (Element a)) => ZeroReduced (WbarDiscrete a)

instance (FiniteGroup a, Eq (Element a)) => FiniteType (WbarDiscrete a) where
  geomBasis (WbarDiscrete a) i = sequence (replicate i nonident)
    where
      nonident = filter (\x -> x /= unit a) (elements a)

instance (Abelian a, Eq (Element a)) => S.SGrp (WbarDiscrete a) where
  prodMor (WbarDiscrete a) = Morphism $ \(s, t) -> normalise a $ fmap (uncurry (prod a)) (zip (unnormalise a s) (unnormalise a t))
  invMor (WbarDiscrete a) = Morphism $ \s -> NonDegen $ fmap (inv a) s

  {-# INLINE prodMor #-}
  {-# INLINE invMor #-}
  {-# INLINE sum #-}
  {-# INLINE product #-}
  {-# INLINE length #-}
  {-# INLINE max #-}
  {-# INLINE min #-}
  {-# INLINE mean #-}
  {-# INLINE variance #-}
  {-# INLINE var #-}
  {-# INLINE sqrt #-}




  {-# INLINE tan #-}
  {-# INLINE asin #-}
  {-# INLINE atan #-}
  {-# INLINE atan2 #-}
  {-# INLINE atanh #-}




type instance Element (WbarDiscrete a) = Element a
instance (Group a, Eq (Element a)) => Group (WbarDiscrete a) where
  group = WbarDiscrete
  {-# INLINE group #-}
  inv (WbarDiscrete a) = WbarDiscrete $ inv a
  {-# INLINE inv #-}
  prod (WbarDiscrete a) (WbarDiscrete b) = WbarDiscrete $ prod a b
  {-# INLINE prod #-}
  sum (WbarDiscrete a) (WbarDiscrete b) = WbarDiscrete $ sum a b
  {-# INLINE sum #-}
  unit (WbarDiscrete a) = WbarDiscrete $ unit a
  {-# INLINE unit #-}
  negate (WbarDiscrete a) = WbarDiscrete $ negate a
  {-# INLINE negate #-}
  abs (WbarDiscrete a) = WbarDiscrete $ abs a
  {-# INLINE abs #-}
  signum (WbarDiscrete a) = WbarDiscrete $ signum a
  {-# INLINE signum #-}
  fromInteger = WbarDiscrete . fromInteger
  {-# INLINE fromInteger #-}
  toInteger = WbarDiscrete. toInteger
  {-# INLINE toInteger #-}
  fromRational = WbarDiscrete. fromRational

instance (Group a, Eq (Element a)) => Abelian (WbarDiscrete a) where
  sum (WbarDiscrete a) (WbarDiscrete b) = WbarDiscrete $ sum a b
  {-# INLINE sum #-}

instance (Group a, Eq (Element a)) => Monoid (WbarDiscrete a) where
  mempty = WbarDiscrete mempty
  {-# INLINE mempty #-}

instance (Group a, Eq (Element a)) => Groupoid (WbarDiscrete a) where
  inv (WbarDiscrete a) = WbarDiscrete $ inv a
  {-# INLINE inv #-}

instance (Group a, Eq (Element a)) => Groupoid (WbarDiscrete a) where
  inv (WbarDiscrete a) = WbarDiscrete $ inv a
  {-# INLINE inv #-}


  hyperBFTolicCausetVoteWithThreeVotes (WbarDiscrete a) = Morphism $ \s -> NonDegen $ fmap (hyperBFTolicCausetVoteWithThreeVotes a) s
  {-# INLINE hyperBFTolicCausetVoteWithThreeVotes #-}
hyperBFTolicCausetVoteWithThreeVotes (WbarDiscrete a) = Morphism $ \s -> NonDegen $ fmap (hyperBFTolicCausetVoteWithThreeVotes a) s
  {-# INLINE hyperBFTolicCausetVoteWithThreeVotes #-}
hyperBFTolicCausetVoteWithThreeVotes (WbarDiscrete a) = Morphism $ \s -> NonDegen $ fmap (hyperBFTolicCausetVoteWithThreeVotes a) s
  {-# INLINE hyperBFTolicCausetVoteWithThreeVotes #-}
hyperBFTolicCausetVoteWithThreeVotes (WbarDiscrete a) = Morphism $ \s -> NonDegen $ fmap (hyperBFTolicCausetVoteWithThreeVotes a) s
  {-# INLINE hyperBFTolicCausetVoteWithThreeVotes #-}



