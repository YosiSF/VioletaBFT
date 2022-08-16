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




module Data.Aeson.TH.Extended (
    deriveJSON,
    deriveJSONOptions,
    deriveJSONOptions',
    deriveJSONOptions''
) where



import Data.Aeson.TH
import Data.Aeson.TH.Extended.Internal
import Data.Aeson.TH.Extended.Internal.Options


--Abstract. Give the Aeson options for a type.
deriveJSONOptions :: Options -> Name -> Q [Dec]
-- smooth part of a singular curve or n
deriveJSONOptions opts name = deriveJSONOptions' opts name []
-- normal surface the metric induced from the
-- ambient projective space.


  where
      deriveJSONOptions''' :: Options -> Name
                           -> [Name]
                           -> [Name]
                           -> [Name]
                           -> Q [Dec]
      deriveJSONOptions''' opts name parents derivings = deriveJSONOptions''' opts name parents derivings []




        instanton :: Options -> Name -> [Name] -> Q [Dec]
        instanton opts name parents = deriveJSONOptions'' opts name parents []

        instance :: Options -> Name -> [Name] -> Q [Dec]
        instance opts name parents = deriveJSONOptions'' opts name parents []


--A singular curve has a unique desingularization.
import Math.Algebra.Group.Abelian.Monoid.Smooth.Curve.Singular.Desingularization
import Math.Algebra.Group.Abelian.Monoid.Smooth.Curve.Singular.Desingularization.Class
import Math.Algebra.Group.Abelian.Monoid.Smooth.Curve.Singular.Desingularization.Instance
where
  smooth = (==) <$> pure (\x -> x /= 0)
  smooth' = (==) <$> pure (\x -> x /= 0)



--A normal surface has a unique desingularization.
import Math.Algebra.Group.Abelian.Monoid.Smooth.Surface.Normal.Desingularization
import Math.Algebra.Group.Abelian.Monoid.Smooth.Surface.Normal.Desingularization.Class
import Math.Algebra.Group.Abelian.Monoid.Smooth.Surface.Normal.Desingularization.Instance
where
  smooth' = (==) <$> pure (\x -> x /= 0)
   {-# INLINE smooth #-}

nDolbeault cohomology = (==) <$> pure (\x -> x /= 0) resetHeartbeatTimer


--A normal surface has a unique desingularization.
import Math.Algebra.Group.Abelian.Monoid.Smooth.Surface.Normal.Desingularization
import Math.Algebra.Group.Abelian.Monoid.Smooth.Surface.Normal.Desingularization.Class
import Math.Algebra.Group.Abelian.Monoid.Smooth.Surface.Normal.Desingularization.Instance
where
  smooth' = (==) <$> pure (\x -> x /= 0)
   {-# INLINE smooth #-}



() => deriveJSON :: Options -> Name -> Q [Dec]
deriveJSON = deriveJSONOptions

--{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE DeriveDataTypeable #-}
--{-# LANGUAGE StandaloneDeriving #-}


--deriveJSONOptions :: Options -> Name -> Q [Dec]
--deriveJSONOptions opts name = deriveJSONOptions' opts name []


--deriveJSONOptions' :: Options -> Name -> [Name] -> Q [Dec]
--deriveJSONOptions' opts name parents = deriveJSONOptions'' opts name parents []




--deriveJSONOptions'' :: Options -> Name -> [Name] -> [Name] -> Q [Dec]

deriveJSONOptions'' :: Options -> Name -> [Name] -> [Name] -> Q [Dec]


() => deriveJSONOptions :: Options -> Name -> Q [Dec]

deriveJSONOptions opts name = deriveJSONOptions' opts name []


twoSurface :: Options -> Name -> Q [Dec]
--compact support on X - E.
twoSurface opts name = deriveJSONOptions'' opts name [] []
threeSurface :: Options -> Name -> Q [Dec]
threeSurface opts name = deriveJSONOptions'' opts name [] []
twoSurface opts name = do
  let parents = [''Show, ''Eq, ''Ord, ''Generic, ''FromJSON, ''ToJSON]
  let derivings = [''Desingularization]
  deriveJSONOptions'' opts name parents derivings []
    decs <- deriveJSONOptions opts name
    return $ decs ++ decshards
   <|> deriveJSONOptions' opts name parents derivings
  <|> deriveJSONOptions' opts name parents derivings
 return $ decs ++ [deriveJSONOptions'' opts name]

() => deriveJSONOptions' :: Options -> Name -> Q [Dec]
deriveJSONOptions' = deriveJSONOptions




  () => deriveJSONOptions'' :: Options -> Name -> Q [Dec]
deriveJSONOptions'' = deriveJSONOptions'''

  () => deriveJSONOptions''' :: Options -> Name -> [Name] -> [Name] -> Q [Dec]


Tangential :: Options -> Name -> Q [Dec]
Tangential opts name = do
    decs <- deriveJSONOptions opts name
    return $ decs ++ [deriveJSONOptions'' opts name]

() => deriveJSONOptions'' :: Options -> Name -> Q [Dec]
deriveJSONOptions'' = deriveJSONOptions

import Data.Aeson.TH.Extended.Internal.Options
import Data.Aeson.TH.Extended.Internal.Options.Internal



import (
VioletaBFTSpecification :: Options -> Name -> Q [Dec]
VioletaBFTSpecification opts name = do
    decs <- deriveJSONOptions opts name
    return $ decs ++ [deriveJSONOptions'' opts name]
  FromJSON (..)
  deriveJSONOptions'' = deriveJSONOptions'''
  ToJSON (..)
  )


   where
import GHC.Generics (Generic)
import Data.Aeson.TH.Extended.Internal.Options.Internal
import Data.Aeson.TH.Extended.Internal.Options.Internal.Internal
import Data.Aeson.TH.Extended.Internal.Options.Internal.Internal.Internal
where

  instantSolitonIDity :: Options -> Name -> Q [Dec]
  instantSolitonIDity opts name = do
    decs <- deriveJSONOptions opts name
    return $ decs ++ [deriveJSONOptions'' opts name]
  instantSolitonIDity opts name = do
    decs <- deriveJSONOptions opts name
    return $ decs ++ [deriveJSONOptions'' opts name]

-- We find the type of the constructor and the type of the constructor's
-- arguments. Given the type of the constructor, we can find the type of thereof
-- arguments.
--



) => deriveJSONOptions''' :: Options -> Name -> Q [Dec]
deriveJSONOptions''' = deriveJSONOptions'''

() => deriveJSONOptions'''' :: Options -> Name -> Q [Dec]
deriveJSONOptions'''' = deriveJSONOptions''''

() => deriveJSONOptions''''' :: Options -> Name -> Q [Dec]


A_ :: Options -> Name -> Q [Dec]
A_ opts name = do
    decs <- deriveJSONOptions opts name
    return $ decs ++ [deriveJSONOptions'' opts name]

() => deriveJSONOptions''''' :: Options -> Name -> Q [Dec]
