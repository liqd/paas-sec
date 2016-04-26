{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-binds #-}

module PaasSec.AgendaS1.Anonymity
  ( HasOwner(owner)
  , kAnon
  , KAnon(..)
  ) where

import Control.Lens
import Data.List
import PaasSec.AgendaS1.Domain


-- * ownership

class HasOwner a where
    owner  :: Lens' a (Maybe UserLogin)

instance HasOwner Proposal where
    owner = propCreator

instance HasOwner Comment where
    owner = commentCreator


-- * k-anonymity

-- | "A release of data is said to have the k-anonymity property if the information for each person
-- contained in the release cannot be distinguished from at least k-1 individuals whose information
-- also appear in the release.'  [https://en.wikipedia.org/wiki/K-anonymity]
data KAnon = KAnon Int

-- | Given a k-anonymity threshold, list of all users in the system, and a list of pairs of query
-- and response, decide whether k-anonymity can be violated.
kAnon :: KAnon -> [UserAcc] -> [(PartialUserAcc, NumUsers)] -> Bool
kAnon = undefined


-- | FIXME: explain!
data DataPoint a = DataPoint
    { _dataPointRange     :: a
    , _dataPointUserInfo  :: PartialUserAcc
    }
  deriving (Eq, Ord, Show)

makeLenses ''DataPoint

-- | FIXME: explain!
class AbsSet a where
    absOverlap :: a -> a -> Bool
    absSubset  :: a -> a -> Bool

instance (Ord a) => AbsSet (a, a) where
    absOverlap (x, y) (x', y') = (x <= x' && x' <= y) || (x' <= x && x <= y')
    absSubset  (x, y) (x', y') = x >= x' && y <= y'


-- | In at least one of the data points, a user can be k-of-n-de-anonymized: for that data point,
-- the analyst can learn that one of a list of @k' < k@ known users is one of the users in it.
--
-- This function provides a tool for testing policy violations.  It gives a upper bound for
-- anonymity, but there is no way of telling how loose a fit this upper bound is.  Anonymity may be
-- worse than indicated, but is guaranteed not to be better.
--
-- Example: the data point answers the question "how many users with age between 30 and 40 have
-- posted comments in the last 4 weeks?".  it contains one user.  the number of users with age
-- between 30-40 is 3.  this breaks 4-of-n anonymity for any number n of users in the system.
--
-- More interesting example: [FIXME: explain intersection attacks.]
kAnon' :: [UserAcc] -> [DataPoint a] -> KAnon
kAnon' allUsers dataPoints = KAnon k
  where
    k = minimum $ length . kOf allUsers . view dataPointUserInfo <$> dataPoints


-- FIXME: improving kAnon bounds with AbsSet:
-- if two datapoints A, B do not overlap: @k'(A) = min(k(A), n-k(B))@ and vice versa.
-- if @A subset B@: @k'(A) = min(k(A), k(B))@.
-- (there are probably more of these rules, right?)


-- * match partial user accounts

-- | which users match a given list of partial user descriptions?
kOf' :: [UserAcc] -> [PartialUserAcc] -> [UserAcc]
kOf' = foldl' kOf

-- | which users match a given partial user description?
kOf :: [UserAcc] -> PartialUserAcc -> [UserAcc]
kOf allUsers partialUA = foldl' (flip ($)) allUsers runFields
  where
    go :: forall a. Eq a => Lens' PartialUserAcc (Maybe a) -> Lens' UserAcc a -> [UserAcc] -> [UserAcc]
    go lpua lua = case partialUA ^. lpua of
        Nothing -> id
        Just v  -> filter ((== v) . view lua)

    runFields :: [[UserAcc] -> [UserAcc]]
    runFields =
        [ go partialUserAccAge userAccAge
        , go partialUserAccGender userAccGender
        , go partialUserAccDepartment userAccDepartment
--        , go partialUserBadge userBadge  -- FIXME: add this to UserAcc redundantly so that we have it easier here?
        ]
