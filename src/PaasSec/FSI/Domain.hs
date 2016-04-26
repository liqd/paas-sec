{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module PaasSec.FSI.Domain
  ( module PaasSec.FSI.Domain
  , AS1.ID
  , AS1.Proposal(..)
  , AS1.ProtoProposal(..)
  , AS1.Comment(..)
  , AS1.ProtoComment(..)
  , AS1.Vote(..)
  , AS1.UserLogin
  , AS1.UserPass
  , AS1.FullName
  , AS1.propId
  , AS1.propCreator
  , AS1.propTitle
  , AS1.propDescription
  , AS1.propComments
  , AS1.propVotes
  , AS1.protoPropTitle
  , AS1.protoPropDescription
  , AS1.commentId
  , AS1.commentCreator
  , AS1.commentText
  , AS1.protoCommentText
  , AS1.NumUsers
  ) where

import Control.Lens
import Data.String.Conversions

import qualified PaasSec.AgendaS1.Domain as AS1


data Role =
      Employee
    | Admin
    | Moderator
    | Analyst
  deriving (Eq, Ord, Show)

data Phase =
      ProposePhase
    | DiscussPhase
    | ModeratePhase
    | ResultPhase
  deriving (Eq, Ord, Show)

data Anon = Anon | NonAnon
  deriving (Eq, Ord, Show)

data UserAcc =
      UserAcc
        { _userAccLogin      :: AS1.UserLogin
        , _userAccPass       :: AS1.UserPass
        , _userAccFullname   :: AS1.FullName
        -- (note that testing does not require to store email)
        , _userAccDepartment :: Department
        , _userAccSeniority  :: Seniority
        }
  deriving (Eq, Ord, Show)

-- | For analytics & anonymity.
data PartialUserAcc =
      PartialUserAcc
        { _partialUserAccDepartment :: Maybe Department
        , _partialUserAccSeniority  :: Maybe Seniority
        , _partialUserBadge         :: Maybe Badge
        }
  deriving (Eq, Ord, Show)

data Badge = BadgeDepartment Department | BadgeSeniority Seniority
  deriving (Eq, Ord, Show)

type Department = ST
type Seniority = ST


makeLenses ''UserAcc
makeLenses ''PartialUserAcc
