{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module PaasSec.AgendaS1.Domain where

import Control.Lens
import Data.String.Conversions


data Role =
      Employee
    | BoardMember
    | BoardAssistance
    | Analyst
    | TeamLead
    | DivisionLead
  deriving (Eq, Ord, Show)

data Phase =
      ProposePhase
    | VotePhase
    | FreezePhase
    | ArchivePhase
  deriving (Eq, Ord, Show)

type ID a = Int

data Proposal = Proposal
    { _propId          :: ID Proposal
    , _propCreator     :: Maybe UserLogin
    , _propTitle       :: ST
    , _propDescription :: ST
    , _propComments    :: [ID Comment]
    , _propVotes       :: [Vote]
    }
  deriving (Eq, Ord, Show)

data ProtoProposal = ProtoProposal
    { _protoPropTitle       :: ST
    , _protoPropDescription :: ST
    }
  deriving (Eq, Ord, Show)

data Comment = Comment
    { _commentId      :: ID Comment
    , _commentCreator :: Maybe UserLogin
    , _commentText    :: ST
    }
  deriving (Eq, Ord, Show)

data ProtoComment = ProtoComment
    { _protoCommentText :: ST
    }
  deriving (Eq, Ord, Show)

data Vote = VoteYes | VoteNo
  deriving (Eq, Ord, Show)

data UserAcc =
      UserAcc
        { _userAccLogin      :: UserLogin
        , _userAccPass       :: UserPass
        , _userAccFullname   :: FullName
        , _userAccAge        :: Age
        , _userAccGender     :: Gender
        , _userAccDepartment :: Department
        }
  deriving (Eq, Ord, Show)

-- | For analytics & anonymity.
data PartialUserAcc =
      PartialUserAcc
        { _partialUserAccAge        :: Maybe Age
        , _partialUserAccGender     :: Maybe Gender
        , _partialUserAccDepartment :: Maybe Department
        , _partialUserBadge         :: Maybe Badge
        }
  deriving (Eq, Ord, Show)

data Badge = BadgeBoardMember | BadgeBoardAssistent
  deriving (Eq, Ord, Show)

type UserLogin  = ST
type UserPass   = ST

type FullName   = ST
type Age        = Int
data Gender     = Male | Female | Other  deriving (Eq, Ord, Show)
type Department = ST

type NumUsers = Int


makeLenses ''Proposal
makeLenses ''ProtoProposal
makeLenses ''Comment
makeLenses ''ProtoComment
makeLenses ''UserAcc
makeLenses ''PartialUserAcc
