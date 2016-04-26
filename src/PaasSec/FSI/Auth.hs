{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module PaasSec.FSI.Auth (authorization) where

import Control.Lens
import Data.Maybe (isNothing)

import PaasSec.FSI.Domain
import PaasSec.FSI.Steps
import PaasSec.AgendaS1.Anonymity (HasOwner(..))
import PaasSec.AgendaS1.Auth (isCreatorOf, doesNotLearnAuthorOf)


-- | A step can be carried out iff this function says so.  The context on which the decision is
-- based consists of the user's role, login name, the process phase, and a continuation for writing
-- rules like "an object can be accessed if it satisfies a certain property".
authorization :: HasOwner r => Step a -> Role -> UserLogin -> Phase -> ((r -> Bool) -> Bool) -> Bool

authorization (Login _ps _) _ _ _ _ = True  -- (do not handle special case "only anonymous users can login".)
authorization (Logout _) _ _ _ _ = True

authorization (ListUserAccs _) Employee        _ _ _ = False
authorization (ListUserAccs _) Admin           _ _ _ = True
authorization (ListUserAccs _) Moderator       _ _ _ = False
authorization (ListUserAccs _) Analyst         _ _ _ = False

authorization (GetUserAcc u _) Employee        u' _ _ = u == u'
authorization (GetUserAcc _ _) Admin           _  _ _ = True
authorization (GetUserAcc u _) Moderator       u' _ _ = u == u'
authorization (GetUserAcc u _) Analyst         u' _ _ = u == u'

authorization (GetBadge _ _) Employee          _ _ _ = True
authorization (GetBadge _ _) Admin             _ _ _ = True
authorization (GetBadge _ _) Moderator         _ _ _ = True
authorization (GetBadge _ _) Analyst           _ _ _ = True

authorization (AddUserAcc _ _) Employee        _ _ _ = False
authorization (AddUserAcc _ _) Admin           _ _ _ = True
authorization (AddUserAcc _ _) Moderator       _ _ _ = False
authorization (AddUserAcc _ _) Analyst         _ _ _ = False

authorization (EditUserAcc ua _ _) Employee    u _ _ = u == ua ^. userAccLogin
authorization (EditUserAcc _  _ _) Admin       _ _ _ = True
authorization (EditUserAcc ua _ _) Moderator   u _ _ = u == ua ^. userAccLogin
authorization (EditUserAcc ua _ _) Analyst     u _ _ = u == ua ^. userAccLogin

authorization (DelUserAcc u _) Employee        u' _ _ = u == u'
authorization (DelUserAcc _ _) Admin           _  _ _ = True
authorization (DelUserAcc u _) Moderator       u' _ _ = u == u'
authorization (DelUserAcc u _) Analyst         u' _ _ = u == u'


authorization (ListProposals _) Employee        _ _ _ = True
authorization (ListProposals _) Admin           _ _ _ = True
authorization (ListProposals _) Moderator       _ _ _ = True
authorization (ListProposals _) Analyst         _ _ _ = True

authorization (GetProposal _ _) Employee        u _ cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetProposal _ _) Admin           u _ cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetProposal _ _) Moderator       u _ cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetProposal _ _) Analyst         u _ cont = cont (u `doesNotLearnAuthorOf`)

authorization (AddProposal _ _) Employee        _ p _ = p == ProposePhase
authorization (AddProposal _ _) Admin           _ _ _ = False
authorization (AddProposal _ _) Moderator       _ _ _ = False
authorization (AddProposal _ _) Analyst         _ _ _ = False

authorization (EditProposal p _ _) Employee     u h _ = u `isCreatorOf` p && h == ProposePhase
authorization (EditProposal _ _ _) Admin        _ _ _ = False
authorization (EditProposal _ _ _) Moderator    _ _ _ = False
authorization (EditProposal _ _ _) Analyst      _ _ _ = False

authorization (DelProposal p _) Employee        u h _ = u `isCreatorOf` p && h == ProposePhase
authorization (DelProposal _ _) Admin           _ _ _ = False
authorization (DelProposal _ _) Moderator       _ _ _ = True
authorization (DelProposal _ _) Analyst         _ _ _ = False

authorization (SetAnon _ _ _) Employee          _ _ _ = False
authorization (SetAnon _ _ _) Admin             _ _ _ = False
authorization (SetAnon _ _ _) Moderator         _ p _ = p == DiscussPhase
authorization (SetAnon _ _ _) Analyst           _ _ _ = False


-- FIXME: implement check: employee can make comments anonymously iff the proposal allows it (since
-- controversial).
authorization (MakeComment _ _ _ _) Employee    _ p _ = p == DiscussPhase
authorization (MakeComment _ _ _ _) Admin       _ _ _ = False
authorization (MakeComment _ _ _ _) Moderator   _ _ _ = False
authorization (MakeComment _ _ _ _) Analyst     _ _ _ = False

authorization (GetComment _ _) Employee        u _ cont = cont (u `doesNotLearnAnonAuthorOf`)
authorization (GetComment _ _) Admin           u _ cont = cont (u `doesNotLearnAnonAuthorOf`)
authorization (GetComment _ _) Moderator       u _ cont = cont (u `doesNotLearnAnonAuthorOf`)
authorization (GetComment _ _) Analyst         u _ cont = cont (u `doesNotLearnAnonAuthorOf`)

authorization (DelComment _ _) Employee        u _ cont = cont (u `isCreatorOf`)
authorization (DelComment _ _) Admin           _ _ _ = False
authorization (DelComment _ _) Moderator       _ _ _ = True
authorization (DelComment _ _) Analyst         _ _ _ = False

authorization (DeAnonComment _ _) Admin        _ _ _ = True
authorization (DeAnonComment _ _) _            _ _ _ = False

authorization (GetAuthor c _) Admin            _ _ _ = isDeAnonymized c
authorization (GetAuthor _ _) _                _ _ _ = False

-- FIXME: votes are anonymous by nature.  make sure that this is the case in the a3 process, if not
-- we need to check that they are anonymous when required.
authorization (CastVote _ _ _ _) Employee      _ _ _ = True
authorization (CastVote _ _ _ _) Admin         _ _ _ = False
authorization (CastVote _ _ _ _) Moderator     _ _ _ = False
authorization (CastVote _ _ _ _) Analyst       _ _ _ = False


-- Analysts may run analytics queries.  k-anonymity is not tested here, but in the interpreter.
-- See 'kAnon' in "PaasSec.Anonymity".
authorization (AnalyticsQuery _ _) Employee    _ _ _ = False
authorization (AnalyticsQuery _ _) Admin       _ _ _ = False
authorization (AnalyticsQuery _ _) Moderator   _ _ _ = False
authorization (AnalyticsQuery _ _) Analyst     _ _ _ = True


authorization (SetPhase _ _) Employee          _ _ _ = False
authorization (SetPhase _ _) Admin             _ _ _ = False
authorization (SetPhase _ _) Moderator         _ _ _ = True
authorization (SetPhase _ _) Analyst           _ _ _ = False


-- * helpers

-- | Only expose author if author has not chosen to be anonymous (and was allowed to do so in the
-- proposal settings.)
doesNotLearnAnonAuthorOf :: HasOwner a => UserLogin -> a -> Bool
doesNotLearnAnonAuthorOf u a = u `isCreatorOf` a || isNotAnon a || isNothing (a ^. owner)
  where
    isNotAnon = undefined
    -- FIXME: to implement this, we need to keep track of all anonymously generated content in the
    -- simulator context.  then we can decide whether a comment has been made anonymously or not.

-- | Have two (different) admins called 'DeAnonComment' on this comment?
isDeAnonymized :: ID Comment -> Bool
isDeAnonymized = undefined
    -- FIXME: to implement this, a similar trick as in @isNotAnon@ in 'doesNotLearnAnonAuthorOf' is
    -- needed.  to keep track of the state, the entire 'authorization' function probably will be
    -- wrapped in a reader monad, but that won't change the representation of the requirements much.
