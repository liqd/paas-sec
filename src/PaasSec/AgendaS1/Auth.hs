{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module PaasSec.AgendaS1.Auth (authorization, isCreatorOf, doesNotLearnAuthorOf) where

import Control.Lens
import Data.Maybe (isNothing)

import PaasSec.AgendaS1.Domain
import PaasSec.AgendaS1.Steps
import PaasSec.AgendaS1.Anonymity


-- | A step can be carried out iff this function says so.  The context on which the decision is
-- based consists of the user's role, login name, and a continuation for writing rules like "an
-- object can be accessed if it satisfies a certain property".  Such rules require inspection of the
-- object in order to decide whether it can be accessed.
authorization :: HasOwner r => Step a -> Role -> UserLogin -> ((r -> Bool) -> Bool) -> Bool

authorization (Login _ps _) _ _ _ = True  -- (do not handle special case "only anonymous users can login".)
authorization (Logout _) _ _ _ = True

-- the step "list user accounts" can be executed only by role "board assistance".  other properties
-- of the calling user or of the resulting user account list are not taken into account.
authorization (ListUserAccs _) Employee        _ _ = False
authorization (ListUserAccs _) BoardMember     _ _ = False
authorization (ListUserAccs _) BoardAssistance _ _ = True
authorization (ListUserAccs _) Analyst         _ _ = False
authorization (ListUserAccs _) TeamLead        _ _ = False
authorization (ListUserAccs _) DivisionLead    _ _ = False


-- the step "get user account" for user id @u@ can be executed by role "board assistance" or by the
-- user with id @u@.
authorization (GetUserAcc u _) Employee        u' _ = u == u'
authorization (GetUserAcc u _) BoardMember     u' _ = u == u'
authorization (GetUserAcc _ _) BoardAssistance _  _ = True
authorization (GetUserAcc u _) Analyst         u' _ = u == u'
authorization (GetUserAcc u _) TeamLead        u' _ = u == u'
authorization (GetUserAcc u _) DivisionLead    u' _ = u == u'

-- badges can be seen by everybody.
authorization (GetBadge _ _) Employee        _ _ = True
authorization (GetBadge _ _) BoardMember     _ _ = True
authorization (GetBadge _ _) BoardAssistance _ _ = True
authorization (GetBadge _ _) Analyst         _ _ = True
authorization (GetBadge _ _) TeamLead        _ _ = True
authorization (GetBadge _ _) DivisionLead    _ _ = True

authorization (AddUserAcc _ _) Employee        _ _ = False
authorization (AddUserAcc _ _) BoardMember     _ _ = False
authorization (AddUserAcc _ _) BoardAssistance _ _ = True
authorization (AddUserAcc _ _) Analyst         _ _ = False
authorization (AddUserAcc _ _) TeamLead        _ _ = False
authorization (AddUserAcc _ _) DivisionLead    _ _ = False

authorization (EditUserAcc ua _ _) Employee        u _ = u == ua ^. userAccLogin
authorization (EditUserAcc ua _ _) BoardMember     u _ = u == ua ^. userAccLogin
authorization (EditUserAcc _ _ _)  BoardAssistance _ _ = True
authorization (EditUserAcc ua _ _) Analyst         u _ = u == ua ^. userAccLogin
authorization (EditUserAcc ua _ _) TeamLead        u _ = u == ua ^. userAccLogin
authorization (EditUserAcc ua _ _) DivisionLead    u _ = u == ua ^. userAccLogin

authorization (DelUserAcc u _) Employee        u' _ = u == u'
authorization (DelUserAcc u _) BoardMember     u' _ = u == u'
authorization (DelUserAcc _ _) BoardAssistance _  _ = True
authorization (DelUserAcc u _) Analyst         u' _ = u == u'
authorization (DelUserAcc u _) TeamLead        u' _ = u == u'
authorization (DelUserAcc u _) DivisionLead    u' _ = u == u'


authorization (ListProposals _) Employee        _ _ = True
authorization (ListProposals _) BoardMember     _ _ = True
authorization (ListProposals _) BoardAssistance _ _ = True
authorization (ListProposals _) Analyst         _ _ = True
authorization (ListProposals _) TeamLead        _ _ = True
authorization (ListProposals _) DivisionLead    _ _ = True

-- proposals can be viewed by everybody, but the author must be either hidden or the calling user.
authorization (GetProposal _ _) Employee        u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetProposal _ _) BoardMember     u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetProposal _ _) BoardAssistance u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetProposal _ _) Analyst         u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetProposal _ _) TeamLead        u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetProposal _ _) DivisionLead    u cont = cont (u `doesNotLearnAuthorOf`)

authorization (AddProposal _p _) Employee        _ _ = True
authorization (AddProposal _p _) BoardMember     _ _ = False
authorization (AddProposal _p _) BoardAssistance _ _ = False
authorization (AddProposal _p _) Analyst         _ _ = False
authorization (AddProposal _p _) TeamLead        _ _ = True
authorization (AddProposal _p _) DivisionLead    _ _ = True

authorization (EditProposal old  _new _) Employee        u  _ = u `isCreatorOf` old
authorization (EditProposal _old _new _) BoardMember     _u _ = False
authorization (EditProposal _old _new _) BoardAssistance _u _ = False
authorization (EditProposal _old _new _) Analyst         _u _ = False
authorization (EditProposal old  _new _) TeamLead        u  _ = u `isCreatorOf` old
authorization (EditProposal old  _new _) DivisionLead    u  _ = u `isCreatorOf` old

authorization (DelProposal p  _) Employee        u _ = u `isCreatorOf` p
authorization (DelProposal _p _) BoardMember     _ _ = False
authorization (DelProposal _p _) BoardAssistance _ _ = True
authorization (DelProposal _p _) Analyst         _ _ = False
authorization (DelProposal p  _) TeamLead        u _ = u `isCreatorOf` p
authorization (DelProposal p  _) DivisionLead    u _ = u `isCreatorOf` p


authorization (MakeComment _p _c _) Employee        _ _ = True
authorization (MakeComment _p _c _) BoardMember     _ _ = True
authorization (MakeComment _p _c _) BoardAssistance _ _ = True
authorization (MakeComment _p _c _) Analyst         _ _ = False
authorization (MakeComment _p _c _) TeamLead        _ _ = True
authorization (MakeComment _p _c _) DivisionLead    _ _ = True

authorization (GetComment _ _) Employee        u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetComment _ _) BoardMember     u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetComment _ _) BoardAssistance u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetComment _ _) Analyst         u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetComment _ _) TeamLead        u cont = cont (u `doesNotLearnAuthorOf`)
authorization (GetComment _ _) DivisionLead    u cont = cont (u `doesNotLearnAuthorOf`)

authorization (CastVote _p _v _) Employee        _ _ = True
authorization (CastVote _p _v _) BoardMember     _ _ = True
authorization (CastVote _p _v _) BoardAssistance _ _ = False
authorization (CastVote _p _v _) Analyst         _ _ = False
authorization (CastVote _p _v _) TeamLead        _ _ = True
authorization (CastVote _p _v _) DivisionLead    _ _ = True


-- Analysts may run analytics queries.  k-anonymity is not tested here, but in the interpreter.
-- See 'kAnon' in "PaasSec.Anonymity".
authorization (AnalyticsQuery _ _) Employee        _ _ = False
authorization (AnalyticsQuery _ _) BoardMember     _ _ = True
authorization (AnalyticsQuery _ _) BoardAssistance _ _ = True
authorization (AnalyticsQuery _ _) Analyst         _ _ = True
authorization (AnalyticsQuery _ _) TeamLead        _ _ = False
authorization (AnalyticsQuery _ _) DivisionLead    _ _ = False


-- * combinators

isCreatorOf :: HasOwner a => UserLogin -> a -> Bool
isCreatorOf u a = a ^. owner == Just u

doesNotLearnAuthorOf :: HasOwner a => UserLogin -> a -> Bool
doesNotLearnAuthorOf u a = u `isCreatorOf` a || isNothing (a ^. owner)
