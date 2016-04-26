{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module PaasSec.FSI.Run.Simulate (run) where

import Control.Monad.Free
import Data.String.Conversions

import PaasSec.FSI.Domain
import PaasSec.FSI.Steps
import PaasSec.FSI.Auth ()


run :: Behavior a -> IO a
run (Pure r) = pure r
run (Free f) = step f


step :: Step (Behavior a) -> IO a

step (Login _ps k) = putStrLn "logged in" >> run k

step (Logout k) = putStrLn "logged out" >> run k


step (ListUserAccs k) = do
    umd <- pure ["login"]
    putStrLn $ "view user md: " <> show umd
    run $ k umd

step (GetUserAcc _ k) = do
    umd <- pure $ UserAcc "login" "pass" "full name" "DOH" "ef"
    putStrLn $ "view user md: " <> show umd
    run $ k umd

step (GetBadge _ k) = do
    putStrLn "view user badge."
    run $ k Nothing

step (AddUserAcc new k) = putStrLn ("add user account: " <> show new) >> run k

step (EditUserAcc old new k) = putStrLn ("edit user account: " <> show (old, new)) >> run k

step (DelUserAcc ua k) = putStrLn ("del user account: " <> show ua) >> run k


step (ListProposals k) = do
    pids <- pure [14]
    putStrLn $ "view proposals: " <> show pids
    run $ k pids

step (GetProposal pid k) = do
    putStrLn ("get prop: " <> show pid)
    run . k $ Proposal 14 (Just "login") "wef" "wef" [3] [VoteYes]

step (AddProposal p k) = putStrLn ("add prop: " <> show p) >> run k

step (EditProposal old new k) = putStrLn ("edit prop: " <> show (old, new)) >> run k

step (DelProposal p k) = putStrLn ("del prop: " <> show p) >> run k

step (SetAnon p a k) = putStrLn ("set anon: " <> show (p, a)) >> run k


step (MakeComment p c a k) = putStrLn ("make comment: " <> show (p, c, a)) >> run k

step (GetComment cid k) = do
    putStrLn ("get comment: " <> show cid)
    run . k $ Comment cid (Just "wef") "wef"

step (DelComment cid k) = do
    putStrLn ("del comment: " <> show cid)
    run k

step (DeAnonComment cid k) = do
    putStrLn ("de-anonymise comment: " <> show cid)
    run k

step (GetAuthor cid k) = do
    putStrLn ("get comment author: " <> show cid)
    run $ k 3

step (CastVote p v a k) = putStrLn ("vote: " <> show (p, v, a)) >> run k


step (AnalyticsQuery puas k) = putStrLn ("analytics query: " <> show puas) >> run (k [3])


step (SetPhase p k) = putStrLn ("set phase: " <> show p) >> run k
