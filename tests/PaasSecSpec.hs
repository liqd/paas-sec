{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module PaasSecSpec where

import qualified PaasSec
import Test.Hspec


spec :: Spec
spec = do
    describe "dummy" . it "works" $ pending
    PaasSec.spec
