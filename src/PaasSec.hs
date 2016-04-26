{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-|

what we promised:

> Methode
>
> - Natürlichsprachliche Formulierung eines Bedrohungsmodells für zwei konkrete Prozesse.
> - Diese Modelle werden in eine DSL übersetzt, in der Nutzer-Aktionen und deren Effekte beschrieben werden.
> - Beispiel: Die Sicherheitsbedingung "X darf Proposal Y nicht löschen" ist erfüllt, wenn immer wenn X die fragliche Aktion ausführt, der Fehler "nicht erlaubt" auftritt.
> - Ein Interpreterprogramm für die DSL, dass alle Nutzeraktionen in REST-Api-Aufrufe übersetzt und automatisch die Sicherheitsbedingungen testet, ist für spätere in diesem WP geplant.
>
>
> Ergebnisse
>
> - Die Übersetzung von natürlicher Sprache in die DSL verlief ohne Komplikationen.
> - Eine Erprobung des Konzepts an einem Toy-Interpreter im Labor war erfolgreich.
> - Die DSL ist hinreichend leicht erweiterbar, so dass unerwartete neue Sicherheitsbedingungen sich leicht ergänzen lassen.
> - Konkretere "Ergebnisse" sind erst zu erwarten, wenn diese Methode fertig um- und eingesetzt worden ist.
>
>
> Erkenntnisse
>
> Aufgrund der Umstrukturierungen bei liqd e.V. wurde ein früherer Plan, einen IFC-Policy-Compiler
> (Information Flow Control) zu bauen, nicht zuende umgesetzt.  Die bis zu diesem Zeitpunkt
> abgelieferten Deliverables mussten dadurch teilweise überarbeitet werden (insbes. die REST-Apis).
>
> Außerdem lässt sich mit der neuen Methode nicht ausschließen, dass die zu schützende Information
> nicht auf einem Weg leakt, die durch die Tests nicht erwartet wird.  Dies war bei der vorherigen
> Methode geplant.  Der schließlich gewählte Ansatz ist ein pragmatischer Kompromiss zwischen
> Mächtigkeit der Testsprache und Umsetzbarkeit.


TODO:

- phases (last page of the scenario): manage phases, trigger timeouts for testing purposes.
- edit analytics dashboard (analyst only).
- smallcheck?

SEE ALSO:

- https://github.com/pallix/paas-pseudonymity
- https://github.com/liqd/adhocracy3/blob/master/src/adhocracy_s1/adhocracy_s1/workflows/test_s1.py

-}
module PaasSec (spec) where

import Control.Monad.IO.Class (liftIO)
import Test.Hspec

import qualified PaasSec.AgendaS1.Domain       as AS1
import qualified PaasSec.AgendaS1.Run.Simulate as AS1
import qualified PaasSec.AgendaS1.Steps        as AS1

import qualified PaasSec.FSI.Domain       as FSI
import qualified PaasSec.FSI.Run.Simulate as FSI
import qualified PaasSec.FSI.Steps        as FSI


spec :: Spec
spec = describe "yeay" . it "works" $ do
    liftIO $ do
        putStrLn "---------------------------"
        AS1.run programAS1
        FSI.run programFSI
        putStrLn "---------------------------"
    True `shouldBe` True


-- this is how system behavior can be described manually.  the implementation will generate these
-- behaviors automatically, though, so ideally there is not much need for writing programs like
-- this.  the implementation will also check auth and anon requirements automatically, as defined in
-- the types, functions (representing decision tables) in @PaasSec.*.{Domain,Auth,Steps,Anonymity}@.

editOneProposalAS1 :: AS1.Behavior ()
editOneProposalAS1 = do
    AS1.login "password"
    ([pid] :: [AS1.ID AS1.Proposal]) <- AS1.listProposals
    p <- AS1.getProposal pid
    AS1.editProposal p (AS1.ProtoProposal "" "")
    AS1.logout

editOneProposalFSI :: FSI.Behavior ()
editOneProposalFSI = do
    FSI.login "password"
    ([pid] :: [FSI.ID FSI.Proposal]) <- FSI.listProposals
    p <- FSI.getProposal pid
    FSI.editProposal p (FSI.ProtoProposal "" "")
    FSI.logout




-- -- this function can (1) make writing down simple programs more convenient; (2) make implicit things
-- -- happen in the background, like collecting all the values occurring in bind.
-- stepsToBehavior :: [Step ()] -> Behavior ()
-- stepsToBehavior = _
