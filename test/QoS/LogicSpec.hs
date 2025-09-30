module QoS.LogicSpec where

import CFSM
import Control.Monad (liftM, liftM2, liftM3)
import Control.Monad.Trans.State.Lazy(evalStateT)
import Data.Map as M
import Data.Set as S
import QoS.Logic
import QoS.CFSM
import SyntacticGlobalChoreographies
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Language.SMTLIB as SMTLIB
import Language.SMTLIB.Util as SMTLIB
import TS
import qualified GCParser
import Data.Maybe ( fromJust, isJust )
import QoS.Logic (MemorizedData(gchorToPomset))

attributeSetWithDefaultOp :: Set AttributeName -> Set QoSAttr
attributeSetWithDefaultOp = S.map (`QoSAttr` "+")

spec :: Spec
spec = do
    -- describe "pushNegations" $ do
    --     context "with symbol Not" $ do
    --         it "pushes negations to atoms" $ do
    --             pushNegations (Not T) `shouldBe` F
    --             pushNegations (Not F) `shouldBe` T

    --     context "with symbol Or" $ do
    --         it "pushes negations to atoms" $ do
    --             pushNegations (Not (Or T T)) `shouldBe` And F F
    --             pushNegations (Not (Or T F)) `shouldBe` And F T
    --             pushNegations (Not (Or F T)) `shouldBe` And T F
    --             pushNegations (Not (Or F F)) `shouldBe` And T T

    --     context "with symbol And" $ do
    --         it "pushes negations to atoms" $ do
    --             pushNegations (Not (And T T)) `shouldBe` Or F F
    --             pushNegations (Not (And T F)) `shouldBe` Or F T
    --             pushNegations (Not (And F T)) `shouldBe` Or T F
    --             pushNegations (Not (And F F)) `shouldBe` Or T T

    --     context "with multiple propositional symbols" $ do
    --         it "pushes negations to atoms" $ do
    --             pushNegations (Not (And (Or (Not F) T) (And T T)))
    --                 `shouldBe` Or (And F F) (Or F F)
    --             pushNegations (Not (Not (And (Or (Not F) T) (And T T))))
    --                 `shouldBe` And (Or T T) (And T T)

    --     context "with  symbol Until" $ do
    --         it "pushes negations to atoms" $ do
    --             pushNegations (Not (Until T Emp T)) `shouldBe` Release F Emp F
    --             pushNegations (Not (Until T Emp F)) `shouldBe` Release F Emp T
    --             pushNegations (Not (Until F Emp T)) `shouldBe` Release T Emp F
    --             pushNegations (Not (Until F Emp F)) `shouldBe` Release T Emp T

    --             pushNegations (Not (Until (Until T Emp T) Emp T))
    --                 `shouldBe` Release (Release F Emp F) Emp F
    --             pushNegations (Not (Until (Until T Emp F) Emp T))
    --                 `shouldBe` Release (Release F Emp T) Emp F
    --             pushNegations (Not (Until (Until F Emp T) Emp T))
    --                 `shouldBe` Release (Release T Emp F) Emp F
    --             pushNegations (Not (Until (Until F Emp F) Emp T))
    --                 `shouldBe` Release (Release T Emp T) Emp F

    --     context "with  symbol Release" $ do
    --         it "pushes negations to atoms" $ do
    --             pushNegations (Not (Release T Emp T)) `shouldBe` Until F Emp F
    --             pushNegations (Not (Release T Emp F)) `shouldBe` Until F Emp T
    --             pushNegations (Not (Release F Emp T)) `shouldBe` Until T Emp F
    --             pushNegations (Not (Release F Emp F)) `shouldBe` Until T Emp T

    --             pushNegations (Not (Release (Release T Emp T) Emp T))
    --                 `shouldBe` Until (Until F Emp F) Emp F
    --             pushNegations (Not (Release (Release T Emp F) Emp T))
    --                 `shouldBe` Until (Until F Emp T) Emp F
    --             pushNegations (Not (Release (Release F Emp T) Emp T))
    --                 `shouldBe` Until (Until T Emp F) Emp F
    --             pushNegations (Not (Release (Release F Emp F) Emp T))
    --                 `shouldBe` Until (Until T Emp T) Emp F

    --     context "with  symbols Until, Release" $ do
    --         it "pushes negations to atoms" $ do
    --             pushNegations (Not (Until (Release T Emp T) Emp T))
    --                 `shouldBe` Release (Until F Emp F) Emp F
    --             pushNegations (Not (Until (Release T Emp F) Emp T))
    --                 `shouldBe` Release (Until F Emp T) Emp F
    --             pushNegations (Not (Until (Release F Emp T) Emp T))
    --                 `shouldBe` Release (Until T Emp F) Emp F
    --             pushNegations (Not (Until (Release F Emp F) Emp T))
    --                 `shouldBe` Release (Until T Emp T) Emp F

    --             pushNegations (Not (Release (Until T Emp T) Emp T))
    --                 `shouldBe` Until (Release F Emp F) Emp F
    --             pushNegations (Not (Release (Until T Emp F) Emp T))
    --                 `shouldBe` Until (Release F Emp T) Emp F
    --             pushNegations (Not (Release (Until F Emp T) Emp T))
    --                 `shouldBe` Until (Release T Emp F) Emp F
    --             pushNegations (Not (Release (Until F Emp F) Emp T))
    --                 `shouldBe` Until (Release T Emp T) Emp F

    let machineC =
            (S.fromList ["0", "1", "2", "3", "4", "5", "6", "7", "8"], "0",
                S.fromList [(("C", "S"), Send, "ack"),
                            (("C", "S"), Send, "helo"),
                            (("C", "S"), Send, "quit"),
                            (("C", "S"), Send, "read"),
                            (("C", "S"), Send, "retr"),
                            (("S", "C"), Receive, "bye"),
                            (("S", "C"), Receive, "int"),
                            (("S", "C"), Receive, "msg"),
                            (("S", "C"), Receive, "size")],
                S.fromList [("0", (("C", "S"), Send, "helo"), "3"),
                            ("1", (("S", "C"), Receive, "bye"), "2"),
                            ("3", (("S", "C"), Receive, "int"), "4"),
                            ("4", (("C", "S"), Send, "quit"), "1"),
                            ("4", (("C", "S"), Send, "read"), "5"),
                            ("5", (("S", "C"), Receive, "size"), "6"),
                            ("6", (("C", "S"), Send, "retr"), "7"),
                            ("7", (("S", "C"), Receive, "msg"), "8"),
                            ("8", (("C", "S"), Send, "ack"), "4")])
        machineS =
            (S.fromList ["0", "1", "2", "3", "4", "5", "6", "7", "8"], "0",
                S.fromList [(("C", "S"), Receive, "ack"),
                            (("C", "S"), Receive, "helo"),
                            (("C", "S"), Receive, "quit"),
                            (("C", "S"), Receive, "read"),
                            (("C", "S"), Receive, "retr"),
                            (("S", "C"), Send, "bye"),
                            (("S", "C"), Send, "int"),
                            (("S", "C"), Send, "msg"),
                            (("S", "C"), Send, "size")],
                S.fromList [("0", (("C", "S"), Receive, "helo"), "3"),
                            ("1", (("S", "C"), Send, "bye"), "2"),
                            ("3", (("S", "C"), Send, "int"), "4"),
                            ("4", (("C", "S"), Receive, "quit"), "1"),
                            ("4", (("C", "S"), Receive, "read"), "5"),
                            ("5", (("S", "C"), Send, "size"), "6"),
                            ("6", (("C", "S"), Receive, "retr"), "7"),
                            ("7", (("S", "C"), Send, "msg"), "8"),
                            ("8", (("C", "S"), Receive, "ack"), "4")])
        sys =
            ([
                machineC,
                machineS
            ], M.fromList [(0, "C"), (1, "S")])
        singletonsys = ([
                (S.fromList ["0"], "0", S.empty, S.empty)
            ], M.fromList [(0, "C")])
        emptysys = ([
                (S.empty, "0", S.empty, S.empty)
            ], M.fromList [(0, "C")])

        emptyRun = []
        conf1 = (["0", "0"], M.fromList [(( "C", "S" ), []), (( "S", "C" ), [])])
        kevent1 = ("0", "0", "C", "S", Send, "helo")
        conf2 = (["3", "0"], M.fromList [(( "C", "S" ), ["helo"]), (( "S", "C" ), [])])
        kevent2 = ("3", "0", "C", "S", Receive, "helo")
        conf3 = (["3", "3"], M.fromList [(( "C", "S" ), []), (( "S", "C" ), [])])
        singletonRun = [(conf1 , kevent1, conf2)]
        twoEventsRun = singletonRun ++ [(conf2 , kevent2, conf3)]
        gammaLow = Just $ SMTLIB.parseTerm "(and (> time 10) (and (> cost 0) (< cost 1)))"
        gammaHigh = Just $ SMTLIB.parseTerm "(and (and (> time 0) (< time 5)) (> cost 1))"
        qosMapC = M.fromList [(("C","0"), gammaLow), (("C","1"), gammaLow), (("C","2"), gammaLow), (("C","3"), gammaHigh), (("C","4"), gammaHigh), (("C","5"), gammaHigh), (("C","6"), gammaHigh), (("C","7"), gammaHigh), (("C","8"), gammaHigh)]
        qosMapS = M.fromList [(("S","0"), gammaLow), (("S","1"), gammaLow), (("S","2"), gammaLow), (("S","3"), gammaHigh), (("S","4"), gammaHigh), (("S","5"), gammaHigh), (("S","6"), gammaHigh), (("S","7"), gammaHigh), (("S","8"), gammaHigh)]
        qosMap = M.union qosMapC qosMapS

    describe "aggregatedAttrsSet" $ do

        context "empty attributes set" $ do
            it "returns empty set" $ do
                aggregatedAttrsSet sys S.empty M.empty `shouldBe` S.empty
        context "empty system" $ do
            it "returns initial QoS Attr set" $ do
                let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["a", "b", "c", "d"]
                aggregatedAttrsSet emptysys qosattrset M.empty `shouldBe` S.fromList ["a", "b", "c", "d"]
        context "singleton system" $ do
            it "returns aggregated attributes set" $ do
                let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["a", "b", "c", "d"]
                aggregatedAttrsSet singletonsys qosattrset M.empty
                `shouldBe` S.fromList ["a", "b", "c", "d",
                                        "a_C_0", "b_C_0", "c_C_0", "d_C_0"]
        context "singleton attribute set" $ do
            it "returns aggregated attributes set" $ do
                let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["a"]
                aggregatedAttrsSet sys qosattrset M.empty
                `shouldBe` S.fromList [ "a",
                                        "a_C_0", "a_C_1", "a_C_2", "a_C_3",
                                        "a_C_4", "a_C_5", "a_C_6", "a_C_7", "a_C_8",
                                        "a_S_0", "a_S_1", "a_S_2", "a_S_3",
                                        "a_S_4", "a_S_5", "a_S_6", "a_S_7", "a_S_8"
                                        ]
        context "two attributes set" $ do
            it "returns aggregated attributes set" $ do
                let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["a", "b"]
                aggregatedAttrsSet sys qosattrset M.empty
                `shouldBe` S.fromList [ "a",
                                        "a_C_0", "a_C_1", "a_C_2", "a_C_3",
                                        "a_C_4", "a_C_5", "a_C_6", "a_C_7", "a_C_8",
                                        "a_S_0", "a_S_1", "a_S_2", "a_S_3",
                                        "a_S_4", "a_S_5", "a_S_6", "a_S_7", "a_S_8",
                                        "b",
                                        "b_C_0", "b_C_1", "b_C_2", "b_C_3",
                                        "b_C_4", "b_C_5", "b_C_6", "b_C_7", "b_C_8",
                                        "b_S_0", "b_S_1", "b_S_2", "b_S_3",
                                        "b_S_4", "b_S_5", "b_S_6", "b_S_7", "b_S_8"
                                        ]

    describe "TS.getStatesFromRun" $ do
        context "empty run" $ do
            it "returns empty list" $ do
                getStatesFromRun sys emptyRun `shouldBe` []
        context "singleton run" $ do
            it "returns list of all local states participating in the run" $ do
                getStatesFromRun sys singletonRun
                    `shouldBe` [("C","0"),("C","3"),("S","0")]
        context "two events run" $ do
            it "returns list of all local states participating in the run" $ do
                getStatesFromRun sys twoEventsRun
                    `shouldBe` [("C","0"),("S","0"),("C","3"), ("S","3")]

    describe "getQoSSpec" $ do
            it "returns correct QoSSpec with renamed attrs" $ do
                let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["time", "cost"]
                show (fromJust (getQoSSpec qosattrset qosMap ("C","0"))) `shouldBe` "( and ( > time_C_0 10 ) ( and ( > cost_C_0 0 ) ( < cost_C_0 1 ) ) )"
                show (fromJust (getQoSSpec qosattrset qosMap ("C","3"))) `shouldBe` "( and ( and ( > time_C_3 0 ) ( < time_C_3 5 ) ) ( > cost_C_3 1 ) )"
                show (fromJust (getQoSSpec qosattrset qosMap ("S","0"))) `shouldBe` "( and ( > time_S_0 10 ) ( and ( > cost_S_0 0 ) ( < cost_S_0 1 ) ) )"
                show (fromJust (getQoSSpec qosattrset qosMap ("S","3"))) `shouldBe` "( and ( and ( > time_S_3 0 ) ( < time_S_3 5 ) ) ( > cost_S_3 1 ) )"

    describe "aggregation" $ do
        context "empty run" $ do
            it "returns empty QoSSpec" $ do
                let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["time", "cost"]
                aggregation sys qosattrset qosMap emptyRun `shouldBe` SMTLIB.smtAND SMTLIB.smtTrue SMTLIB.smtTrue
        context "singleton run" $ do
            it "returns aggregated QoSSpec" $ do
                let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["time", "cost"]
                show (aggregation sys qosattrset qosMap singletonRun)
                    `shouldBe`
                        "\
                        \( and \

                            \( and \
                                \( and \
                                    \( and \
                                        \true \
                                        \( and ( > time_C_0 10 ) ( and ( > cost_C_0 0 ) ( < cost_C_0 1 ) ) )\
                                    \ ) \
                                \( and ( > time_S_0 10 ) ( and ( > cost_S_0 0 ) ( < cost_S_0 1 ) ) )\
                                \ ) \

                                \( and \
                                    \( and \
                                        \true \
                                        \( = cost ( + ( + cost_C_0 cost_C_3 ) cost_S_0 ) )\
                                    \ ) \
                                    \( = time ( + ( + time_C_0 time_C_3 ) time_S_0 ) )\
                                \ ) \
                            \) \

                            \( and ( and ( > time_C_3 0 ) ( < time_C_3 5 ) ) ( > cost_C_3 1 ) ) \
                        \)"

        context "two events run" $ do
            it "returns aggregated QoSSpec" $ do
                let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["time", "cost"]
                show (aggregation sys qosattrset qosMap twoEventsRun)
                    `shouldBe`
                        "\
                        \( and \

                            \( and \
                                \( and \
                                    \( and \
                                        \( and \
                                            \true \
                                            \( and ( > time_C_0 10 ) ( and ( > cost_C_0 0 ) ( < cost_C_0 1 ) ) )\
                                        \ ) \
                                        \( and ( > time_S_0 10 ) ( and ( > cost_S_0 0 ) ( < cost_S_0 1 ) ) )\
                                    \ ) \
                                    \( and \
                                        \( and \
                                            \true \
                                            \( = cost ( + ( + ( + cost_C_0 cost_S_0 ) cost_C_3 ) cost_S_3 ) )\
                                        \ ) \
                                        \( = time ( + ( + ( + time_C_0 time_S_0 ) time_C_3 ) time_S_3 ) ) \
                                    \) \
                                \) \
                                \( and ( and ( > time_C_3 0 ) ( < time_C_3 5 ) ) ( > cost_C_3 1 ) )\
                            \ ) \
                            \( and ( and ( > time_S_3 0 ) ( < time_S_3 5 ) ) ( > cost_S_3 1 ) ) \
                        \)"

    describe "checkEntailment" $ do
        context "empty hypothesis and contingent consequent" $ do
            it "returns False" $ do
                let qosattrset = S.fromList ["time", "cost"]
                    consequent = SMTLIB.parseTerm "(> time 0)"
                res <- checkEntailment qosattrset SMTLIB.smtTrue consequent
                res `shouldBe` False
        context "empty hypothesis and tautology consequent" $ do
            it "returns True" $ do
                let qosattrset = S.fromList ["time", "cost"]
                    consequent = SMTLIB.parseTerm "(= time time)"
                res <- checkEntailment qosattrset SMTLIB.smtTrue consequent
                res `shouldBe` True
        context "non empty hypothesis and implied consequent" $ do
            it "returns True" $ do
                let qosattrset = S.fromList ["time", "cost"]
                    hypothesis = SMTLIB.parseTerm "(> time 10)"
                    consequent = SMTLIB.parseTerm "(> time 0)"
                res <- checkEntailment qosattrset hypothesis consequent
                res `shouldBe` True
        context "non empty hypothesis and non implied consequent" $ do
            it "returns False" $ do
                let attrNameSet = S.fromList ["time", "cost"]
                    hypothesis = SMTLIB.parseTerm "(> time 10)"
                    consequent = SMTLIB.parseTerm "(> cost 0)"
                res <- checkEntailment attrNameSet hypothesis consequent
                res `shouldBe` False

    let qosattrset = S.map (`QoSAttr` "+") $ S.fromList ["time", "cost"]
        qosMapC = M.fromList [("0", gammaLow), ("1", gammaLow), ("2", gammaLow), ("3", gammaHigh), ("4", gammaHigh), ("5", gammaHigh), ("6", gammaHigh), ("7", gammaHigh), ("8", gammaHigh)]
        qosMapS = M.fromList [("0", gammaLow), ("1", gammaLow), ("2", gammaLow), ("3", gammaHigh), ("4", gammaHigh), ("5", gammaHigh), ("6", gammaHigh), ("7", gammaHigh), ("8", gammaHigh)]
        finalStatesC = S.fromList ["2"]
        finalStatesS = S.fromList ["2"]
        qoscfsms = [(machineC, qosMapC, finalStatesC), (machineS, qosMapS, finalStatesS)]
        qossys = (qoscfsms, qosattrset, snd sys)
        finalStatesMapCS = M.fromList [("C", finalStatesC), ("S", finalStatesS)]
        qossys' :: QoSSystemComponents
        qossys' = (extractSystem qossys, qosattrset, buildQosMap qossys, finalStatesMapCS)

    describe "qModels" $ do
        context "atomic proposition formula" $ do
            it "returns correct answer" $ do
                res <- evalStateT (qModels T qossys' [] [] (initConf sys)) initialMem{gchorToPomset=computePomsetsOfGchors T}
                res `shouldBe` True
                res <- evalStateT (qModels (Not T) qossys' [] [] (initConf sys)) initialMem{gchorToPomset=computePomsetsOfGchors (Not T)}
                res `shouldBe` False
        -- context "atomic qos spec and empty run" $ do
        --     it "returns False" $ do
        --         let spec = SMTLIB.parseTerm "(> time 10)"
        --         res <- evalStateT (qModels (computePomsetsOfGchors (Spec spec)) (Spec spec) qossys' [] [] (initConf sys)) initialMem{gchorToPomset=}
        --         res `shouldBe` False
        context "atomic qos spec and run in which it holds" $ do
            it "returns True" $ do
                let spec = SMTLIB.parseTerm "(> time 20)"
                res <- evalStateT (qModels (Spec spec) qossys' singletonRun singletonRun (initConf sys)) initialMem{gchorToPomset=computePomsetsOfGchors (Spec spec)}
                res `shouldBe` True
        context "atomic qos spec and run in which it doesn't hold" $ do
            it "returns False" $ do
                let spec = SMTLIB.parseTerm "(> time 30)"
                res <- evalStateT (qModels (Spec spec) qossys' singletonRun singletonRun (initConf sys)) initialMem{gchorToPomset=computePomsetsOfGchors (Spec spec)}
                res `shouldBe` False
        context "And, Or, Spec, in non empty run" $ do
            it "returns correct answer" $ do
                let spec1 = SMTLIB.parseTerm "(> time 20)"
                    spec2 = SMTLIB.parseTerm "(> time 30)"
                    spec3 = SMTLIB.parseTerm "(> cost 1)"
                    spec4 = SMTLIB.parseTerm "(> cost 2)"

                    -- phi1 = And (Spec spec1) (Spec spec2)
                    -- phi2 = And (Spec spec1) (Spec spec3)
                    phi1 = Not (Or (Not (Spec spec1)) (Not (Spec spec2)))
                    phi2 = Not (Or (Not (Spec spec1)) (Not (Spec spec3)))
                    phi3 = Or (Spec spec2) (Spec spec4)
                    phi4 = Or (Spec spec2) (Spec spec3)

                res <- evalStateT (qModels phi1 qossys' singletonRun singletonRun (initConf sys)) initialMem{gchorToPomset=computePomsetsOfGchors phi1}
                res `shouldBe` False

                res <- evalStateT (qModels phi2 qossys' singletonRun singletonRun (initConf sys)) initialMem{gchorToPomset=computePomsetsOfGchors phi2}
                res `shouldBe` True

                res <- evalStateT (qModels phi3 qossys' singletonRun singletonRun (initConf sys)) initialMem{gchorToPomset=computePomsetsOfGchors phi3}
                res `shouldBe` False

                res <- evalStateT (qModels phi4 qossys' singletonRun singletonRun (initConf sys)) initialMem{gchorToPomset=computePomsetsOfGchors phi4}
                res `shouldBe` True

    describe "qSat'" $ do
        let qconfig = QSatConfig {
            kLow = 0,
            kUp = 8,
            unfoldingsNumber = 0,
            buffersBound = 8,
            QoS.Logic.verbose = False
            }
        context "True Until (eventually)" $ do
            it "returns correct answer" $ do
                let spec1 = SMTLIB.parseTerm "(> time 20)"
                    spec2 = SMTLIB.parseTerm "(> time 30)"
                    spec3 = SMTLIB.parseTerm "(> cost 1)"
                    spec4 = SMTLIB.parseTerm "(> cost 3)"
                    spec5 = SMTLIB.parseTerm "(> cost 4)"
                    spec6 = SMTLIB.parseTerm "(> cost 5)"

                    ( (gg1, _), _ ) = case GCParser.gcgrammar "C -> S : helo" (0,0) (0,0) of
                        GCParser.Ok x -> x
                        GCParser.Er s -> error s

                    ( (gg2, _), _ ) = case GCParser.gcgrammar "C -> S : helo; S -> C : nonexistent" (0,0) (0,0) of
                        GCParser.Ok x -> x
                        GCParser.Er s -> error s

                    ( (gg3, _), _ ) = case GCParser.gcgrammar "C -> S : helo; S -> C : int" (0,0) (0,0) of
                        GCParser.Ok x -> x
                        GCParser.Er s -> error s

                let phi1 = Until T gg1 (Spec spec1)
                res <- qSat' qconfig phi1 qossys
                isJust res `shouldBe` True

                let phi2 = Until T gg1 (Not (Or (Not (Spec spec1)) (Not (Spec spec3))))
                res <- qSat' qconfig phi2 qossys
                isJust res `shouldBe` True

                let phi3 = Until T gg1 (Spec spec2)
                res <- qSat' qconfig phi3 qossys
                isJust res `shouldBe` False

                let phi4 = Until T gg1 (Or (Spec spec2) (Spec spec4))
                res <- qSat' qconfig phi4 qossys
                isJust res `shouldBe` False

                let phi5 = Until T gg2 (Spec spec1)
                res <- qSat' qconfig phi5 qossys
                isJust res `shouldBe` False

                let phi6 = Until T gg2 (Spec spec2)
                res <- qSat' qconfig phi6 qossys
                isJust res `shouldBe` False

                let phi7 = Until T gg2 T
                res <- qSat' qconfig phi7 qossys
                isJust res `shouldBe` False

                let phi8 = Until T gg3 (Spec spec5)
                res <- qSat' qconfig phi8 qossys
                isJust res `shouldBe` True

                let phi9 = Until T gg3 (Spec spec6)
                res <- qSat' qconfig phi9 qossys
                isJust res `shouldBe` False

        let qosMapC = M.fromList [("0", gammaLow), ("1", gammaLow), ("2", gammaLow), ("3", gammaLow), ("4", gammaHigh), ("5", gammaHigh), ("6", gammaHigh), ("7", gammaHigh), ("8", gammaHigh)]
            qosMapS = M.fromList [("0", gammaLow), ("1", gammaLow), ("2", gammaLow), ("3", gammaHigh), ("4", gammaHigh), ("5", gammaHigh), ("6", gammaHigh), ("7", gammaHigh), ("8", gammaHigh)]
            qosattrset2 = S.map (`QoSAttr` "+") $ S.fromList ["time", "cost"]
            qoscfsms2 = [(fst sys !! 0, qosMapC, finalStatesC), (fst sys !! 1, qosMapS, finalStatesS)]
            qossys2 = (qoscfsms2, qosattrset2, snd sys)

        context "phi Until phi'" $ do
            it "returns correct answer" $ do
                let spec1 = SMTLIB.parseTerm "(> time 30)"
                    spec2 = SMTLIB.parseTerm "(> time 40)"
                    spec3 = SMTLIB.parseTerm "(< cost 3)"

                    ( (gg1, _), _ ) = case GCParser.gcgrammar "C -> S : helo" (0,0) (0,0) of
                        GCParser.Ok x -> x
                        GCParser.Er s -> error s

                    ( (gg2, _), _ ) = case GCParser.gcgrammar "C -> S : helo; S -> C : nonexistent" (0,0) (0,0) of
                        GCParser.Ok x -> x
                        GCParser.Er s -> error s

                    ( (gg3, _), _ ) = case GCParser.gcgrammar "C -> S : helo; S -> C : int" (0,0) (0,0) of
                        GCParser.Ok x -> x
                        GCParser.Er s -> error s

                let phi1 = Until (Spec spec3) gg1 (Spec spec1)
                res <- qSat' qconfig phi1 qossys2
                isJust res `shouldBe` True

                let phi2 = Until (Spec spec3) gg1 (Spec spec2)
                res <- qSat' qconfig phi2 qossys
                isJust res `shouldBe` False

        -- ToDo: add tests with gchors that repeat an event and, therefore, the machines pass more than once through the same state

    -- we no longer have the modelBounds function
    -- 
    -- describe "modelBounds" $ do
    --     let ( (gg1, _), _ ) = case GCParser.gcgrammar "C -> S : helo" (0,0) (0,0) of
    --             GCParser.Ok x -> x
    --             GCParser.Er s -> error s

    --         ( (gg2, _), _ ) = case GCParser.gcgrammar "C -> S : helo; S -> C : nonexistent" (0,0) (0,0) of
    --             GCParser.Ok x -> x
    --             GCParser.Er s -> error s

    --         ( (gg3, _), _ ) = case GCParser.gcgrammar "C -> S : helo; S -> C : int; C -> S : int" (0,0) (0,0) of
    --             GCParser.Ok x -> x
    --             GCParser.Er s -> error s

    --         spec1 = SMTLIB.parseTerm "(> time 30)"

    --     it "cases of length zero" $ do
    --         modelBounds T Nothing `shouldBe` (0,0)
    --         -- modelBounds F Nothing `shouldBe` (0,0)
    --         modelBounds (Spec spec1) Nothing `shouldBe` (0,0)
    --         -- modelBounds (And (Spec spec1) (Spec spec1)) Nothing `shouldBe` (0,0)
    --         modelBounds (Or T (Not T)) Nothing `shouldBe` (0,0)
    --         -- modelBounds (Or T F) Nothing `shouldBe` (0,0)
    --         modelBounds (Not T) Nothing `shouldBe` (0,0)
    --         modelBounds (Not (Spec spec1)) Nothing `shouldBe` (0,0)

    --     it "cases with one Until" $ do
    --         modelBounds (Until T gg1 T) Nothing `shouldBe` (2,2)
    --         modelBounds (Until T gg2 T) Nothing `shouldBe` (4,4)
    --         modelBounds (Until T gg3 T) Nothing `shouldBe` (6,6)

    --     it "cases with simple Untils combined with propositional operators" $ do
    --         modelBounds (Not (Until T gg1 T)) Nothing `shouldBe` (2,2)
    --         modelBounds (Or (Until T gg2 T) (Until T gg1 T)) Nothing `shouldBe` (4,4)
    --         modelBounds (Or (Until T gg1 T) (Until T gg2 T)) Nothing `shouldBe` (4,4)
    --         modelBounds (Or (Until T gg3 T) (Or (Until T gg1 T) (Until T gg2 T))) Nothing `shouldBe` (6,6)
    --         modelBounds (Or (Until T gg2 T) (Or (Until T gg1 T) (Until T gg3 T))) Nothing `shouldBe` (6,6)

    --     it "cases with nested Untils" $ do
    --         modelBounds (Not (Until (Until T gg1 T) gg1 T)) Nothing `shouldBe` (4,4)
    --         modelBounds (Not (Until (Until T gg1 T) gg1 (Until T gg1 T))) Nothing `shouldBe` (4,4)
    --         modelBounds (Not (Until (Until (Until T gg1 T) gg1 T) gg1 (Until T gg1 T))) Nothing `shouldBe` (6,6)
    --         modelBounds (Not (Until (Until (Until T gg1 T) gg1 T) gg2 (Until T gg3 T))) Nothing `shouldBe` (10,10)
    --         modelBounds (Not (Until (Until (Until T gg1 T) gg3 T) gg2 (Until T gg1 T))) Nothing `shouldBe` (12,12)
    --         modelBounds (Not (Until (Until T gg1 T) gg2 (Until (Until T gg1 T) gg3 T))) Nothing `shouldBe` (12,12)