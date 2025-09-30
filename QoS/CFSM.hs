--
-- Authors: Agustin Martinez Sune <aemartinez@dc.uba.ar>
--
-- This module implements QoS extended CFSMs
--
--

module QoS.CFSM where

import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Set as S

import CFSM
import Language.SMTLIB as SMTLIB
import Language.SMTLIB.Util as SMTLIB
import Misc

type AttributeName = String

data QoSAttr = QoSAttr
    { attrName :: AttributeName
    , attrAggregationOp :: String
    } deriving (Show, Eq, Ord)

-- A QoSSpec is an SMT-LIB Term, its free symbols are names of QoS attributes.
type QoSSpec = SMTLIB.Term

type QosAnnotation = Maybe QoSSpec

-- A QoSCFSM is a CFSM with QoS annotations on states, and
-- a set of final states.
type QoSCFSM = (CFSM, Map State QosAnnotation, Set State)
type QoSSystem = ([QoSCFSM], Set QoSAttr, P)

-- A different representation of a QoSSystem, for convenience
type QoSSystemComponents = (System, Set QoSAttr, Map (Ptp, State) QosAnnotation, Map Ptp (Set State))

extractSystem :: QoSSystem -> System
extractSystem (qoscfsms, _, ptps) = (L.map getCFSM qoscfsms, ptps)

qosSysToQosSysComp :: QoSSystem -> QoSSystemComponents
qosSysToQosSysComp qosSys@(qoscfsms, qosattrs, ptps) = (extractSystem qosSys, qosattrs, buildQosMap qosSys, buildFinalStatesMap qosSys)
    where buildFinalStatesMap :: QoSSystem -> Map Ptp (Set State)
          buildFinalStatesMap (qoscfsms, _, ptps) = M.fromList $ [ (ptps!i, getFinalStates (qoscfsms!!i)) | i <- [0..L.length qoscfsms -1] ]

getCFSM :: QoSCFSM -> CFSM
getCFSM (cfsm, _, _) = cfsm

getQosMap :: QoSCFSM -> Map State QosAnnotation
getQosMap (_, qosmap, _) = qosmap

getFinalStates :: QoSCFSM -> Set State
getFinalStates (_, _, finalstates) = finalstates

buildQosMap :: QoSSystem -> Map (Ptp, State) QosAnnotation
buildQosMap (qoscfsms, _, ptps) = M.unions [ buildmap (getQosMap (qoscfsms!!i)) (ptps!i) | i <- [0..L.length qoscfsms -1], (ptps!i) /= "" ]
    where buildmap :: Map State QosAnnotation -> Ptp -> Map (Ptp, State) QosAnnotation
          buildmap qosmap ptp = M.mapKeys (\s -> (ptp, s)) qosmap

joinSpecs :: Set QoSSpec -> QoSSpec
joinSpecs = S.foldl SMTLIB.smtAND emptySpec

negateSpec :: QoSSpec -> QoSSpec
negateSpec = SMTLIB.smtNOT

emptySpec :: QoSSpec
emptySpec = SMTLIB.smtTrue

attrSet2String :: Set QoSAttr -> String
attrSet2String = L.intercalate ",\n" . L.map (\a -> attrName a ++ ":" ++ attrAggregationOp a) . S.toList

qosmap2String :: Map (Ptp, State) QosAnnotation -> String
qosmap2String qosmap = L.intercalate ",\n" $ L.map (\((ptp, s), qos) -> ptp ++ "@" ++ s ++ " : " ++ showQos qos) $ L.filter (\(_, qosAnn) -> isJust qosAnn) (M.toList qosmap)
    where showQos :: QosAnnotation -> String
          showQos Nothing = "Irrelevant"
          showQos (Just spec) = show spec

ignoreNonexistentQoS :: QoSSystem -> QoSSystem
ignoreNonexistentQoS (qoscfsms, qosattrs, ptps) = (L.map ignoreNonQoS qoscfsms, qosattrs, ptps)
    where ignoreNonQoS :: QoSCFSM -> QoSCFSM
          ignoreNonQoS (cfsm, qosmap, finalstates) = (cfsm, qosmap', finalstates)
              where qosmap' = M.union qosmap (M.fromList $ L.map (\s -> (s, Nothing)) $ S.toList $ S.difference (statesOf cfsm) (M.keysSet qosmap))