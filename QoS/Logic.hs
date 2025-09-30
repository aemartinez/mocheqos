--
-- Authors: Agustin Martinez Sune <aemartinez@dc.uba.ar>
--
-- This module implements QoS Logic for the analysis of QoS extended CFSMs
--
--

module QoS.Logic where

import Control.Monad (foldM, liftM2, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy(StateT, evalStateT, get, put)
import Data.HashMap.Strict as HashM
import Data.HashSet as HashS
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Set as S

import qualified SimpleSMT

import CFSM
import FSA
import Language.SMTLIB as SMTLIB
import Language.SMTLIB.Util as SMTLIB
import Misc
import PomsetSemantics
import QoS.CFSM
import SyntacticGlobalChoreographies
import TS

data QL
    = T
    | Spec QoSSpec
    | Or QL QL
    | Until QL GC QL
    | Not QL
    deriving (Show, Eq)

data QSatConfig = QSatConfig
    { kLow :: Int
    , kUp :: Int
    , unfoldingsNumber :: Int
    , buffersBound :: Int
    , verbose :: Bool
    }

data MemorizedData = MemorizedData {
    gchorToPomset :: HashMap GC (Set Pomset, Int, Int),
    atomicEntailments :: Map (TS.Run, QoSSpec) Bool,
    knownTracesInGchor :: HashMap GC (HashSet Trace),
    knownTracesNotInGchor :: HashMap GC (HashSet Trace)
}
initialMem :: MemorizedData
initialMem = MemorizedData HashM.empty M.empty HashM.empty HashM.empty

naiveModelUpBound :: QL -> HashMap GC (Set Pomset, Int, Int) -> Int
naiveModelUpBound phi gchorToPomset =
    case phi of
        T -> 0
        Spec _ -> 0
        Or phi1 phi2 ->
            let u1 = naiveModelUpBound phi1 gchorToPomset
                u2 = naiveModelUpBound phi2 gchorToPomset
             in max u1 u2
        Until phi1 gc phi2 ->
            let u1 = naiveModelUpBound phi1 gchorToPomset
                u2 = naiveModelUpBound phi2 gchorToPomset
             in maxLength gc + max u1 u2
        Not phi -> naiveModelUpBound phi gchorToPomset
  where
    maxLength gc = case HashM.lookup gc gchorToPomset of
        Just (_, maxLength, _) -> maxLength
        Nothing -> error "Precomputed pomset not found."

qSat' :: QSatConfig -> QL -> QoSSystem -> IO (Maybe Run)
qSat' qconfig phi qossys = evalStateT (qSat qconfig phi qossys) initialMem

qSat :: QSatConfig -> QL -> QoSSystem -> StateT MemorizedData IO (Maybe Run)
qSat qconfig phi qossys = do
    mem <- get
    put mem {gchorToPomset = gchorToPomset'}
    go 0 (S.fromList [[]])
  where
    gchorToPomset' = computePomsetsOfGchors' (unfoldingsNumber qconfig) phi

    qosSysComps@(sys, _, _, _) = qosSysToQosSysComp qossys
    checkQModels :: TS.Run -> StateT MemorizedData IO (Maybe Run)
    checkQModels run = do
       if endsInFinalConf run
            then do
                modelsRes <- qModels phi qosSysComps run emptyRun (initConf sys)
                if modelsRes 
                    then pure $ Just run
                    else pure Nothing
            else pure Nothing

    go :: Int -> Set TS.Run -> StateT MemorizedData IO (Maybe Run)
    go i runSet
        | i < kLow qconfig = go (i + 1) $ S.unions (S.map extendRun runSet)
        | i <= kUp qconfig = do
            when (verbose qconfig) $ do
                liftIO $ putStrLn $ verboseInfo i runSet
                liftIO $ putStrLn ""
            -- pb <- newProgressBar defStyle 10 (Progress 0 (S.size runSet) ())
            res <-
                foldM
                    ( \b a ->
                        case b of
                            Just run -> pure $ Just run
                            Nothing -> checkQModels a
                    )
                    Nothing
                    runSet
            case res of
                Just run -> pure $ Just run
                Nothing ->
                    if S.null runSet
                        then pure Nothing
                        else go (i + 1) $ S.unions (S.map extendRun $ filterRunSet i runSet)
        | otherwise = pure Nothing

    verboseInfo :: Int -> Set TS.Run -> String
    verboseInfo i runSet = "Checking models of length: " ++ show i ++ "\n\tNumber of models: " ++ show (S.size runSet) ++ "\n\tNumber of models ending in a final conf: " ++ show (S.size $ S.filter endsInFinalConf runSet)

    filterRunSet :: Int -> Set TS.Run -> Set TS.Run
    filterRunSet i
        --  If the model upper bound has been reached we can stop extending runs that end in a final configuration
        | i < naiveModelUpBound phi gchorToPomset' = id
        | otherwise = S.filter (not . endsInFinalConf)

    extendRun :: TS.Run -> Set TS.Run
    extendRun run = S.map (run ++) ktransitions
      where
        conf = if L.null run then initConf sys else get3rd (last run)
        ktransitions = S.map (\(kevt, conf') -> [(conf, kevt, conf')]) $ nextConfs conf
    nextConfs = TS.step (buffersBound qconfig) True sys
    endsInFinalConf run = case run of
        [] -> False
        _ -> isFinalConf qosSysComps $ get3rd (last run)

get3rd :: (a, b, c) -> c
get3rd (_, _, c) = c

-- get2nd :: (a, b, c) -> b
-- get2nd (_,c,_) = c

get1st :: (a, b, c) -> a
get1st (c, _, _) = c

qModels :: QL -> QoSSystemComponents -> TS.Run -> TS.Run -> TS.Configuration -> StateT MemorizedData IO Bool
qModels phi qossys@(sys, qosattrs, qosmaps, finalStatesMap) run run' conf =
    case phi of
        T -> return True
        Spec phi -> checkEntailment' phi qossys run' conf
        Not phi1 -> fmap not (qModels phi1 qossys run run' conf)
        Or phi1 phi2 -> do
            resPhi1 <- qModels phi1 qossys run run' conf
            if resPhi1 then pure True else qModels phi2 qossys run run' conf
        Until phi1 gchor phi2 -> qUntil (phi1, gchor, phi2) qossys run run' emptyRun conf

qUntil :: (QL, GC, QL) -> QoSSystemComponents -> TS.Run -> TS.Run -> TS.Run -> TS.Configuration -> StateT MemorizedData IO Bool
qUntil (phi1, gchor, phi2) qossys@(sys, _, _, finalStatesMap) run run' run'' conf = do
    mem <- get
    isInGChor <- isTraceInGchor (traceOf run'') gchor 
    if isInGChor
        then do
            phi2IsSat <- qModels phi2 qossys run (run' ++ run'') conf
            if phi2IsSat then pure True else recursiveCase
        else recursiveCase
  where
    extendedRun = extendByOne run run' run''
    (_, _, conf') = last extendedRun
    -- isPrefixInLanguage =
    --     any (isTraceInPomsetPrefix $ traceOf extendedRun) (get1st pomsetsAndLength)
    recursiveCase = do
        if run == run' ++ run'' -- || not isPrefixInLanguage
            then pure False 
            else do phi1IsSat <- qModels phi1 qossys run (run' ++ run'') conf
                    if not phi1IsSat
                        then pure False
                        else qUntil (phi1, gchor, phi2) qossys run run' extendedRun conf'

isTraceInGchor :: Trace -> GC -> StateT MemorizedData IO Bool
isTraceInGchor trace gc = do
    mem <- get
    let pomsetsAndLength = fromMaybe (error "Precomputed pomset not found.") $ HashM.lookup gc (gchorToPomset mem)
        isInGChor = any (isTraceInPomset trace) (get1st pomsetsAndLength)
    knownInGChor <- case HashM.lookup gc (knownTracesInGchor mem) of
                    Just traceSet -> pure $ HashS.member trace traceSet
                    Nothing -> pure False
    if knownInGChor
        then pure True
        else do 
            knownNotInGchor <- case HashM.lookup gc (knownTracesNotInGchor mem) of
                    Just traceSet -> pure $ HashS.member trace traceSet
                    Nothing -> pure False
            if knownNotInGchor
                then pure False
                else if isInGChor
                    then do
                        put mem {knownTracesInGchor = HashM.insertWith HashS.union gc (HashS.singleton trace) (knownTracesInGchor mem)}
                        pure True
                    else do
                        put mem {knownTracesNotInGchor = HashM.insertWith HashS.union gc (HashS.singleton trace) (knownTracesNotInGchor mem)}
                        pure False

extendByOne :: TS.Run -> TS.Run -> TS.Run -> TS.Run
extendByOne run run' run''
    | L.length run' + L.length run'' < L.length run = run'' ++ [run !! (L.length run' + L.length run'')]
    | otherwise = error "run is not longer than run' + run''"

isFinalConf :: QoSSystemComponents -> TS.Configuration -> Bool
isFinalConf (sys@(cfsms, ptps), _, _, finalStatesMap) conf@(nod, buf) = L.and [S.member (nod !! i) (finalStatesMap M.! (ptps M.! i)) | i <- [0 .. L.length cfsms - 1]]

gchorsInFormula :: QL -> Set GC
gchorsInFormula ql =
    case ql of
        Until psi gchor psi' -> S.insert gchor $ S.union (gchorsInFormula psi) (gchorsInFormula psi')
        Or psi psi' -> S.union (gchorsInFormula psi) (gchorsInFormula psi')
        Not psi -> gchorsInFormula psi
        _ -> S.empty

computePomsetsOfGchors :: QL -> HashMap GC (Set Pomset, Int, Int)
computePomsetsOfGchors = computePomsetsOfGchors' 0

computePomsetsOfGchors' :: Int -> QL -> HashMap GC (Set Pomset, Int, Int)
computePomsetsOfGchors' unfoldingsNumber ql = S.foldr (\gc -> HashM.insert gc (computePomsets unfoldingsNumber gc)) HashM.empty (gchorsInFormula ql)

computePomsets :: Int -> GC -> (Set Pomset, Int, Int)
computePomsets unfoldingsNumber gc = (pomsets, maxLengthOfLanguage pomsets, minLengthOfLanguage pomsets)
  where
    pomsets = S.unions (S.map (\gc' -> fst (pomsetsOf gc' 0 0)) (unfoldGCUpTo unfoldingsNumber gc))

aggregatedAttrsSet :: System -> Set QoSAttr -> Map (Ptp, State) QosAnnotation -> Set AttributeName
aggregatedAttrsSet sys@(cfsms, ptps) qosattrs qosmaps =
    S.union attrNameSet $ S.unions [aggregatedAttrsPtp (cfsms !! i, ptps M.! i) attrNameSet | i <- [0 .. length cfsms - 1]]
  where
    attrNameSet = S.map attrName qosattrs

aggregatedAttrsPtp :: (CFSM, Ptp) -> Set AttributeName -> Set AttributeName
aggregatedAttrsPtp (cfsm, ptp) qosattrs = S.unions $ S.map (renameAttrsSet qosattrs ptp) (statesOf cfsm)

renameAttrsSet :: Set AttributeName -> Ptp -> State -> Set AttributeName
renameAttrsSet attrNameSet ptp state = S.map rename attrNameSet
  where
    rename attr = attr ++ "_" ++ ptp ++ "_" ++ state

aggregateOneConf :: System -> Set QoSAttr -> Map (Ptp, State) QosAnnotation -> TS.Configuration -> QoSSpec
aggregateOneConf sys qosattrs qosmaps conf =
    let ptpStateList = getStatesFromConf sys conf
        filteredPtpStateSet = L.filter (hasQosSpec qosmaps) ptpStateList
     in joinSpecs $ S.insert (aggregationAxioms qosattrs filteredPtpStateSet) (collectLocalSpecs qosattrs qosmaps $ S.fromList filteredPtpStateSet)

aggregation :: System -> Set QoSAttr -> Map (Ptp, State) QosAnnotation -> Run -> QoSSpec
aggregation sys qosattrs qosmaps run =
    let ptpStateList = getStatesFromRun sys run
        filteredPtpStateSet = L.filter (hasQosSpec qosmaps) ptpStateList
     in joinSpecs $ S.insert (aggregationAxioms qosattrs filteredPtpStateSet) (collectLocalSpecs qosattrs qosmaps $ S.fromList filteredPtpStateSet)

hasQosSpec :: Map (Ptp, State) QosAnnotation -> (Ptp, State) -> Bool
hasQosSpec qosmaps (ptp, state) = isJust qosannotation
  where
    qosannotation = case M.lookup (ptp, state) qosmaps of
        Just e -> e
        Nothing -> error "(ptp, state) has no QosAnnotation associated"

collectLocalSpecs :: Set QoSAttr -> Map (Ptp, State) QosAnnotation -> Set (Ptp, State) -> Set QoSSpec
collectLocalSpecs attrSet qosmaps = S.map fromJust . S.filter isJust . S.map (getQoSSpec attrSet qosmaps)

getQoSSpec :: Set QoSAttr -> Map (Ptp, State) QosAnnotation -> (Ptp, State) -> QosAnnotation
getQoSSpec attrSet qosmaps (ptp, state) = renameAttrs attrSet ptp state <$> qosannotation
  where
    qosannotation = case M.lookup (ptp, state) qosmaps of
        Just e -> e
        Nothing -> error "(ptp, state) has no QosAnnotation associated"

renameAttrs :: Set QoSAttr -> Ptp -> State -> QoSSpec -> QoSSpec
renameAttrs attrSet ptp state = SMTLIB.mapS renameIfAttr
  where
    renameIfAttr :: String -> String
    renameIfAttr s = if S.member s (S.map attrName attrSet) then s ++ sufix else s
    sufix = "_" ++ ptp ++ "_" ++ state

aggregationAxioms :: Set QoSAttr -> [(Ptp, State)] -> QoSSpec
aggregationAxioms attrSet ptpStateList
    | L.null ptpStateList = SMTLIB.smtTrue
    | otherwise = S.foldl f SMTLIB.smtTrue attrSet
  where
    f trm attr = SMTLIB.smtAND trm $ equalToCompositionTerm attr ptpStateList

equalToCompositionTerm :: QoSAttr -> [(Ptp, State)] -> SMTLIB.Term
equalToCompositionTerm (QoSAttr name op) ptpStateList =
    -- PRE: ptpStateList is not null
    SMTLIB.smtEq (SMTLIB.smtConstName name) $ L.foldl writeSum firstAttrTrm (tail attrList)
  where
    attrList = L.map (\(ptp, state) -> name ++ "_" ++ ptp ++ "_" ++ state) ptpStateList
    writeSum trm att = SMTLIB.smtBinOp op trm (SMTLIB.smtConstName att)
    firstAttrTrm = SMTLIB.smtConstName $ head attrList

checkEntailment' :: QoSSpec -> QoSSystemComponents -> TS.Run -> TS.Configuration -> StateT MemorizedData IO Bool
checkEntailment' phi qossys@(sys, qosattrs, qosmaps, finalStatesMap) run' conf = do
    mem <- get
    case M.lookup (run', phi) (atomicEntailments mem) of
        Just b -> pure b
        Nothing -> do
            res <- case run' of
                        [] -> liftIO $ checkEntailment (aggregatedAttrsSet sys qosattrs qosmaps) (aggregateOneConf sys qosattrs qosmaps conf) phi
                        _ -> liftIO $ checkEntailment (aggregatedAttrsSet sys qosattrs qosmaps) (aggregation sys qosattrs qosmaps run') phi
            put mem {atomicEntailments = M.insert (run', phi) res (atomicEntailments mem)}
            pure res

checkEntailment :: Set AttributeName -> QoSSpec -> QoSSpec -> IO Bool
checkEntailment attrs term term' =
    let smtScript = buildScript attrs (SMTLIB.smtAND term (SMTLIB.smtNOT term'))
     in do
            res <- runSolver smtScript
            case res of
                SimpleSMT.Unknown -> error "SMT solver returned unknown"
                SimpleSMT.Sat -> pure False
                SimpleSMT.Unsat -> pure True

buildScript :: Set AttributeName -> SMTLIB.Term -> SMTLIB.Script
buildScript attrNameSet trm = SMTLIB.Script $ declareAttrs ++ extraDecls ++ [SMTLIB.Assert trm] ++ [SMTLIB.Check_sat]
  where
    declareAttrs = S.foldl (\cmds attr -> declareConst attr : cmds) [] attrNameSet
    declareConst attr = SMTLIB.Declare_const attr (SMTLIB.Sort_identifier (SMTLIB.Identifier "Real"))
    extraDecls =
        SMTLIB.parseCommands
            "(define-fun max ((x Real) (y Real)) Real\
            \(ite (< x y) y x))\
            \(define-fun min ((x Real) (y Real)) Real\
            \(ite (< x y) x y))"

runSolver :: SMTLIB.Script -> IO SimpleSMT.Result
runSolver script = do
    -- print script
    solver <- SimpleSMT.newSolver "z3" ["-in"] Nothing
    SimpleSMT.loadString solver (show script)
    res <- SimpleSMT.check solver
    SimpleSMT.stop solver
    pure res
