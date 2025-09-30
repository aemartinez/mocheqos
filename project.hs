--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This program returns the fsa or dot format of projections of a
-- g-choreography or its restriction on a participant p while
-- maintaining the syntactic structure of the original
-- g-choreography. It is possible to select the participants onto
-- which projections must be calculated. A typical usage is
--
--    project <filename> -p A B C
--
-- where <filename> is the path to a .gc file with a choreography
-- having A, B, and C among its participants. If the -p option is
-- omitted, all the projections are returned.
-- 

import Misc
import DotStuff (getDotConf)
import GCParser
import CFSM (Ptp, cfsm2String, emptyCFSM, dottifyCFSM, printCFSM, prettyDotCFSM, getAcceptingStates, removeAcceptingTrxs)
import FSA (minimise,determinise)
import SyntacticGlobalChoreographies
import WellFormedness (filterPtp)
import System.Environment
import Data.Set (toList, empty)
import Data.List as L
import Data.Map.Strict as M
import QoS.CFSM ( buildQosMap, attrSet2String, qosmap2String )


filterPtps :: [Ptp] -> GC -> GC
filterPtps ps gc =
  -- restrict gc on the participants in ps...without splitting interactions
  case gc of
    Emp -> Emp
    Act (s,r) _ ->
      if L.elem s ps || L.elem r ps
      then gc
      else Emp
    Par gs -> Par (L.map (filterPtps ps) gs)
    Bra sel gs -> Bra sel (M.map (filterPtps ps) gs)
    Seq gs -> Seq (L.map (filterPtps ps) gs)
    Rep p' gc' -> Rep p' (filterPtps ps gc')

main :: IO ()
main = do
  progargs <- getArgs
  flines <- getDotConf
  if L.null progargs
    then do putStrLn $ usage PROJ
    else do
      let tmp =
            L.dropWhile (\x -> x /= "-p") progargs
      let ptp =
            if L.null tmp then [] else tail tmp
      let ( sourcefile, flags ) =
            getCmd PROJ (L.take (L.length progargs - L.length tmp) progargs)
          badFormat =
            error $ msgFormat PROJ ("unknown format " ++ (flags!"-fmt"))
      gctxt <- readFile sourcefile
      let ( (gc, names), outputQosAttr ) =
            case gcgrammar gctxt (0, 0) (0, 0) of
              Ok x -> x
              Er err -> error err
      let ptps = (Data.Set.toList names) ++ [""]
      let ptps_map =
            M.fromList $ L.zip (range $ L.length ptps) ptps
          handleND =
            case flags!"-D" of
              "min" ->
                (minimise . fst)
              "det" ->
                (determinise . fst)
              _ ->
                fst
          (pre, post) =
            case (flags!"-fmt") of
              "fsa" -> ("", "")
              "dot" -> ("digraph all {\n", "\n}")
              "gc" ->
                ("","")
              "GC" ->
                ("","")
              _ ->
                badFormat
          projection p = handleND $ proj gc "q0" (Data.Set.empty, Data.Set.empty) "qe" p ptps_map
          projs = (\p -> (p, projection p)) <$> ptps
          acceptingStatesStr = getAcceptingStates (L.map snd projs, ptps_map)
          projs' = [ if M.member "-qos" flags
                    then (p, removeAcceptingTrxs $ removeQosFromCFSM cfsm)
                    else (p, cfsm)
                    | (p, cfsm) <- projs]
          output =
            if ptp == [] -- all projections are returned
            then
              let cs =
                    case (flags!"-fmt") of
                      "fsa" -> 
                        L.map (uncurry cfsm2String) projs'
                      "dot" -> 
                        --L.map (\(p,s) -> "\nsubgraph " ++ p ++ "{\n label=\"" ++ p ++ "\"\n" ++ s ++ "\n}")
                        L.map (\(p, cfsm) -> CFSM.prettyDotCFSM cfsm p flines ptps_map) projs'
                      "GC" ->
                        [gc2txt 0 gc]
                      _ -> 
                        badFormat
              in
                L.foldr (++) "\n\n" cs
            else
              if flags!"-fmt" == "GC"
              then gc2txt 0 (filterPtps ptp gc)
              else
                let
                  aux p =
                    case (L.elemIndex p ptps) of
                      Nothing ->
                        case flags!"-fmt" of
                          "fsa" ->
                            CFSM.cfsm2String p emptyCFSM
                          "dot" ->
                            CFSM.prettyDotCFSM emptyCFSM p flines ptps_map
                          "gc" ->
                            gc2txt 0 Emp
                          "GC" ->
                            gc2txt 0 Emp
                          _ -> badFormat
                      Just _ ->
                        case (flags!"-fmt") of
                          "fsa" ->
                            CFSM.cfsm2String p (projection p)
                          "dot" ->
                            CFSM.prettyDotCFSM (projection p) p flines ptps_map
                          "gc" ->
                            gc2txt 0 (filterPtp p gc)
                          _ ->
                            badFormat
                in
                  (L.foldl (\x y -> x ++ "\n\n" ++ (aux y)) "" ptp)
      let qoscfsms = L.map getQoSCFSM (snd <$> projs)
          attrset = Data.Set.empty
          qossys = (qoscfsms, attrset, ptps_map)
          qosmap = buildQosMap qossys
          -- outputQosAttr = attrSet2String attrset
          outputQosMap = qosmap2String qosmap
      if (M.member "-qos" flags)
        then do putStrLn
                  ( "fsa{\n" ++ pre ++ output ++ post ++ "\n}\n" ++
                    "qos_attributes{\n" ++ outputQosAttr ++ "\n}\n" ++
                    "qos_specifications{\n" ++ outputQosMap ++ "\n}\n" ++
                    acceptingStatesStr
                  )
        else do putStrLn (pre ++ output ++ post)
      putStrLn $
        if (flags!"-a" == yes)
        then acceptingStatesStr
        else ""
      if (flags!"-v" == yes)
        then do mapM_ (\(k,v) -> putStrLn $ (show k) ++ " |--> " ++ (show v)) (M.toList ptps_map)
        else do putStrLn ""
