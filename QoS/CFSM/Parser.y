{
module QoS.CFSM.Parser 
    ( parseQoSCFSM
    ) where

import QoS.CFSM.Lexer as Lex
import QoS.CFSM
import QoS.CFSM.QoSMapParser (parseQoSMap)
import CFSM (Ptp, State)
import Language.SMTLIB as SMTLIB
import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Char(isSpace)
import Data.List.Split as Split
import qualified SystemParser
}

%name parser
%tokentype { Lex.Token }
%error { parseError }
%monad { Lex.Alex } { >>= } { pure }
%lexer { lexer } { Lex.EOF }

%token
    fsa         { Lex.FSA $$ }
    qosattrs    { Lex.QoSAttrs $$ }
    qosspecs    { Lex.QoSSpecs $$ }
    finalstates { Lex.FinalStates $$ }

%%

Sys     : fsa qosattrs qosspecs finalstates        {parseComponents $1 $2 $3 $4}

{

parseComponents :: String -> String -> String -> String -> QoSSystem
parseComponents fsa qosattrs qosspecs finalstates =
    let (cfsms, ptps) = SystemParser.parseSystem ".fsa" fsa
        attrs = parseAttrs qosattrs
        emptySpecsMap = M.fromList [ (ptp, M.empty) | (id, ptp) <- M.toList ptps]
        specs = M.unionWith M.union emptySpecsMap (parseSpecs qosspecs)
        finalStatesMap = parseFinalStates finalstates
        qoscfsms = [(cfsms!!i, specs!(ptps!i), finalStatesMap!(ptps!i)) | i <- [0..length cfsms - 1]]
    in (qoscfsms, attrs, ptps)

parseAttrs :: String -> Set QoSAttr
parseAttrs qosattrs = S.map buildAttr $ S.fromList $ Split.splitOn "," $ Prelude.filter (not . isSpace) qosattrs
    where buildAttr str = QoSAttr (name str) (op str)
          name str = (Split.splitOn ":" str)!!0
          op str = (Split.splitOn ":" str)!!1

parseSpecs :: String -> Map Ptp (Map State QosAnnotation)
parseSpecs specs = qosmap
    where qosmap = case parseQoSMap specs of
                        Right m -> m
                        Left s -> error "Parse error"

parseFinalStates :: String -> Map Ptp (Set State)
parseFinalStates finalstates = 
    -- example of finalstates: "p1: [s1 s2 s3], p2: [s4 s5 s6]"
    M.fromList $ L.map buildFinalStateSet $ Split.splitOn "," finalstates
    where buildFinalStateSet str = (ptpName str, stateSet str)
          ptpName str = Prelude.filter (not . isSpace) $ (Split.splitOn ":" str)!!0
          stateSet str = S.fromList $ wordsBy isSpace $ takeWhile (/= ']') $ tail $ dropWhile (/= '[') $ (Split.splitOn ":" str)!!1

parseError :: Lex.Token -> a
parseError tok = error $ "Parse error at token " <> show tok <> ".\n"

lexer :: (Lex.Token -> Lex.Alex a) -> Lex.Alex a
lexer = (=<< Lex.alexMonadScan)

parseQoSCFSM :: String -> Either String QoSSystem
parseQoSCFSM str = Lex.runAlex str parser
}