{
module QoS.CFSM.QoSMapParser 
    ( parseQoSMap
    ) where

import QoS.CFSM.QoSMapLexer as Lex
import QoS.CFSM
import CFSM (Ptp, State)
import Data.Set as S
import Data.Map as M
import Language.SMTLIB as SMTLIB
}

%name parser
%tokentype { Lex.Token }
%error { parseError }
%monad { Lex.Alex } { >>= } { pure }
%lexer { lexer } { Lex.EOF }

%token
    at          { Lex.At }
    colon       { Lex.Colon }
    comma       { Lex.Comma }
    string      { Lex.String $$ }

%%

Map     : Elem                          { $1 }
        | Elem comma Map                { mergeMaps $1 $3 }

Elem    : string at string colon string { parseElem $1 $3 $5}

{

parseElem :: String -> String -> String -> Map Ptp (Map State QosAnnotation)
parseElem ptp state spec = M.fromList [(ptp, M.fromList [(state, parseSpec spec)])]

mergeMaps :: Map Ptp (Map State QosAnnotation) -> Map Ptp (Map State QosAnnotation) -> Map Ptp (Map State QosAnnotation)
mergeMaps m1 m2 = M.unionWith M.union m1 m2

parseSpec :: String -> QosAnnotation
parseSpec txt
    | txt == "Irrelevant" = Nothing
    | otherwise = Just (SMTLIB.parseTerm txt)

parseError :: Lex.Token -> a
parseError tok = error $ "Parse error at token " <> show tok <> ".\n"

lexer :: (Lex.Token -> Lex.Alex a) -> Lex.Alex a
lexer = (=<< Lex.alexMonadScan)

parseQoSMap :: String -> Either String (Map Ptp (Map State QosAnnotation))
parseQoSMap str = Lex.runAlex str parser
}