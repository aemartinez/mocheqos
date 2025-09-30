{
module QoS.Logic.Parser 
    ( parseQL
    ) where

import QoS.Logic.Lexer as Lex
import QoS.Logic as QL
import QoS.CFSM (QoSSpec)
import Language.SMTLIB as SMTLIB
import SyntacticGlobalChoreographies (GC(Emp))
import qualified GCParser
import Data.Set as S
}

%name parser
%tokentype { Lex.Token }
%error { parseError }
%monad { Lex.Alex } { >>= } { pure }
%lexer { lexer } { Lex.EOF }

%token
    qos         { Lex.Qos $$ }
    gchor       { Lex.GChor $$ }
--     and         { Lex.And }
    or          { Lex.Or }
    until       { Lex.Until }
--     release     { Lex.Release }
    not         { Lex.Not }
    true        { Lex.Ttrue }
--     false       { Lex.False }
    lpar        { Lex.LPar }
    rpar        { Lex.RPar }

%%

Form    : lpar Form rpar                { $2 }
        | Prop                          { $1 }
        | Modal                         { $1 }
        | Atom                          { $1 }

Prop    : Form or Form                  { QL.Or $1 $3 }
        -- | Form and Form                 { QL.And $1 $3 }
        | not Form                      { QL.Not $2 }

Modal   : Form until gchor Form         { QL.Until $1 (parseGC $3) $4 }
        -- | Form release gchor Form       { QL.Release $1 (parseGC $3) $4 }

Atom    : true                          { QL.T }
        -- | false                         { QL.F }
        | qos                           { QL.Spec $ parseSpec $1 }

{

parseGC :: String -> GC
parseGC txt = gc
    where ((gc, _), _) = case GCParser.gcgrammar txt (0,0) (0,0) of
                        GCParser.Ok x -> x
                        GCParser.Er s -> error $ s <> ": " <> txt

parseSpec :: String -> QoSSpec
parseSpec txt = SMTLIB.parseTerm txt

parseError :: Lex.Token -> a
parseError _ = error "Parse error"

lexer :: (Lex.Token -> Lex.Alex a) -> Lex.Alex a
lexer = (=<< Lex.alexMonadScan)

parseQL :: String -> Either String QL
parseQL str = Lex.runAlex str parser
}
