{
module QoS.Logic.Lexer
  ( Token(..),
    Alex,
    alexMonadScan,
    runAlex,
  ) where

/* import Prelude hiding(True, False) */
}

%wrapper "monad"

tokens :-

    $white+         ;
    -- "And"           { tok And }
    -- "∧"             { tok And }
    "Or"            { tok Or }
    "v"             { tok Or }
    "∨"             { tok Or }
    "Until"         { tok Until }
    "U"             { tok Until }
    -- "Release"       { tok Release }
    -- "R"             { tok Release }
    "Not"           { tok Not }
    "¬"             { tok Not }
    "True"          { tok Ttrue }
    -- "False"         { tok False }
    "("             { tok LPar }
    ")"             { tok RPar }
    qos\{[^\}]*\}   { tokQoS }
    
    \[
      (
        [^\]]
        |
        \n
      )*
    \]      { tokGChor }

{
data Token
    = Qos String
    | GChor String
    -- | And
    | Or
    | Until
    -- | Release
    | Not
    | Ttrue
    -- | False
    | LPar
    | RPar
    | EOF
    deriving (Eq, Show)

tok :: Token -> AlexAction Token
tok token inp len =
  pure token

tokQoS :: AlexAction Token
tokQoS inp@(_, _, _, str) len = 
    pure $ Qos $ drop 4 $ init (take len str)

tokGChor :: AlexAction Token
tokGChor inp@(_, _, _, str) len = 
    pure $ GChor $ tail $ init (take len str)

alexEOF :: Alex Token
alexEOF = pure EOF
}
