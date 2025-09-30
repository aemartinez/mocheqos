{
module QoS.CFSM.QoSMapLexer
  ( Token(..),
    Alex,
    alexMonadScan,
    runAlex,
  ) where

import Data.String.Utils(strip)
}

%wrapper "monad"

tokens :-

    $white+     ;
    @           { tok At }
    :           { tok Colon }
    \,          { tok Comma }
    [^@:\,]+    { tokString }

{
data Token
    = At
    | Colon
    | Comma
    | String String
    | EOF
    deriving (Eq, Show)

tok :: Token -> AlexAction Token
tok token inp len =
  pure token

tokString :: AlexAction Token
tokString inp@(_, _, _, str) len = pure $ String $ strip (take len str)

alexEOF :: Alex Token
alexEOF = pure EOF
}