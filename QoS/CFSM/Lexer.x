{
module QoS.CFSM.Lexer
  ( Token(..),
    Alex,
    alexMonadScan,
    runAlex,
  ) where
}

%wrapper "monad"

tokens :-

    $white+                                 ;
    fsa\{([^\}]|[\n])*\}                    { tokFSA }
    qos_attributes\{([^\}]|[\n])*\}         { tokQoSAttrs }
    qos_specifications\{([^\}]|[\n])*\}     { tokQoSSpecs }
    final_states\{([^\}]|[\n])*\}           { tokFinalStates }

{
data Token
    = FSA String
    | QoSAttrs String
    | QoSSpecs String
    | FinalStates String
    | EOF
    deriving (Eq, Show)

tokFSA :: AlexAction Token
tokFSA inp@(_, _, _, str) len = 
    pure $ FSA $ drop 4 $ init (take len str)

tokQoSAttrs :: AlexAction Token
tokQoSAttrs inp@(_, _, _, str) len = 
    pure $ QoSAttrs $ drop (length "qos_attributes{") $ init (take len str)

tokQoSSpecs :: AlexAction Token
tokQoSSpecs inp@(_, _, _, str) len = 
    pure $ QoSSpecs $ drop (length "qos_specifications{") $ init (take len str)

tokFinalStates :: AlexAction Token
tokFinalStates inp@(_, _, _, str) len = 
    pure $ FinalStates $ drop (length "final_states{") $ init (take len str)

alexEOF :: Alex Token
alexEOF = pure EOF
}