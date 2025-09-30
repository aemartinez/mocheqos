--
-- Authors: Agustín Martinez Suñé <aemartinez@dc.uba.ar>
--
-- This program reads a QoS extended Communicating System, a QL formula, and
-- outputs whether the formula is satisfiable/valid in the system.
--

import Control.Monad (when)
import qualified Data.Foldable as L
import Data.Maybe (fromMaybe)
import Data.Set as S
import Misc
import Options.Applicative as O
import QoS.CFSM
import QoS.CFSM.Parser (parseQoSCFSM)
import QoS.Logic
import QoS.Logic.Parser (parseQL)
import System.Environment
import TS (initConf, traceOf, showTrace)
import Control.Monad.Trans.RWS (put)

data Subcommand
    = Satisfiability FilePath FilePath Int (Maybe Int) Bool Bool
    | Validity FilePath FilePath Int (Maybe Int) Bool Bool
    -- = Satisfiability FilePath FilePath Int (Maybe Int) (Maybe Int) Bool Bool
    -- | Validity FilePath FilePath Int (Maybe Int) (Maybe Int) Bool Bool
    deriving (Eq, Show)

data SubcommandCase = SatisfiabilityCase | ValidityCase
    deriving (Eq, Show)

subParser :: SubcommandCase -> Parser Subcommand
subParser scase =
    subcommand
        <$> argument str (metavar "sys.qosfsa")
        <*> argument str (metavar "prop.ql")
        <*> argument auto (metavar "k-bound")
        <*> optional (option auto (long "unfoldings" <> short 'u' <> metavar "N"))
        -- <*> optional (option auto (long "buffers-bound" <> short 'b' <> metavar "B"))
        <*> switch (long "verbose" <> short 'v')
        <*> switch (long "show-model")
  where
    subcommand = case scase of
        SatisfiabilityCase -> Satisfiability
        ValidityCase -> Validity

satParser :: Parser Subcommand
satParser = subParser SatisfiabilityCase

validityParser :: Parser Subcommand
validityParser = subParser ValidityCase

subcommandParser :: Parser Subcommand
subcommandParser =
    subparser
        ( command "satisfiability" (O.info (O.helper <*> satParser) (progDesc "Check for satisfiability within the given bounds."))
            <> command "validity" (O.info (O.helper <*> validityParser) (progDesc "Check for validity within the given bounds."))
        )

parseFiles :: String -> String -> IO (QoSSystem, QL)
parseFiles sysfile qlfile = do
    qossystxt <- readFile sysfile
    qltxt <- readFile qlfile
    let ql = case parseQL qltxt of
            Right x -> x
            Left x -> error x
        qossys = case parseQoSCFSM qossystxt of
            Right x -> x
            Left x -> error x
    return (qossys, ql)

runParser :: ParserInfo a -> IO a
runParser = customExecParser (prefs $ showHelpOnEmpty <> showHelpOnError)

main :: IO ()
main = do
    subcommand <- runParser parserInfo
    case subcommand of
        Satisfiability sysfile qlfile k mbU verbose showModel -> do
            (qossys, phi) <- parseFiles sysfile qlfile
            let qossys' = ignoreNonexistentQoS qossys
                unfoldingsNumber = fromMaybe k mbU
                buffersBound = k
                qconfig = QSatConfig 0 k unfoldingsNumber buffersBound verbose
            res <- qSat' qconfig phi qossys'
            case res of 
                Just run -> putStrLn "Satisfiable." >> when showModel (putStrLn $ "\nModel: " ++ showTrace (traceOf run))
                Nothing -> putStrLn "No model found within the given bounds."
        Validity sysfile qlfile k mbU verbose showModel -> do
            (qossys, phi) <- parseFiles sysfile qlfile
            let qossys' = ignoreNonexistentQoS qossys
                unfoldingsNumber = fromMaybe k mbU
                buffersBound = k
                qconfig = QSatConfig 0 k unfoldingsNumber buffersBound verbose
            isSat <- qSat' qconfig (Not phi) qossys'
            case isSat of
                Just run -> putStrLn "Not valid." >> when showModel (putStrLn $ "\nCounterexample: " ++ showTrace (traceOf run))
                Nothing -> putStrLn "No counterexample found within the given bounds."
  where
    parserInfo :: ParserInfo Subcommand
    parserInfo =
        O.info
            (O.helper <*> subcommandParser)
            ( fullDesc
                <> progDesc "Check (k-bounded) satisfiability or validity of a QL formula in a QoS extended Communicating System"
                <> header "MoCheQoS - Automated Analysis of Quality of Service properties of Communicating Systems"
            )
