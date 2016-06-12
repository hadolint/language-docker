module Main where

import           Data.List                       (sort)
import           System.Environment              (getArgs)
import           System.Exit                     hiding (die)
import           System.IO                       (hPrint, hPutStrLn, stderr)
import           Text.Parsec                     (ParseError)

import           Language.Dockerfile.FormatCheck
import           Language.Dockerfile.Parser
import           Language.Dockerfile.PrettyPrint (prettyPrint)
import           Language.Dockerfile.Rules
import           Language.Dockerfile.Syntax

printChecks :: [Check] -> IO ()
printChecks checks = do
    mapM_ (putStrLn . formatCheck) $ sort checks
    if null checks then exit else die

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-i"] = do
    content <- getContents
    length content `seq` return ()
    if not (null content)
    then checkAst $ parseString content
    else usage >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [file] = parseFile file >>= checkAst
parse ["-f", file] = do
    ed <- parseFile file
    case ed of
      Right d -> putStrLn (prettyPrint d)
      Left e -> do
          hPutStrLn stderr "Parse Failure:"
          hPrint stderr (show e)
          die
parse _ = usage >> die

checkAst :: Either ParseError Dockerfile -> IO ()
checkAst ast = case ast of
    Left err -> print err >> die
    Right df -> printChecks (analyzeAll df)

analyzeAll :: Dockerfile -> [Check]
analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither :: Either t Dockerfile -> [Check]
analyzeEither (Left _) = []
analyzeEither (Right df)  = analyzeAll df

usage :: IO ()
usage = putStrLn "Usage: hadolint [-vhif] <file>"

version :: IO ()
version = putStrLn "Haskell Dockerfile Linter v1.0"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitWith (ExitFailure 1)
