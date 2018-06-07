import Language.Docker
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    output <- parseFile $ head args
    case output of
        Left err -> error (parseErrorPretty err)
        Right ast -> print (prettyPrintDockerfile ast)
