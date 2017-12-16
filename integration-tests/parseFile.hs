import Language.Docker
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    output <- parseFile $ head args
    print output
