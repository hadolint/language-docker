import System.Environment (getArgs)
import Language.Dockerfile.Parser
import Language.Dockerfile.PrettyPrint (prettyPrint)

main = do
    as <- getArgs
    case as of
        (f:_) -> do
            ed <- parseFile f
            case ed of
                Left e -> error ("Parse failure: " ++ show e)
                Right d -> putStrLn (prettyPrint d)
        _ -> error "Usage: dockerfmt <file>"
