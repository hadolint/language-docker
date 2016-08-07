import Language.Dockerfile
main = do
    Right d <- parseFile "./Dockerfile"
    putStr (prettyPrint d)
