import Language.Docker
main = do
    Right d <- parseFile "./Dockerfile"
    putStr (prettyPrint d)
