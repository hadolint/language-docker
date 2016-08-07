import Language.Dockerfile
main = do
    ef <- parseFile "./Dockerfile"
    print ef
