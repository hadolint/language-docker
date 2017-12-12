import Language.Docker
main = do
    ef <- parseFile "./Dockerfile"
    print ef
