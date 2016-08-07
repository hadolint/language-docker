import Language.Dockerfile
main = do
    c <- readFile "./Dockerfile"
    print (parseString c)
