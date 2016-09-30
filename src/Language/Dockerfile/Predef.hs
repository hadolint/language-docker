{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Dockerfile.Predef
  where

import           Control.Monad
import           Control.Monad.Free.Class
import           Control.Monad.IO.Class
import           Data.Aeson                     (Value (..))
import qualified Data.HashMap.Strict            as HashMap
import           Data.Maybe
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.IO                   as Text
import qualified Data.Yaml                      as Yaml
import           System.Directory
import           System.FilePath
import qualified System.FilePath.Glob           as Glob

import           Language.Dockerfile
import           Language.Dockerfile.EDSL.Types

appendLnIfMissing :: FilePath -> Text -> IO ()
appendLnIfMissing fp cts = do
    e <- doesFileExist fp
    unless e (Text.writeFile fp "")
    txt <- Text.lines <$> Text.readFile fp
    unless (cts `elem` txt) $
        Text.writeFile fp (Text.unlines (txt <> [cts]))

dockerIgnore :: Text -> IO ()
dockerIgnore = appendLnIfMissing ".dockerignore"

addGlob
  :: (MonadIO m, MonadFree EInstruction m) =>
     String -> Destination -> m ()
addGlob pattern dest = do
    fs <- liftIO $ do
        fs <- Glob.glob pattern
        cwd <- getCurrentDirectory
        let fs' = map (makeRelative cwd . normalise) fs
        forM fs' $ \f -> do
             isdir <- doesDirectoryExist f
             return $ if isdir
                 then (f <> "/", dest <> takeBaseName f)
                 else (f, dest <> takeBaseName f)
    forM_ fs (uncurry add)

copyGlob
  :: (MonadIO m, MonadFree EInstruction m) =>
     String -> Destination -> m ()
copyGlob = addGlob

stackBuild
  :: (Monad m, MonadIO m,
      MonadFree EInstruction m) =>
     m ()
stackBuild = do
    sts <- liftIO getStackYamlResolver
    stackBuild' sts (return ())
  where
    getStackYamlResolver = do
        mhm <- Yaml.decodeFile "./stack.yaml" :: IO (Maybe Value)
        return $ fromMaybe "latest" $ do
            hm <- mhm
            o <- case hm of
                Object o -> return o
                _ -> Nothing
            rs <- HashMap.lookup "resolver" o
            toString rs
      where
        toString (String m) = Just (Text.unpack m)
        toString _ = Nothing

stackBuild'
  :: (Monad m, MonadIO m,
      MonadFree EInstruction m) =>
     String -> m () -> m ()
stackBuild' tag extra = do
    liftIO $ dockerIgnore ".stack-work"
    liftIO $ dockerIgnore ".cabal-sandbox"

    from ("fpco" `tagged` tag)
    extra
    add "./package.yaml" "/app/package.yaml"
    addGlob "./*.cabal" "/app/"
    add "./stack.yaml" "/app/stack.yaml"
    workdir "/app/"
    run "stack build --only-dependencies"
    add "." "/app/stack.yaml"
    run "stack build"

nodejs
  :: (Monad m, MonadIO m,
      MonadFree EInstruction m) =>
     m ()
nodejs = nodejs' "6" (return ())

nodejs'
  :: (Monad m, MonadIO m,
      MonadFree EInstruction m) =>
     String -> m () -> m ()
nodejs' tag extra = do
    liftIO $ dockerIgnore "node_modules"
    liftIO $ dockerIgnore "bower_components"

    from ("node" `tagged` tag)
    extra
    add "./package.json" "/app/package.json"
    workdir "/app/"
    run "npm install"
    add "." "/app/"
    cmd "npm start"
