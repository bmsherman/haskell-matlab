{-# LANGUAGE TemplateHaskell        #-}

module Test.UtilTemplate where

import           Control.Monad.Catch             (MonadThrow)
import           Control.Monad.IO.Class
import qualified Control.Exception               as E
import qualified Data.Text                       as Txt
import Language.Haskell.TH (Exp, Q, runIO)
import           Path
import           System.Environment

getRepoDir :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
getRepoDir = do
  testExePath <- liftIO $ getEnv "PWD"
  repoPath <- pure $ parseAbsDir $ Txt.unpack $ fst $ Txt.breakOn
    (Txt.pack "FarmDataServer/.stack-work") (Txt.pack testExePath)
  repoPath
