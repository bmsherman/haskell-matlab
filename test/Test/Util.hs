{-# LANGUAGE TemplateHaskell        #-}

module Test.Util where

import           Control.Monad.Catch             (MonadThrow)
import           Control.Monad.IO.Class
import qualified Control.Exception               as E
import Control.Monad (join)
-- import           Data.AEq
-- import           Data.CallStack
import           Data.Either                     (fromRight)
import qualified Data.Text                       as Txt
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import           Path
import           System.Environment
-- import           Test.HUnit.Lang                 (FailureReason(..), HUnitFailure(..))
import           Test.UtilTemplate

getRepoDirStatic :: String
getRepoDirStatic = $(join $ lift <$> toFilePath <$> (runIO $ getRepoDir))


-- fromRightTst :: Either a b -> b
-- fromRightTst = (fromRight undefined)

-- assertEqualFP :: (AEq a, Show a) => [Char] -> a -> a -> IO ()
-- assertEqualFP preface e a = unless (a === e) $ unequalBody preface e a

-- assertApproxFP :: (AEq a, Show a) => [Char] -> a -> a -> IO ()
-- assertApproxFP preface e a = unless (a ~== e) $ unequalBody preface e a

-- unequalBody :: (Show a1, Show a2) => [Char] -> a2 -> a1 -> IO ()
-- unequalBody preface expected actual = do
--   (prefaceMsg `deepseq` expectedMsg `deepseq` actualMsg `deepseq` E.throwIO
--     (HUnitFailure location $ ExpectedButGot prefaceMsg expectedMsg actualMsg))
--   where
--     prefaceMsg
--       | null preface = Nothing
--       | otherwise = Just preface
--     expectedMsg = show expected
--     actualMsg = show actual

-- -- | As copied from https://hackage.haskell.org/package/HUnit-1.6.0.0/docs/src/Test.HUnit.Lang.html#location
-- location :: HasCallStack => Maybe SrcLoc
-- location = case reverse callStack of
--   (_, loc) : _ -> Just loc
--   [] -> Nothing
