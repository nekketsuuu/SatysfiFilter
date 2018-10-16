{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module SatysfiFilter.Shell (
  checkCommands,
  compileCode,
  generateImg,
  getCodeBasename,
  getImgFilename,
  getSatysfiVersion,
  saveCode,
  necessaryCmds
  ) where

import Data.IORef
import qualified Data.Text as T
import Shelly
import System.Directory

import SatysfiFilter.Config

default (T.Text)

necessaryCmds :: [String]
necessaryCmds = ["mogrify", "pandoc", "pdftoppm", "satysfi"]

checkCommands :: [String] -> IO ()
checkCommands [] = return ()
checkCommands (cmd:cmds) = do
  r <- findExecutable cmd
  case r of
    Just _ -> checkCommands cmds
    Nothing -> error $ "Command \"" ++ cmd ++ "\" does not exist"

-- TODO(nekketsuuu): padding zeros
getCodeBasename :: CodeId -> IO String
getCodeBasename codeId = do
  id <- readIORef codeId
  return $ "output" ++ (show id)

getImgFilename :: String -> String
getImgFilename codeBasename = codeBasename ++ "." ++ imgFormat

getSatysfiVersion :: IO Version
getSatysfiVersion = do
  shOut <- shelly $ silently $ run "satysfi" ["-v"]
  return $ T.unpack $ T.strip shOut

-- TODO(nekketsuuu): implement
saveCode :: String -> String -> IO ()
saveCode codeBasename contents = return ()

-- TODO(nekketsuuu): implement
compileCode :: String -> IO ()
compileCode codeBasename = return ()

-- TODO(nekketsuuu): implement
generateImg :: String -> IO ()
generateImg codeBasename = return ()
