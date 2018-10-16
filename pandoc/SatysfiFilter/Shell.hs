{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module SatysfiFilter.Shell (
  checkCommands,
  compileCode,
  generateImg,
  getFileBasename,
  getImgFilename,
  getSatysfiVersion,
  saveCode,
  necessaryCmds
  ) where

import Data.IORef
import qualified Data.Text as T
import Shelly
import System.Directory
import qualified System.FilePath as FP

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
getFileBasename :: CodeId -> IO String
getFileBasename codeId = do
  id <- readIORef codeId
  return $ "output" ++ (show id)

getSatyFilename :: String -> String
getSatyFilename base = base ++ ".saty"

getSatyPath :: String -> String
getSatyPath base = outputDir FP.</> getSatyFilename base

getImgFilename :: String -> String
getImgFilename codeBasename = codeBasename ++ "." ++ imgFormat

getImgPath :: String -> String
getImgPath base = outputDir FP.</> getImgFilename base

getSatysfiVersion :: IO Version
getSatysfiVersion = do
  shOut <- shelly $ silently $ run "satysfi" ["-v"]
  return $ T.unpack $ T.strip shOut

saveCode :: String -> String -> IO ()
saveCode basename contents =
  writeFile (getSatyPath basename) contents

-- TODO(nekketsuuu): implement
compileCode :: String -> IO ()
compileCode basename = return ()

-- TODO(nekketsuuu): implement
generateImg :: String -> IO ()
generateImg basename = return ()
