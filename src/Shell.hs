{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Shell
  ( checkCommands
  , compileCode
  , compileCodeWithArgs
  , compileInvalidCode
  , generateAllImgs
  , generateImg
  , getFileBasename
  , getFirstId
  , getImgFilename
  , getPdfNumberOfPages
  , getSatysfiVersion
  , saveCode
  , necessaryCmds
  ) where

import Data.IORef (IORef, readIORef)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Shelly
import System.Directory (doesDirectoryExist, findExecutable, listDirectory)
import qualified System.FilePath as FP
import Text.Read (readMaybe)

import Config

default (T.Text)

necessaryCmds :: [String]
necessaryCmds = ["awk", "mogrify", "pandoc", "pdftk", "pdftoppm", "satysfi"]

checkCommands :: [String] -> IO ()
checkCommands [] = return ()
checkCommands (cmd:cmds) = do
  r <- findExecutable cmd
  case r of
    Just _ -> checkCommands cmds
    Nothing -> error $ "Command \"" ++ cmd ++ "\" does not exist"

getFirstId :: IO CodeId
getFirstId = do
  filesOut <- listDirectory' outputDir
  filesImg <- listDirectory' $ outputDir FP.</> imgDir
  filesTmp <- listDirectory' $ outputDir FP.</> tmpDir
  let bases = map FP.takeBaseName (filesOut ++ filesImg ++ filesTmp)
  let suffixes = catMaybes $ map (stripPrefix outputPrefix) bases
  let ids = (catMaybes $ map readMaybe suffixes) :: [CodeId]
  return $
    case ids of
      [] -> 0
      _ -> 1 + maximum ids
  where
    listDirectory' path = do
      exist <- doesDirectoryExist path
      if exist then listDirectory path else return []

-- TODO(nekketsuuu): padding zeros
-- TODO(nekketsuuu): use hash value?
getFileBasename :: IORef CodeId -> IO String
getFileBasename codeId = do
  id <- readIORef codeId
  return $ outputPrefix ++ (show id)

getSatyPath :: String -> String
getSatyPath base = outputDir FP.</> tmpDir FP.</> base FP.<.> "saty"

getPdfPath :: String -> String
getPdfPath base = outputDir FP.</> tmpDir FP.</> base FP.<.> "pdf"

getImgFilename :: String -> Int -> String
getImgFilename base page = base ++ "_" ++ (show page) FP.<.> imgFormat

getImgPathWithoutEx :: String -> Int -> String
getImgPathWithoutEx base page = outputDir FP.</> imgDir FP.</> base ++ "_" ++ (show page)

getImgPath :: String -> Int -> String
getImgPath base page = (getImgPathWithoutEx base page) FP.<.> imgFormat

getSatysfiVersion :: IO Version
getSatysfiVersion = do
  shOut <- shelly $ silently $ run "satysfi" ["-v"]
  return $ T.unpack $ T.strip shOut

saveCode :: String -> String -> IO ()
saveCode base contents =
  writeFile (getSatyPath base) contents'
  where
    contents' =
      if last contents == '\n' then contents
      else contents ++ "\n"

compileCode :: String -> IO ()
compileCode base = shelly $ silently $ do
  run_ "satysfi" [ T.pack $ getSatyPath base
                 , "-o", T.pack $ getPdfPath base]

compileCodeWithArgs :: String -> [T.Text] -> IO ()
compileCodeWithArgs base args = shelly $ silently $ do
  run_ "satysfi" $
    [ T.pack $ getSatyPath base
    , "-o", T.pack $ getPdfPath base]
    ++
    args

generateOneImg :: String -> Int -> Sh ()
generateOneImg base page = do
  run_ "pdftoppm" [ T.pack ("-" ++ imgFormat)
                  , "-f", T.pack $ show page
                  , "-singlefile"
                  , "-rx", resolution
                  , "-ry", resolution
                  , T.pack $ getPdfPath base
                  , T.pack $ getImgPathWithoutEx base page]
  run_ "mogrify" [ "-fuzz", "20%"
                 , "-trim"
                 , "-bordercolor", "White"
                 , "-border", T.pack (border ++ "x" ++ border)
                 , "-strip"
                 , T.pack $ getImgPath base page]
  where
    resolution = "300"
    border = "30"

generateImg :: String -> IO ()
generateImg base = shelly $ silently $
  generateOneImg base 1

getPdfNumberOfPages :: String -> IO Int
getPdfNumberOfPages base =
  shelly $ silently $ getPdfNumberOfPages' base

getPdfNumberOfPages' :: String -> Sh Int
getPdfNumberOfPages' base = do
  let pdf = T.pack $ getPdfPath base
  pageStr <-
    run "pdftk" [pdf, "dump_data"] -|- run "awk" ["/NumberOfPages/{print $2}"]
  return $
    either error fst $ TR.decimal pageStr

generateAllImgs :: String -> IO ()
generateAllImgs base = shelly $ silently $ do
  pages <- getPdfNumberOfPages' base
  mapM_ (generateOneImg base) [1..pages]

compileInvalidCode :: String -> IO T.Text
compileInvalidCode base = shelly $ silently $ do
  (exitCode, mes) <-
    runErr "satysfi" [ T.pack $ getSatyPath base
                     , "-o", T.pack $ getPdfPath base]
  when (exitCode == 0) $
    errorExit $
    "Compilation of " `T.append` (T.pack base) `T.append` ".saty must be failed, but didn't."
  return mes

runErr :: Shelly.FilePath -> [T.Text] -> Sh (Int, T.Text)
runErr cmd args = errExit False $ do
  -- TODO(nekketsuuu): patch SATySFi to use stderr if panic
  mes <- run cmd args
  exitCode <- lastExitCode
  return (exitCode, mes)
