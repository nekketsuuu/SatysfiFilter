{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Control.Monad (when)
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import Text.Pandoc.JSON

import Config
import Misc
import Shell

default (T.Text)

main :: IO ()
main = do
  checkCommands necessaryCmds
  version <- getSatysfiVersion
  codeId <- newIORef 0
  toJSONFilter $ doFilter version codeId

doFilter :: Version -> CodeId -> Block -> IO Block
doFilter version codeId cb@(CodeBlock (id, classes, namevals) contents) = do
  basename <- getFileBasename codeId
  case (caselessElem "satysfi" classes, lookup "eval" namevals) of
    (True, Nothing)            -> converted basename
    (True, Just v) | v /= "no" -> converted basename
    (True, _)                  -> return cb'
    _                          -> return cb
  where
    namevals' = deleteAll "eval" namevals
    converted base = do
      num <- readIORef codeId
      when (num == 0) $ do
        createDirectoryIfMissing True outputDir
        createDirectoryIfMissing True $ outputDir </> imgDir
        createDirectoryIfMissing True $ outputDir </> tmpDir
      modifyIORef codeId (+ 1)
      convertBlock version base (id, classes, namevals') contents
    cb' = CodeBlock (id, classes, namevals') contents
doFilter _ _ x = return x

convertBlock :: Version -> String -> Attr -> String -> IO Block
convertBlock version basename (id, classes, namevals) contents = do
  saveCode basename contents
  compileCode basename
  generateImg basename
  return $ Div attr [codeBlock, imgBlock, versionBlock]
  where
    attr = ("", [cssClass], [])
    displayContents = snipCode contents
    codeBlock = CodeBlock (id, classes, namevals) displayContents
    -- TODO(nekketsuuu): add alt-text
    imgInline = Image nullAttr [] (urlConcat [imgDir, getImgFilename basename], "")
    imgBlock = Para $ [imgInline]
    versionBlock = Para $ [Str $ "Compiled by " ++ version]

snipCode :: String -> String
snipCode contents =
  unlines $ slice begin end ls
  where
    ls = lines contents
    slice from to xs = take (to - from) $ drop from $ xs
    begin = 1 + (fromMaybe (-1) $ findIndex (\l -> isSpecialComment "BEGIN" $ T.pack l) ls)
    end = fromMaybe (1 + length ls) $ findIndex (\l -> isSpecialComment "END" $ T.pack l) ls

