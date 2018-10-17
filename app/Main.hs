{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Control.Monad (when)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Data.List (drop, findIndex, take)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
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
  firstId <- getFirstId
  codeId <- newIORef firstId
  toJSONFilter $ doFilter version codeId

doFilter :: Version -> IORef CodeId -> Block -> IO Block
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
  return $ Div attr [descBlock1, codeBlock, descBlock2, imgBlock]
  where
    attr = ("", [cssClass], [])
    displayContents = snipCode contents
    descBlock1 = Para $ [Strong [Str $ "コード例"]]
    codeBlock = CodeBlock (id, classes, namevals) displayContents
    descBlock2 = Para $ [Strong [Str $ "コード例の組版結果 (" ++ version ++ ")"]]
    -- TODO(nekketsuuu): add alt-text
    imgInline = Image nullAttr [] (urlConcat [imgDir, getImgFilename basename], "")
    imgBlock = Para $ [imgInline]

snipCode :: String -> String
snipCode contents =
  unlines $ slice begin end ls
  where
    ls = lines contents
    slice from to xs = take (to - from) $ drop from $ xs
    begin = 1 + (fromMaybe (-1) $ findIndex (\l -> isSpecialComment "BEGIN" $ T.pack l) ls)
    end = fromMaybe (1 + length ls) $ findIndex (\l -> isSpecialComment "END" $ T.pack l) ls

