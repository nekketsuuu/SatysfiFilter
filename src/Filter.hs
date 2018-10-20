{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Filter (doFilter) where

import Data.IORef (IORef, modifyIORef, readIORef)
import Control.Monad (when)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Pandoc.JSON

import Config
import Shell
import Misc

default (T.Text)

doFilter :: Version -> IORef CodeId -> Block -> IO Block
doFilter version codeId cb@(CodeBlock (id, classes, namevals) contents) = do
  basename <- getFileBasename codeId
  case (caselessElem "satysfi" classes, lookup "eval" namevals) of
    (True, Nothing)               -> resultDiv basename
    (True, Just v) | v == "error" -> errorDiv basename
    (True, Just v) | v /= "no"    -> resultDiv basename
    (True, _)                     -> return cb'
    _                             -> return cb
  where
    namevals' = deleteAll "eval" namevals
    cb' = CodeBlock (id, classes, namevals') contents
    resultDiv base = do
      num <- readIORef codeId
      createDirectoriesIfMissing num
      modifyIORef codeId (+ 1)
      generateResultDiv version base (id, classes, namevals') contents
    errorDiv base = do
      generateErrorDiv version (id, classes, namevals') contents
doFilter _ _ x = return x

createDirectoriesIfMissing :: CodeId -> IO ()
createDirectoriesIfMissing num = when (num == 0) $ do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True $ outputDir </> imgDir
  createDirectoryIfMissing True $ outputDir </> tmpDir

codeAttr :: Attr
codeAttr = ("", [codeCssClass], [])

imgAttr :: Attr
imgAttr = ("", [imgCssClass], [])

generateResultDiv :: Version -> String -> Attr -> String -> IO Block
generateResultDiv version basename (id, classes, namevals) contents = do
  saveCode basename contents
  compileCode basename
  generateImg basename
  return $ Div codeAttr [descBlock1, codeBlock, descBlock2, imgBlock]
  where
    displayContents = snipCode contents
    descBlock1 = Para [Strong [Str $ "コード例"]]
    codeBlock = CodeBlock (id, classes, namevals) displayContents
    descBlock2 = Para [Strong [Str $ "コード例の組版結果 (" ++ version ++ ")"]]
    imgInline = Image nullAttr [] (urlConcat [imgDir, getImgFilename basename], "")
    imgBlock = Para [Span imgAttr [imgInline]]

-- TODO(nekketsuuu): Parallelize (consider naming of code files)
generateErrorDiv :: Version -> Attr -> String -> IO Block
generateErrorDiv version attr contents = do
  saveCode outputErrorBasename contents
  mes <- compileInvalidCode outputErrorBasename
  let lines = T.lines mes
  let err = T.unlines $ dropWhile (\ line -> not $ "!" `T.isPrefixOf` line) lines
  return $ generateErrorDiv' version attr contents (T.unpack err)

generateErrorDiv' :: Version -> Attr -> String -> String -> Block
generateErrorDiv' version attr contents message =
  Div codeAttr [descBlock1, codeBlock, descBlock2, errBlock]
  where
    displayContents = snipCode contents
    descBlock1 = Para [Strong [Str $ "間違ったコード例"]]
    codeBlock = CodeBlock attr displayContents
    descBlock2 = Para [Strong [Str $ "コード例を組版しようとしたときのエラー例 (" ++ version ++ ")"]]
    errAttr = ("", ["satysfi-error"], [])
    errBlock = CodeBlock errAttr message
