{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Filter
  ( convertBlock
  , doFilter
  ) where

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
  return $ Div codeAttr [descBlock1, codeBlock, descBlock2, imgBlock]
  where
    codeAttr = ("", [codeCssClass], [])
    displayContents = snipCode contents
    descBlock1 = Para [Strong [Str $ "コード例"]]
    codeBlock = CodeBlock (id, classes, namevals) displayContents
    descBlock2 = Para [Strong [Str $ "コード例の組版結果 (" ++ version ++ ")"]]
    imgAttr = ("", [imgCssClass], [])
    imgInline = Image nullAttr [] (urlConcat [imgDir, getImgFilename basename], "")
    imgBlock = Para [Span imgAttr [imgInline]]
