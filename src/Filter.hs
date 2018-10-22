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
  case (caselessElem "satysfi" classes, lookup "eval" namevals) of
    (True, Nothing)                -> resultDiv
    (True, Just "all-pages")       -> allResultDiv
    (True, Just "error")           -> generateErrorDiv version attr' contents
    (True, Just "type-check-only") -> generateCodeDivWithCompile attr' contents
    (True, Just "no")              -> generateCodeDiv attr' contents
    (True, _)                      -> resultDiv
    _                              -> return cb
  where
    namevals' = deleteAll "eval" namevals
    attr' = (id, classes, namevals')
    allResultDiv = do
      basename <- getBasename codeId
      generateAllResultDiv version basename attr' contents
    resultDiv = do
      basename <- getBasename codeId
      generateResultDiv version basename attr' contents
doFilter _ _ x = return x

getBasename :: IORef CodeId -> IO String
getBasename codeId = do
  base <- getFileBasename codeId
  num <- readIORef codeId
  createDirectoriesIfMissing num
  modifyIORef codeId (+ 1)
  return base

createDirectoriesIfMissing :: CodeId -> IO ()
createDirectoriesIfMissing num = when (num == 0) $ do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True $ outputDir </> imgDir
  createDirectoryIfMissing True $ outputDir </> tmpDir

codeAttr :: Attr
codeAttr = ("", [codeCssClass], [])

imgAttr :: Attr
imgAttr = ("", [imgCssClass], [])

codeBlocks :: Attr -> String -> (Block, Block)
codeBlocks attr contents =
  (descBlock, codeBlock)
  where
    displayContents = snipCode contents
    descBlock = Para [Strong [Str $ "コード例"]]
    codeBlock = CodeBlock attr displayContents

imgDescBlock :: Version -> Block
imgDescBlock version =
  Para [Strong [Str $ "コード例の組版結果 (" ++ version ++ ")"]]

imgBlock :: String -> Int -> Block
imgBlock basename page =
  let imgInline = Image nullAttr [] (urlConcat [imgDir, getImgFilename basename page], "")
  in Para [Span imgAttr [imgInline]]

imgBlocks :: Version -> String -> (Block, Block)
imgBlocks version basename =
  (imgDescBlock version, imgBlock basename 1)

generateAllResultDiv :: Version -> String -> Attr -> String -> IO Block
generateAllResultDiv version basename attr contents = do
  saveCode basename contents
  compileCode basename
  generateAllImgs basename
  pages <- getPdfNumberOfPages basename
  let allImgBlocks = map getImgBlock $ [1..pages]
  return $ Div codeAttr ([descBlock1, codeBlock, descBlock2] ++ allImgBlocks)
  where
    (descBlock1, codeBlock) = codeBlocks attr contents
    descBlock2 = imgDescBlock version
    getImgBlock page = imgBlock basename page

generateResultDiv :: Version -> String -> Attr -> String -> IO Block
generateResultDiv version basename attr contents = do
  saveCode basename contents
  compileCode basename
  generateImg basename
  return $ Div codeAttr [descBlock1, codeBlock, descBlock2, imgBlock]
  where
    (descBlock1, codeBlock) = codeBlocks attr contents
    (descBlock2, imgBlock) = imgBlocks version basename

generateCodeDivWithCompile :: Attr -> String -> IO Block
generateCodeDivWithCompile attr contents = do
  saveCode outputTmpBasename contents
  compileCodeWithArgs outputTmpBasename ["--type-check-only"]
  return $ Div codeAttr [descBlock, codeBlock]
  where
    (descBlock, codeBlock) = codeBlocks attr contents

generateCodeDiv :: Attr -> String -> IO Block
generateCodeDiv attr contents = do
  return $ Div codeAttr [descBlock, codeBlock]
  where
    (descBlock, codeBlock) = codeBlocks attr contents

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
