{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Text.Pandoc.JSON

import SatysfiFilter.Config
import SatysfiFilter.Misc
import SatysfiFilter.Shell

default (T.Text)

main :: IO ()
main = do
  checkCommands necessaryCmds
  version <- getSatysfiVersion
  codeId <- newIORef 0
  toJSONFilter $ doFilter version codeId

doFilter :: Version -> CodeId -> Block -> IO Block
doFilter version codeId cb@(CodeBlock (id, classes, namevals) contents) = do
  codeBasename <- getCodeBasename codeId
  modifyIORef codeId (+ 1)
  case (caselessElem "satysfi" classes, lookup "eval" namevals) of
    (True, Nothing)            -> converted codeBasename
    (True, Just v) | v /= "no" -> converted codeBasename
    _ -> return cb
  where converted base = convertBlock version base (id, classes, namevals) contents
doFilter _ _ x = return x

convertBlock :: Version -> String -> Attr -> String -> IO Block
convertBlock version codeBasename (id, classes, namevals) contents = do
  saveCode codeBasename contents
  compileCode codeBasename
  generateImg codeBasename
  return $ Div nullAttr [codeBlock, imgBlock, versionBlock]
  where
    displayContents = snipCode contents
    codeBlock = CodeBlock (id, classes, namevals) displayContents
    imgInline = Image nullAttr [] (urlConcat [imgDir, getImgFilename codeBasename], "")
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

