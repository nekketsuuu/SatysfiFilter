{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import Text.Pandoc.JSON
default (T.Text)

main :: IO ()
main = do
  checkCommands ["mogrify", "pandoc", "pdftoppm", "satysfi"]
  toJSONFilter doFilter

checkCommands :: [String] -> IO ()
checkCommands [] = return ()
checkCommands (cmd:cmds) = do
  r <- findExecutable cmd
  case r of
    Just _ -> checkCommands cmds
    Nothing -> error $ "Command \"" ++ cmd ++ "\" does not exist"

doFilter :: Block -> IO Block
doFilter cb@(CodeBlock (id, classes, namevals) contents) =
  case (caselessElem "satysfi" classes, lookup "eval" namevals) of
    (True, Nothing)            -> return converted
    (True, Just v) | v /= "no" -> return converted
    _ -> return cb
  where converted = convertBlock (id, classes, namevals) contents
doFilter x = return x

caselessElem :: String -> [String] -> Bool
caselessElem query lst = elem query' lst'
  where
    query' = T.toCaseFold . T.pack $ query
    lst' = map (T.toCaseFold . T.pack) lst

convertBlock :: Attr -> String -> Block
convertBlock (id, classes, namevals) contents =
  Div nullAttr [codeBlock]
  where
    displayContents = snipCode contents
    codeBlock = CodeBlock (id, classes, namevals) displayContents

snipCode :: String -> String
snipCode contents =
  unlines $ slice begin end ls
  where
    ls = lines contents
    slice from to xs = take (to - from) $ drop from $ xs
    begin = 1 + (fromMaybe (-1) $ findIndex (\l -> isSpecialComment "BEGIN" $ T.pack l) ls)
    end = fromMaybe (1 + length ls) $ findIndex (\l -> isSpecialComment "END" $ T.pack l) ls

-- True if a given line matches r'^[\s]*%%[\s]*TAG.*'
isSpecialComment :: T.Text -> T.Text -> Bool
isSpecialComment tag line = isDoublePercent && isTag
  where
    (isDoublePercent, strTag) = getTag line
    isTag = tag `T.isPrefixOf` strTag
    line' = T.stripStart line
    (head2, rest) = (T.take 2 line', T.drop 2 line')
    getTag str =
      if head2 == "%%" then (True, T.stripStart rest)
      else (False, "")
