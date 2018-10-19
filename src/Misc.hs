{-# LANGUAGE OverloadedStrings #-}

module Misc
  ( caselessElem
  , deleteAll
  , isSpecialComment
  , urlConcat
  , snipCode
  ) where

import qualified Data.Text as T
import Data.List (drop, findIndex, take)
import Data.Maybe (fromMaybe)
default (T.Text)

caselessElem :: String -> [String] -> Bool
caselessElem query lst = elem query' lst'
  where
    query' = T.toCaseFold . T.pack $ query
    lst' = map (T.toCaseFold . T.pack) lst

deleteAll :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteAll _ [] = []
deleteAll key ((k, v):xs) =
  if k == key then deleteAll key xs
  else (k, v) : deleteAll key xs

urlConcat :: [String] -> String
urlConcat [] = ""
urlConcat (p:ps) = p ++ go ps
  where
    go [] = ""
    go (p:ps) = '/' : (p ++ go ps)

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

snipCode :: String -> String
snipCode contents =
  unlines $ slice begin end ls
  where
    ls = lines contents
    slice from to xs = take (to - from) $ drop from $ xs
    begin = 1 + (fromMaybe (-1) $ findIndex (\l -> isSpecialComment "BEGIN" $ T.pack l) ls)
    end = fromMaybe (1 + length ls) $ findIndex (\l -> isSpecialComment "END" $ T.pack l) ls
