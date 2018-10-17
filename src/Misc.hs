{-# LANGUAGE OverloadedStrings #-}

module Misc
  ( caselessElem
  , deleteAll
  , isSpecialComment
  , urlConcat
  ) where

import qualified Data.Text as T
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
