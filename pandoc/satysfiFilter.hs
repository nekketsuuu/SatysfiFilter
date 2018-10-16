import Data.Char
import Data.List
import Data.Maybe
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter doFilter

doFilter :: Block -> IO Block
doFilter cb@(CodeBlock (id, classes, namevals) contents) =
  case (elemUpTo "satysfi" classes, lookup "eval" namevals) of
    (True, Nothing)            -> return converted
    (True, Just v) | v /= "no" -> return converted
    _ -> return cb
  where converted = convertBlock (id, classes, namevals) contents
doFilter x = return x

elemUpTo :: String -> [String] -> Bool
elemUpTo query target = elem (toL query) (map toL target)
  where toL = map toLower

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
    begin = 1 + (fromMaybe (-1) $ findIndex (\l -> isSpecialComment "BEGIN" l) ls)
    end = fromMaybe (1 + length ls) $ findIndex (\l -> isSpecialComment "END" l) ls

isSpecialComment :: String -> String -> Bool
isSpecialComment tag str = isDoublePercent && isTag
  where
    (isDoublePercent, strTag) = getTag str
    isTag = tag `isPrefixOf` strTag
    str' = dropHeadSpaces str
    (head2, rest) = (take 2 str', drop 2 str')
    getTag str =
      if head2 == "%%" then (True, dropHeadSpaces rest)
      else (False, "")

dropHeadSpaces :: String -> String
dropHeadSpaces "" = ""
dropHeadSpaces (x:xs)
  | isSpace x = dropHeadSpaces xs
  | otherwise = x:xs
