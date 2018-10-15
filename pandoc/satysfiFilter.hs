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
  CodeBlock (id, classes, namevals) ("% modified\n" ++ displayContents)
  where
    displayContents = snipCode contents

snipCode :: String -> String
snipCode contents =
  unlines $ slice begin end ls
  where
    ls = lines contents
    slice from to xs = take (to - from) $ drop from $ xs
    -- TODO(nekketsuuu): use regexp to allow any number of spaces
    begin = 1 + (fromMaybe (-1) $ findIndex (\l -> isInfixOf "%% BEGIN" l) ls)
    end = fromMaybe (1 + length ls) $ findIndex (\l -> isInfixOf "%% END" l) ls
