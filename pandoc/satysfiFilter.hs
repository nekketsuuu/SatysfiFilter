import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import Text.Pandoc.JSON

-- TODO(nekketsuuu): Use Data.Text instead of String

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

-- True if a given line matches r'^[\s]*%%[\s]*TAG.*'
isSpecialComment :: String -> String -> Bool
isSpecialComment tag line = isDoublePercent && isTag
  where
    (isDoublePercent, strTag) = getTag line
    isTag = tag `isPrefixOf` strTag
    line' = dropHeadSpaces line
    (head2, rest) = (take 2 line', drop 2 line')
    getTag str =
      if head2 == "%%" then (True, dropHeadSpaces rest)
      else (False, "")

dropHeadSpaces :: String -> String
dropHeadSpaces "" = ""
dropHeadSpaces (x:xs)
  | isSpace x = dropHeadSpaces xs
  | otherwise = x:xs
