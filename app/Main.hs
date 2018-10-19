import Data.IORef (newIORef)
import Text.Pandoc.JSON

import Config
import Filter
import Misc
import Shell

main :: IO ()
main = do
  checkCommands necessaryCmds
  version <- getSatysfiVersion
  firstId <- getFirstId
  codeId <- newIORef firstId
  toJSONFilter $ doFilter version codeId
