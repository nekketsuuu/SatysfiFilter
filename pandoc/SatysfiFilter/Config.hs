module SatysfiFilter.Config (
  CodeId,
  Version,
  outputDir,
  imgFormat
  ) where

import Data.IORef

type Version = String
type CodeId = IORef Integer

-- TODO(nekketsuuu): ad-hoc
outputDir :: String
outputDir = "compiled"

imgFormat :: String
imgFormat = "png"

