module SatysfiFilter.Config (
  CodeId,
  Version,
  cssClass,
  outputDir,
  imgFormat
  ) where

import Data.IORef

type Version = String
type CodeId = IORef Integer

-- TODO(nekketsuuu): ad-hoc
outputDir :: String
outputDir = "generated"

imgFormat :: String
imgFormat = "png"

cssClass :: String
cssClass = "p-code"
