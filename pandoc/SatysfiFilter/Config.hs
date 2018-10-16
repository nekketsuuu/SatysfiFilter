module SatysfiFilter.Config (
  CodeId,
  Version,
  imgDir,
  imgFormat
  ) where

import Data.IORef

type Version = String
type CodeId = IORef Integer

-- TODO(nekketsuuu): ad-hoc
imgDir :: String
imgDir = "./compiled"

imgFormat :: String
imgFormat = "png"

