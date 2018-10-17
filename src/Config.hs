module Config (
  CodeId,
  Version,
  cssClass,
  outputDir,
  imgDir,
  imgFormat,
  tmpDir
  ) where

import Data.IORef (IORef)

type Version = String
type CodeId = IORef Integer

-- TODO(nekketsuuu): ad-hoc
outputDir :: String
outputDir = "generated"

imgDir :: String
imgDir = "img"

tmpDir :: String
tmpDir = "tmp"

imgFormat :: String
imgFormat = "png"

cssClass :: String
cssClass = "p-code"
