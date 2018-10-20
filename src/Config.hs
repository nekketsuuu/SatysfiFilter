module Config
  ( CodeId
  , Version
  , codeCssClass
  , imgCssClass
  , outputDir
  , outputErrorBasename
  , outputPrefix
  , outputTmpBasename
  , imgDir
  , imgFormat
  , tmpDir
  ) where

import Data.IORef (IORef)

type Version = String
type CodeId = Int

outputDir :: String
outputDir = "generated"

outputPrefix :: String
outputPrefix = "output"

outputErrorBasename :: String
outputErrorBasename = "must-error"

outputTmpBasename :: String
outputTmpBasename = "tmp"

imgDir :: String
imgDir = "img"

tmpDir :: String
tmpDir = "tmp"

imgFormat :: String
imgFormat = "png"

codeCssClass :: String
codeCssClass = "satysfi-code"

imgCssClass :: String
imgCssClass = "result-img"
