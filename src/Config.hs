module Config
  ( CodeId
  , Version
  , cssClass
  , outputDir
  , outputPrefix
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

imgDir :: String
imgDir = "img"

tmpDir :: String
tmpDir = "tmp"

imgFormat :: String
imgFormat = "png"

cssClass :: String
cssClass = "satysfi-code"
