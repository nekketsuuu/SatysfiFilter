module MiscSpec (spec) where

import Control.Monad (liftM2)
import Data.Char (toLower, toUpper)
import Data.List (all)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Misc

spec :: Spec
spec = do
  describe "caselessElem" $ do
    it "passes small manual cases" $ do
      caselessElem "def" ["abc", "def", "ghi"] `shouldBe` True
      caselessElem "DEF" ["ABC", "DEF", "GHI"] `shouldBe` True
      caselessElem "dEf" ["ABc", "deF", "gHI"] `shouldBe` True
    prop "is safe for ASCII characters" prop_caselessElem_ascii

prop_caselessElem_ascii :: ASCIIString -> Property
prop_caselessElem_ascii (ASCIIString str) =
  forAll (stringLowerEq str) $ \str' ->
  forAll (listsContains str') $ \strs ->
  caselessElem str strs
  where
    toLU = elements [toLower, toUpper]
    toLUs str = vectorOf (length str) toLU
    stringLowerEq str = fmap (zipWith (\ x f -> f x) str) (toLUs str)
    lowerEq xs ys = all2 (\ x y -> (toLower x) == (toLower y)) xs ys
    fappend1 gxs ys = fmap (++ ys) gxs
    fappend2 gxs gys = liftM2 (++) gxs gys
    listsContains str = arbitrary `fappend1` [str] `fappend2` arbitrary

all2 :: (a -> a -> Bool) -> [a] -> [a] -> Bool
all2 _ [] [] = True
all2 f (x:xs) (y:ys) = (f x y) && (all2 f xs ys)
all2 _ _ _ = False
