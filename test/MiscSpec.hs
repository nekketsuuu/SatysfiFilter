{-# LANGUAGE OverloadedStrings #-}

module MiscSpec (spec) where

import Control.Monad (liftM2)
import Data.Char (toLower, toUpper)
import Data.List (all, lookup)
import Data.Maybe (isNothing)
import qualified Data.Text as T

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Misc

default (T.Text)

spec :: Spec
spec = do
  spec_caselessElem
  spec_deleteAll
  spec_urlConcat
  spec_isSpecialComment

spec_caselessElem :: Spec
spec_caselessElem = do
  describe "caselessElem" $ do
    it "passes small manual cases" $ do
      caselessElem "def" ["abc", "def", "ghi"] `shouldBe` True
      caselessElem "DEF" ["ABC", "DEF", "GHI"] `shouldBe` True
      caselessElem "dEf" ["ABc", "deF", "gHI"] `shouldBe` True
    prop "is correct for ASCII characters" prop_caselessElem_ascii

prop_caselessElem_ascii :: ASCIIString -> Property
prop_caselessElem_ascii (ASCIIString str) =
  forAll (stringLowerEq str) $ \ str' ->
  forAll (listsContains str') $ \ strs ->
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

spec_deleteAll :: Spec
spec_deleteAll = do
  describe "deleteAll" $ do
    it "passes small manual cases" $ do
      deleteAll "f" [("f", "x")] `shouldBe` []
      deleteAll "f" [("g", "x")] `shouldBe` [("g", "x")]
      deleteAll "f" [("x", "f")] `shouldBe` [("x", "f")]
    prop "result does not contain a given key" prop_deleteAll_no_key

prop_deleteAll_no_key :: Integer -> Property
prop_deleteAll_no_key k =
  forAll allKvs $ \ kvs ->
  isNothing $ lookup k $ deleteAll k kvs
  where
    allKvs = arbitrary :: Gen [(Integer, Integer)]

spec_urlConcat :: Spec
spec_urlConcat = do
  describe "urlConcat" $ do
    it "passes small manual cases" $ do
      urlConcat ["example.com", "foo", "bar"] `shouldBe` "example.com/foo/bar"
      urlConcat [".", "foo/bar"] `shouldBe` "./foo/bar"

spec_isSpecialComment :: Spec
spec_isSpecialComment = do
  describe "isSpecialComment" $ do
    it "passes small manual cases" $ do
      isSpecialComment "BEGIN" "%% BEGIN" `shouldBe` True
      isSpecialComment "BEGIN" "    %%BEGIN abcdef  " `shouldBe` True
