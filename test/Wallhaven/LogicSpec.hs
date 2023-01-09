module Wallhaven.LogicSpec (spec) where

import Data.List as List
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Types
import Wallhaven.Logic (unlikedWallpapers, wallpaperName)

prefixExtPathGen :: Gen (String, WallpaperName)
prefixExtPathGen = do
  name <- listOf1 $ elements ['a' .. 'z']
  extension <- listOf1 $ elements ['a' .. 'z']
  let nameWithExt = name ++ "." ++ extension
  prefix <- arbitrary
  return (prefix <> "/" <> nameWithExt, nameWithExt)

prop_wallpaperName_prefix_ext :: Property
prop_wallpaperName_prefix_ext = forAll prefixExtPathGen $ \(path, name) ->
  wallpaperName path `shouldBe` name

prop_wallpaperName_no_prefix_no_ext :: Property
prop_wallpaperName_no_prefix_no_ext =
  forAll (listOf1 $ elements ['a' .. 'z']) $ \name ->
    wallpaperName name `shouldBe` name

prop_wallpaperName_prefix_no_ext :: Property
prop_wallpaperName_prefix_no_ext =
  forAll arbitrary $ \prefix ->
    forAll (listOf1 $ elements ['a' .. 'z']) $ \name ->
      wallpaperName (prefix <> "/" <> name) `shouldBe` name

prop_wallpaperName_no_prefix_ext :: Property
prop_wallpaperName_no_prefix_ext =
  forAll (listOf1 $ elements ['a' .. 'z']) $ \name ->
    forAll (listOf1 $ elements ['a' .. 'z']) $ \extension ->
      let nameWithExt = name ++ "." ++ extension
       in wallpaperName nameWithExt `shouldBe` nameWithExt

prop_wallpaperName_no_name :: Property
prop_wallpaperName_no_name =
  forAll (listOf1 $ elements ['a' .. 'z']) $ \extension ->
    forAll arbitrary $ \prefix ->
      wallpaperName (prefix <> "/." <> extension) `shouldBe` "." <> extension

alpha :: Gen String
alpha = listOf1 $ elements ['a' .. 'z']

fullWallpaperURL :: String -> String
fullWallpaperURL name = "https://w.wallhaven.cc/full/2y/" <> name <> ".jpg"

localWallpaperFromName :: String -> Gen WallpaperPath
localWallpaperFromName name = do
  prefixBits <- listOf1 alpha
  return $ List.intercalate "/" prefixBits <> "/" <> name <> ".jpg"

unlikedWallpapersGen :: Gen ([FullWallpaperURL], LocalWallpapers, LocalWallpapers)
unlikedWallpapersGen = do
  favNames <- listOf1 alpha
  let favFullURLs = map fullWallpaperURL favNames
  syncedLocalWallpapers <- sublistOf favNames >>= mapM localWallpaperFromName
  unlikedLocalWallpapers <-
    listOf1 alpha
      >>= mapM localWallpaperFromName . filter (not . flip elem favNames)
  return
    ( favFullURLs,
      syncedLocalWallpapers ++ unlikedLocalWallpapers,
      unlikedLocalWallpapers
    )

prop_unliked_wallpapers :: Property
prop_unliked_wallpapers = forAll unlikedWallpapersGen $
  \(favFullURLs, allLocal, unliked) ->
    unlikedWallpapers favFullURLs allLocal `shouldBe` unliked

spec :: Spec
spec = do
  describe "Util.Wallhaven" $ do
    describe "wallpaperName" $ do
      prop "should strip prefix and extension" prop_wallpaperName_prefix_ext
      prop
        "should return the input if no prefix or extension exists"
        prop_wallpaperName_no_prefix_no_ext
      prop
        "should return name after last slash if no extension exists"
        prop_wallpaperName_prefix_no_ext
      it "returns empty string on empty input" $
        wallpaperName "" `shouldBe` ""
      prop
        "should return name before first dot if no prefix exists"
        prop_wallpaperName_no_prefix_ext
      prop
        "should return empty string if no name exists"
        prop_wallpaperName_no_name

    describe "unlikedWallpapers" $ do
      prop "should return unliked wallpapers" prop_unliked_wallpapers
