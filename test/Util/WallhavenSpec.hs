module Util.WallhavenSpec (spec) where

import Data.List as List
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Types
import Util.Wallhaven (unlikedWallpapers, wallpaperName)

prefixExtPathGen :: Gen (String, WallpaperName)
prefixExtPathGen = do
  name <- listOf1 $ elements ['a' .. 'z']
  extension <- listOf1 $ elements ['a' .. 'z']
  prefix <- arbitrary
  return (prefix <> "/" <> name <> "." <> extension, name)

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
      wallpaperName (name <> "." <> extension) `shouldBe` name

prop_wallpaperName_no_name :: Property
prop_wallpaperName_no_name =
  forAll (listOf1 $ elements ['a' .. 'z']) $ \extension ->
    forAll arbitrary $ \prefix ->
      wallpaperName (prefix <> "/." <> extension) `shouldBe` ""

alpha :: Gen String
alpha = listOf1 $ elements ['a' .. 'z']

previewURLFromName :: String -> PreviewURL
previewURLFromName = ("https://wallhaven.cc/w/" <>)

localWallpaperFromName :: String -> Gen WallpaperPath
localWallpaperFromName name = do
  prefixBits <- listOf1 alpha
  return $ List.intercalate "/" prefixBits <> "/" <> "wallhaven-" <> name <> ".jpg"

unlikedWallpapersGen :: Gen ([PreviewURL], LocalWallpapers, LocalWallpapers)
unlikedWallpapersGen = do
  favNames <- listOf1 alpha
  let previewURLs = map previewURLFromName favNames
  syncedLocalWallpapers <- sublistOf favNames >>= mapM localWallpaperFromName
  unlikedLocalWallpapers <-
    listOf1 alpha
      >>= mapM localWallpaperFromName . filter (not . flip elem favNames)
  return
    ( previewURLs,
      syncedLocalWallpapers ++ unlikedLocalWallpapers,
      unlikedLocalWallpapers
    )

prop_unliked_wallpapers :: Property
prop_unliked_wallpapers = forAll unlikedWallpapersGen $
  \(favPreviewURLs, allLocal, unliked) ->
    unlikedWallpapers favPreviewURLs allLocal `shouldBe` unliked

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
