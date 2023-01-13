module Wallhaven.ActionSpec (spec) where

import Control.Monad.State (MonadState, State, get, modify, runState)
import qualified Data.List as List
import Test.Hspec
import Test.QuickCheck (Property, listOf, sublistOf)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Property (forAll)
import Types (WallpaperName)
import Util.Gen (alpha)
import Wallhaven.Action (deleteUnlikedWallpapers)
import Wallhaven.Monad
  ( MonadDeleteWallpaper,
    MonadGetDownloadedWallpapers,
    MonadInitDB,
    MonadSaveWallpaper,
    deleteWallpaper,
    getDownloadedWallpapers,
    initDB,
    saveWallpaper,
  )

newtype TestActionM a = TestActionM
  { unTestActionM :: State [WallpaperName] a
  }
  deriving (Functor, Applicative, Monad, MonadState [WallpaperName])

runTestActionM ::
  TestActionM a -> [WallpaperName] -> (a, [WallpaperName])
runTestActionM = runState . unTestActionM

instance MonadGetDownloadedWallpapers TestActionM where
  getDownloadedWallpapers = get

instance MonadDeleteWallpaper TestActionM where
  deleteWallpaper w = modify (List.delete w)

instance MonadSaveWallpaper TestActionM where
  saveWallpaper w _ = modify (w :)

instance MonadInitDB TestActionM where
  initDB = pure ()

deleteUnlikedWallpapersGen ::
  Gen ([WallpaperName], [WallpaperName], [WallpaperName])
deleteUnlikedWallpapersGen = do
  liked <- List.nub <$> listOf alpha
  unliked <- (List.\\ liked) . List.nub <$> listOf alpha
  downloaded <- (++) <$> sublistOf unliked <*> sublistOf liked
  return (downloaded, liked, unliked)

prop_deleteUnlikedWallpapers :: Property
prop_deleteUnlikedWallpapers =
  forAll deleteUnlikedWallpapersGen $ \(downloaded, liked, unliked) -> do
    let (unliked', downloaded') =
          runTestActionM
            (deleteUnlikedWallpapers downloaded liked)
            downloaded
    downloaded' `shouldBe` (liked `List.intersect` downloaded)
    unliked' `shouldBe` (unliked `List.intersect` downloaded)

spec :: Spec
spec = do
  describe "deleteUnlikedWallpapers" $ do
    it "deletes unliked wallpapers and returns them" prop_deleteUnlikedWallpapers
