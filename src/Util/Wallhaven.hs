module Util.Wallhaven
  ( wallpaperName,
    unlikedWallpapers,
    extractCollectionIDFromCollectionsResponse,
    extractWallhavenMetaLastPage,
    extractFullWallpaperURLs,
  )
where

import Control.Exception.Safe (MonadCatch, MonadThrow, StringException (StringException), catch, throwM, throwString)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import qualified Data.List as List
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Types
import Types.WallhavenAPI (findCollectionByLabel, wallhavenCollectionID, wallhavenCollectionWallpaperFullURL, wallhavenCollectionWallpapersResponseData, wallhavenCollectionWallpapersResponseMetaLastPage)

wallpaperName :: WallpaperPath -> WallpaperName
wallpaperName = head . splitOn "." . last . splitOn "/"

wallpaperNameFromLocalWallpaper :: FilePath -> WallpaperName
wallpaperNameFromLocalWallpaper = stripWallhavenPrefix . wallpaperName

stripWallhavenPrefix :: String -> String
stripWallhavenPrefix name =
  if "wallhaven-" `List.isPrefixOf` name then drop 10 name else name

unlikedWallpapers :: [PreviewURL] -> LocalWallpapers -> LocalWallpapers
unlikedWallpapers favs = filter notFavorite
  where
    notFavorite :: WallpaperPath -> Bool
    notFavorite = not . flip Set.member favNames . wallpaperNameFromLocalWallpaper

    favNames :: Set.Set WallpaperName
    favNames = Set.fromList $ map wallpaperName favs

-- Parses the given bytestring as a Wallpaper Collection Response
-- and returns the ID of the collection with the given label.
extractCollectionIDFromCollectionsResponse ::
  (MonadThrow m, MonadCatch m) =>
  Label ->
  ByteString ->
  m CollectionID
extractCollectionIDFromCollectionsResponse label json = do
  collectionsResponse <-
    decodeWithAPIParseException
      "failed to parse collections response from Wallhaven"
      json
  maybe
    (throwM $ LabelNotFoundException label)
    (pure . wallhavenCollectionID)
    (findCollectionByLabel label collectionsResponse)

-- Parses the given bytestring as Wallhaven meta response and returns
-- the last page field.
extractWallhavenMetaLastPage ::
  (MonadThrow m, MonadCatch m) => ByteString -> m Int
extractWallhavenMetaLastPage json =
  wallhavenCollectionWallpapersResponseMetaLastPage
    <$> decodeWithAPIParseException
      "failed to parse meta from Wallhaven wallpapers response"
      json

extractFullWallpaperURLs ::
  (MonadThrow m, MonadCatch m) => ByteString -> m [FullWallpaperURL]
extractFullWallpaperURLs json =
  fmap wallhavenCollectionWallpaperFullURL
    . wallhavenCollectionWallpapersResponseData
    <$> decodeWithAPIParseException
      "failed to parse wallpapers from Wallhaven response"
      json

-- A variant of Aeson.eitherDecodeStrict that uses MonadThrow for reporting failures.
decodeStrictThrow :: (Aeson.FromJSON a, MonadThrow m) => ByteString -> m a
decodeStrictThrow = either throwString pure . Aeson.eitherDecodeStrict

-- Decodes the given strict JSON bytestring and throws an APIParseException
-- using the provided error message on decode failure.
decodeWithAPIParseException ::
  (MonadThrow m, MonadCatch m, Aeson.FromJSON a) =>
  String ->
  ByteString ->
  m a
decodeWithAPIParseException msg json =
  decodeStrictThrow json
    `catch` ( throwM
                . APIParseException msg
                . stringExceptionMessage
            )

stringExceptionMessage :: StringException -> String
stringExceptionMessage (StringException msg _) = msg
