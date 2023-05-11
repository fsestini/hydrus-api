{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hydrus.API.Types where

import Hydrus.API.Utils

import Data.Aeson
    ( FromJSON(..),
      ToJSON (..),
      genericParseJSON,
      defaultOptions,
      Options(fieldLabelModifier),
      Value (..), Object, fromJSON, ToJSONKey (..), withObject, (.:) )
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (..), Accept (..), MimeUnrender (mimeUnrender, mimeUnrenderWithType), OctetStream)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (toStrict)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.Client (BaseUrl, ClientM, ClientError, runClientM, mkClientEnv)
import Network.HTTP.Media.MediaType ((//), MediaType)
import Data.Set (Set, fromList)
import Data.Foldable (toList)
import Data.Aeson.Types (FromJSON(..), toJSONKeyText, Parser)
import qualified Data.Aeson.KeyMap as KM (union, empty, fromList, singleton, fromMapText)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Map (Map)
import Data.Text (Text, pack)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal, Symbol, KnownSymbol)
import qualified Data.Map as M
import Data.Aeson.Key (fromString)
import Data.Ratio (Ratio)
import Data.Constraint (Dict(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Except (MonadError (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty (NonEmpty(..))

--------------------------------------------------------------------------------

data APIVersion = APIVersion
  { version :: Int
  , hydrus_version :: Int
  } deriving (Generic, Show)
instance FromJSON APIVersion

newtype PercentJSON a = PercentJSON { unPercentJSON :: a }

instance ToJSON a => ToHttpApiData (PercentJSON a) where
  toQueryParam = toStrict . encodeToLazyText . unPercentJSON

-- Access keys

type AccessKey = Text

newtype AccessKeyResult = AccessKeyResult { access_key :: Text } deriving Generic
instance FromJSON AccessKeyResult

-- Session keys

newtype SessionKey = SessionKey { unSessionKey :: Text }
  deriving newtype (ToHttpApiData, FromJSON)
newtype SessionKeyResult = SessionKeyResult { session_key :: SessionKey }
  deriving Generic
instance FromJSON SessionKeyResult

-- Keys

type HydrusKey = Either AccessKey SessionKey

-- Basic permissions

type BasicPermissionsParam = PercentJSON [Int]

data BasicPermission
  = URLs -- ^ Import and Edit URLs
  | Files -- ^ Import and Delete Files
  | Tags -- ^ Edit File Tags
  | Search -- ^ Search for and Fetch Files
  | Pages -- ^ Manage Pages
  | Cookies -- ^ Manage Cookies and Headers
  | Database -- ^ Manage Database
  | Notes -- ^ Edit File Notes
  | Relationships -- ^ Manage File Relationships
  deriving (Eq, Ord, Show)

instance Enum BasicPermission where
  toEnum = \case
    0 -> URLs
    1 -> Files
    2 -> Tags
    3 -> Search
    4 -> Pages
    5 -> Cookies
    6 -> Database
    7 -> Notes
    8 -> Relationships
    _ -> error "invalid basic permission"
  fromEnum = \case
    URLs -> 0
    Files -> 1
    Tags -> 2
    Search -> 3
    Pages -> 4
    Cookies -> 5
    Database -> 6
    Notes -> 7
    Relationships -> 8

toBasicPermissionsParams :: Set BasicPermission -> BasicPermissionsParam
toBasicPermissionsParams = PercentJSON . map fromEnum . toList

-- Permission info

data PermissionInfoF a = PermissionInfoResultF
  { basic_permissions :: a
  , human_description :: Text
  } deriving (Functor, Generic)
instance FromJSON a => FromJSON (PermissionInfoF a)

type PermissionInfoResult = PermissionInfoF [Int]
type PermissionInfo = PermissionInfoF (Set BasicPermission)

toPermissionInfo :: PermissionInfoResult -> PermissionInfo
toPermissionInfo = fmap (fromList . map toEnum)

-- Services

data ServiceType
  = TagRepository
  | FileRepository
  | LocalFileDomain
  | LocalTagDomain
  | NumericalRatingService
  | LikeDislikeRatingService
  | AllKnownTags
  | AllKnownFiles
  | LocalBooru
  | IPFS
  | Trash
  | AllLocalFiles
  | FileNotes
  | ClientAPI
  | AllDeletedFiles
  | LocalUpdates
  | AllMyFiles
  | IncDecRatingService
  | ServerAdministration

intToServiceType :: Int -> ServiceType
intToServiceType = \case
  0 -> TagRepository
  1 -> FileRepository
  2 -> LocalFileDomain
  5 -> LocalTagDomain
  6 -> NumericalRatingService
  7 -> LikeDislikeRatingService
  10 -> AllKnownTags
  11 -> AllKnownFiles
  12 -> LocalBooru
  13 -> IPFS
  14 -> Trash
  15 -> AllLocalFiles
  17 -> FileNotes
  18 -> ClientAPI
  19 -> AllDeletedFiles
  20 -> LocalUpdates
  21 -> AllMyFiles
  22 -> IncDecRatingService
  99 -> ServerAdministration

type ServiceName = Text
type ServiceKey = Text

data ServiceInfoF a = ServiceInfoF
  { name :: ServiceName
  , service_key :: ServiceKey
  , _type :: a -- Int
  , type_pretty :: Text
  } deriving (Generic, Functor)

instance FromJSON a => FromJSON (ServiceInfoF a) where
  parseJSON =
    let f s = if s == "_type" then "type" else s
        o = defaultOptions { fieldLabelModifier = f }
    in genericParseJSON o

data ServicesInfoF a = ServicesInfoF
  { local_tags :: [ServiceInfoF a]
  , tag_repositories  :: [ServiceInfoF a]
  , file_repositories :: [ServiceInfoF a]
  , local_files :: [ServiceInfoF a]
  , all_local_media :: [ServiceInfoF a]
  , trash :: [ServiceInfoF a]
  , local_updates :: [ServiceInfoF a]
  , all_local_files :: [ServiceInfoF a]
  , all_known_files :: [ServiceInfoF a]
  , all_known_tags :: [ServiceInfoF a]
  } deriving (Generic, Functor)

instance FromJSON a => FromJSON (ServicesInfoF a)

type ServiceInfoResult = ServiceInfoF Int
type ServicesInfoResult = ServicesInfoF Int
type ServiceInfo = ServiceInfoF ServiceType
type ServicesInfo = ServicesInfoF ServiceType

toServiceInfo :: ServiceInfoResult -> ServiceInfo
toServiceInfo = fmap intToServiceType

toServicesInfo :: ServicesInfoResult -> ServicesInfo
toServicesInfo = fmap intToServiceType

-- Files

type SHA256 = Text

newtype AddFileRequestBody = AddFileRequestBody { path :: Text }
  deriving newtype ToJSON

-- 1 - File was successfully imported
-- 2 - File already in database
-- 3 - File previously deleted
-- 4 - File failed to import
-- 7 - File vetoed

data FileStatus
  = NotInDatabase
  | Success
  | AlreadyInDatabase
  | PreviouslyDeleted
  | Failed
  | Vetoed

instance FromJSON FileStatus where
   parseJSON = fmap intToFileStatus . parseJSON

intToFileStatus :: Int -> FileStatus
intToFileStatus = \case
  0 -> NotInDatabase
  1 -> Success
  2 -> AlreadyInDatabase
  3 -> PreviouslyDeleted
  4 -> Failed
  7 -> Vetoed

data AddFileResult = AddFileResult
  { status :: FileStatus
  , hash :: SHA256
  , note :: Text
  } deriving Generic
instance FromJSON AddFileResult

-- toAddFileResult :: AddFileResultF Int -> AddFileResult
-- toAddFileResult = fmap intToFileStatus

type FileId = Int

data Files
  -- = FileId { file_id :: Int }
  = FileIds { file_ids :: [FileId] }
  -- | FileHash { hash :: SHA256 }
  | FileHashes { hashes :: [SHA256] }
  deriving Generic
instance ToJSON Files

filesToMaybes :: Files -> (Maybe [FileId], Maybe [SHA256])
filesToMaybes = \case
  FileIds ids -> (Just ids, Nothing)
  FileHashes hs -> (Nothing, Just hs)

data FileDomain
  = FileServiceKeys { file_service_key :: [ServiceKey] }
  | DeletedFileServiceKeys { deleted_file_service_key :: [ServiceKey] }
  deriving Generic
instance ToJSON FileDomain

newtype DeleteFilesReason = DeleteFilesReason { reason :: Text }
  deriving Generic
instance ToJSON DeleteFilesReason

-- data DeleteFilesRequest = DeleteFilesRequest
--   { files :: Files
--   , fileDomain :: Maybe FileDomain
--   , dfReason :: Maybe DeleteFilesReason
--   }

type DeleteFilesRequest = Files :*: Maybe FileDomain :*: Maybe DeleteFilesReason

-- instance ToJSON DeleteFilesRequest where
--   toJSON x =
--     mergeAeson [toJSON (files x), toJSON (fileDomain x), toJSON (dfReason x)]

type UndeleteFilesRequest = Files :*: Maybe FileDomain

-- URLs


-- data UrlFileStatus
--   | AlreadyInDatabase
--   | PreviouslyDeleted

type URL = Text

data UrlFileInfo = UrlFileInfo
  { status :: FileStatus
  , hash :: SHA256
  , note :: Text
  } deriving Generic
instance FromJSON UrlFileInfo

data UrlFilesInfo = UrlFilesInfo
  { normalised_url :: Text
  , url_file_statuses :: [UrlFileInfo]
  } deriving Generic
instance FromJSON UrlFilesInfo

data UrlType
  = PostURL
  | FileURL
  | GalleryURL
  | WatchableURL
  | UnknownURL

intToUrlType :: Int -> UrlType
intToUrlType = \case
  0 -> PostURL
  2 -> FileURL
  3 -> GalleryURL
  4 -> WatchableURL
  5 -> UnknownURL

instance FromJSON UrlType where
  parseJSON = fmap intToUrlType . parseJSON

data UrlInfo = UrlInfo
  { normalised_url :: Text
  , url_type :: UrlType
  , url_type_string :: Text
  , match_name :: Text
  , can_parse :: Bool
  } deriving Generic
instance FromJSON UrlInfo

type PageKey = Text
type PageName = Text

data DestinationPage
  = DestinationPageKey { destination_page_key :: PageKey }
  | DestinationPageName { destination_page_name :: PageName }
  deriving Generic
instance ToJSON DestinationPage

data AddUrlReq = AddUrlReq
  { url :: URL
  , service_keys_to_additional_tags :: Maybe (Map ServiceKey [Tag])
  , filterable_tags :: Maybe [Tag]
  } deriving Generic
instance ToJSON AddUrlReq

type AddUrlRequest = AddUrlReq :*: Maybe DestinationPage

addUrlRequest :: URL -> AddUrlRequest
addUrlRequest url = AddUrlReq url Nothing Nothing :*: Nothing

withDestinationPage :: AddUrlRequest -> DestinationPage -> AddUrlRequest
withDestinationPage (r :*: _) dp = r :*: Just dp

withAdditionalTags :: AddUrlRequest -> Map ServiceKey [Tag] -> AddUrlRequest
withAdditionalTags (r :*: x) m = r { service_keys_to_additional_tags = Just m } :*: x

withFilterableTags :: AddUrlRequest -> [Tag] -> AddUrlRequest
withFilterableTags (r :*: x) ts = r { filterable_tags = Just ts } :*: x

data AddUrlResult = AddUrlResult
  { human_result_text :: Text
  , normalised_url :: Text
  } deriving Generic
instance FromJSON AddUrlResult

-- Tags

type Tag = Text

data ContentUpdateAction
  = AddToLocalTagService
  | DeleteFromLocalTagService
  | PendToTagRepository
  | RescindPend
  | PetitionFromTagRepository
  | RescindPetition

cuaToInt :: ContentUpdateAction -> Int
cuaToInt = \case
  AddToLocalTagService -> 0
  DeleteFromLocalTagService -> 1
  PendToTagRepository -> 2
  RescindPend -> 3
  PetitionFromTagRepository -> 4
  RescindPetition -> 5

instance ToJSON ContentUpdateAction where
  toJSON = toJSON . cuaToInt

instance ToJSONKey ContentUpdateAction where
  toJSONKey = toJSONKeyText (pack . show . cuaToInt)

type AddTagsRequest
  =   Files
  :*: KeyVal "service_keys_to_tags"
             (Maybe (Map ServiceKey [Tag]))
  :*: KeyVal "service_keys_to_actions_to_tags"
             (Maybe (Map ServiceKey (Map ContentUpdateAction [Tag])))

type AssociateUrlRequest
  =   Files
  :*: KeyVal "urls_to_add" [Text]
  :*: KeyVal "urls_to_delete" [Text]

type CleanTagsResult = KeyVal "tags" [Tag]

-- data SearchTagsRequest = SearchTagsRequest
--   { search :: Text
--   , file_service_key :: Maybe ServiceKey
--   , tag_service_key :: Maybe Text
--   , tag_display_type :: Maybe TagDisplayType
--   }

type SearchTagsRequest = MergeAll
  [ KeyVal "search" Text
  , KeyVal "file_service_key" (Maybe ServiceKey)
  , KeyVal "tag_service_key" (Maybe Text)
  , KeyVal "tag_display_type" (Maybe TagDisplayType)
  ]

-- newtype TagDisplayType = TagDisplayType { unTagDisplayType :: Text }
--   deriving newtype ToJSON

data TagDisplayType = Display

instance ToJSON TagDisplayType where
  toJSON = \case
    Display -> toJSON ("display" :: String)

instance ToHttpApiData TagDisplayType where
  toQueryParam = \case
    Display -> "display"

-- displayTagDisplayType :: TagDisplayType
-- displayTagDisplayType = TagDisplayType "display"

mkSearchTagsRequest :: Text -> SearchTagsRequest
mkSearchTagsRequest t =
  KeyVal t :*: KeyVal Nothing :*: KeyVal Nothing :*: KeyVal Nothing :*: Nil

-- strWithFileService :: SearchTagsRequest -> ServiceKey -> SearchTagsRequest
-- strWithFileService (SearchTagsRequest s _ t d) k = SearchTagsRequest s (Just k) t d

-- strWithTagService :: SearchTagsRequest -> ServiceKey -> SearchTagsRequest
-- strWithTagService r k = r { tag_service_key = Just k }

-- type TagResult = KeyVal "value" Tag :*: KeyVal "count" Int
data TagResult = TagResult { value :: Tag, count :: Int } deriving Generic
instance FromJSON TagResult
type TagSeachResult = KeyVal "tags" [TagResult]

tagResultsToMap :: [TagResult] -> Map Tag Int
tagResultsToMap = M.fromList . fmap (\(TagResult t c) -> (t,c))

-- newtype TagSeachResult = TagSeachResult { tags :: [TagResult] }
--   deriving Generic

    -- withObject "KeyVal" $ \v -> KeyVal
    --  <$> v .: read (symbolVal (keyValStr _))


----- Notes

-- 0 - replace - Overwrite the existing conflicting note.
-- 1 - ignore - Make no changes.
-- 2 - append - Append the new text to the existing text.
-- 3 - rename (default) - Add the new text under a 'name (x)'-style rename.

data NoteConflictResolution
  = Overwrite -- ^ Overwrite the existing conflicting note.
  | Ignore -- ^ Make no changes.
  | Append -- ^ Append the new text to the existing text.
  | Rename -- ^ Add the new text under a 'name (x)'-style rename.

ncrToInt :: NoteConflictResolution -> Int
ncrToInt = \case
  Overwrite -> 0
  Ignore -> 1
  Append -> 2
  Rename -> 3

instance ToJSON NoteConflictResolution where
  toJSON = toJSON . ncrToInt

type NoteName = Text

type SetNotesRequestBody = MergeAll
  [ KeyVal "notes" (Map NoteName Text)
  , KeyVal "hash" SHA256 :+: KeyVal "file_id" FileId
  , KeyVal "merge_cleverly" (Maybe Bool) -- defaults to false
  , KeyVal "extend_existing_note_if_possible" (Maybe Bool) -- defaults to false
  , KeyVal "conflict_resolution" (Maybe NoteConflictResolution) -- defaults to Rename
  ]

plusFromEither :: Either a b -> KeyVal s1 a :+: KeyVal s2 b
plusFromEither = either (L . KeyVal) (R . KeyVal)

type SetNotesRequest = SetNotesRequestBody

mkSetNotesRequest :: Map NoteName Text -> Either SHA256 FileId -> SetNotesRequest
mkSetNotesRequest ns x = -- SetNotesRequest $
      KeyVal ns
  :*: plusFromEither x
  :*: KeyVal Nothing :*: KeyVal Nothing :*: KeyVal Nothing :*: Nil

newtype SetNotesResult = SetNotesResult { notes :: Map NoteName Text }
  deriving Generic
instance FromJSON SetNotesResult

type DeleteNotesRequest
 =   KeyVal "note_names" [NoteName]
 :*: (KeyVal "hash" SHA256 :+: KeyVal "file_id" FileId)

-- Searching and Fetching Files

-- 0 - file size (smallest first/largest first)
-- 1 - duration (shortest first/longest first)
-- 2 - import time (oldest first/newest first)
-- 3 - filetype (N/A)
-- 4 - random (N/A)
-- 5 - width (slimmest first/widest first)
-- 6 - height (shortest first/tallest first)
-- 7 - ratio (tallest first/widest first)
-- 8 - number of pixels (ascending/descending)
-- 9 - number of tags (on the current tag domain) (ascending/descending)
-- 10 - number of media views (ascending/descending)
-- 11 - total media viewtime (ascending/descending)
-- 12 - approximate bitrate (smallest first/largest first)
-- 13 - has audio (audio first/silent first)
-- 14 - modified time (oldest first/newest first)
-- 15 - framerate (slowest first/fastest first)
-- 16 - number of frames (smallest first/largest first)
-- 18 - last viewed time (oldest first/newest first)
-- 19 - archive timestamp (oldest first/newest first)
-- 20 - hash hex (N/A)

data FileSortType
  = FileSize
  | Duration
  | ImportTime
  | FileType
  | Random
  | Width
  | Height
  | Ratio
  | NumberOfPixels
  | NumerOfTags
  | NumberOfMediaViews
  | TotalMediaViewtime
  | ApproximateBitrate
  | HasAudio
  | ModifiedTime
  | Framerate
  | NumberOfFrames
  | LastViewedTime
  | ArchiveTimestamp
  | HashHex

fstToInt :: FileSortType -> Int
fstToInt = \case
  FileSize -> 0
  Duration -> 1
  ImportTime -> 2
  FileType -> 3
  Random -> 4
  Width -> 5
  Height -> 6
  Ratio -> 7
  NumberOfPixels -> 8
  NumerOfTags -> 9
  NumberOfMediaViews -> 10
  TotalMediaViewtime -> 11
  ApproximateBitrate -> 12
  HasAudio -> 13
  ModifiedTime -> 14
  Framerate -> 15
  NumberOfFrames -> 16
  LastViewedTime -> 18
  ArchiveTimestamp -> 19
  HashHex -> 20

instance ToHttpApiData FileSortType where
  toQueryParam = toQueryParam . fstToInt

-- instance ToJSON FileSortType where
--   toJSON = toJSON . fstToInt

type SearchFilesRequest = MergeAll
  [ Prop Tag
  , KeyVal "file_service_key" (Maybe ServiceKey)
  , KeyVal "tag_service_key" (Maybe Text)
  , KeyVal "file_sort_type" (Maybe FileSortType)
  , KeyVal "file_sort_asc" (Maybe Bool)
  -- , KeyVal "return_file_ids" (Maybe Bool)
  -- , KeyVal "return_hashes" (Maybe Bool)
  ]

data FileResultType a :: * where
  FileId :: FileResultType FileId
  FileHash :: FileResultType SHA256

data Prop a = Lit a | And (Prop a) (Prop a) | Or (Prop a) (Prop a)
  deriving (Show, Eq, Ord)

toCNF :: Prop a -> [[a]]
toCNF (Lit x) = [[x]]
toCNF (And p q) = toCNF p ++ toCNF q
toCNF (Or p q) = do
  xs <- toCNF p
  ys <- toCNF q
  pure (xs ++ ys)

mkSearchFilesRequest :: Prop Tag -> SearchFilesRequest
mkSearchFilesRequest x =
  x :*: KeyVal Nothing :*: KeyVal Nothing :*: KeyVal Nothing :*: KeyVal Nothing :*: Nil
  -- :*: KeyVal Nothing :*: KeyVal Nothing :*: Nil

data SearchFilesResult = SearchFilesResult
  { file_ids :: Maybe [FileId]
  , hashes :: Maybe [SHA256]
  } deriving (Generic, Show)
instance FromJSON SearchFilesResult

fileResultTyToFlags :: FileResultType a -> (Maybe Bool, Maybe Bool)
fileResultTyToFlags FileId = (Just True, Nothing)
fileResultTyToFlags FileHash = (Nothing, Just True)

typeCheckFileResult :: FileResultType a -> SearchFilesResult -> [a]
typeCheckFileResult FileId (SearchFilesResult (Just xs) _) = xs
typeCheckFileResult FileHash (SearchFilesResult _ (Just xs)) = xs
typeCheckFileResult _ _ = error "wrong file result type"

-- sfrToEither :: SearchFilesResult -> Either [FileId] [SHA256]
-- sfrToEither (SearchFilesResult (Just xs) _) = Left xs
-- sfrToEither (SearchFilesResult _ (Just xs)) = Right xs

-- type SearchFilesResult
--   =   KeyVal "file_ids" (Maybe [FileId])
--   :*: KeyVal "hashes" (Maybe [SHA256])

type MD5 = Text
type SHA1 = Text
type SHA512 = Text

type SomeHash = Text

data HashType :: * -> * where
  SHA256Ty :: HashType SHA256
  MD5Ty :: HashType MD5
  SHA1Ty :: HashType SHA1
  SHA512Ty :: HashType SHA512

hashTyEq :: HashType a -> Dict (a ~ SomeHash)
hashTyEq = \case
  SHA256Ty -> Dict
  MD5Ty -> Dict
  SHA1Ty -> Dict
  SHA512Ty -> Dict

hashTypeToString :: HashType a -> String
hashTypeToString = \case
  SHA256Ty -> "sha256"
  MD5Ty -> "md5"
  SHA1Ty -> "sha1"
  SHA512Ty -> "sha512"


data Some (f :: * -> *) = forall a . Some (f a)

-- data FileHashesResult = FileHashesResult
--   { hashes :: Map SomeHash SomeHash
--   }

type FileHashesResult = KeyVal "hashes" (Map SomeHash SomeHash)

instance ToHttpApiData (HashType a) where
  toQueryParam = pack . hashTypeToString

instance (forall a . ToHttpApiData (f a)) => ToHttpApiData (Some f) where
  toQueryParam (Some x) = toQueryParam x


data FileMetadataType (a :: *) :: * where
  Essential :: FileMetadataType EssentialMetadata
  Basic     :: FileMetadataType BasicMetadata
--  Detailed  :: FileMetadataType DetailedMetadata

-- TODO: handle Detailed type as well
metaTypeToFlags :: FileMetadataType a -> (Maybe Bool, Maybe Bool, Maybe Bool, Maybe Bool)
metaTypeToFlags = \case
  Essential -> (Just True, Nothing, Nothing, Nothing)
  Basic     -> (Nothing, Just True, Nothing, Nothing)

type FileMetadataRequest = MergeAll
  [ Files
  , KeyVal "create_new_file_ids" (Maybe Bool)
  ]

mkFileMetadataRequest :: Files -> FileMetadataRequest
mkFileMetadataRequest fs = fs :*: KeyVal Nothing :*: Nil

type EssentialMetadataL =
  [ KeyVal "file_id" FileId
  , KeyVal "hash" SHA256
  ]

type BasicMetadataL =
  [ KeyVal "size" Int
  , KeyVal "mime" Text
  , KeyVal "ext" Text
  , KeyVal "width" Int
  , KeyVal "height" Int
  , KeyVal "duration" (Maybe Int)
  , KeyVal "has_audio" Bool
  , KeyVal "num_frames" (Maybe Int)
  , KeyVal "num_words" (Maybe Int)
  ]

type EssentialMetadata = MergeAll EssentialMetadataL
type BasicMetadata = MergeAll (Concat EssentialMetadataL BasicMetadataL)
-- data DetailedMetadata

newtype FileMetadataResult = FileMetadataResult
  { metadata :: [SomeMetadata] }
  deriving Generic

instance FromJSON FileMetadataResult

type SomeMetadata = BasicMetadata :+: EssentialMetadata

typeCheckMetadata :: FileMetadataType a -> SomeMetadata -> a
typeCheckMetadata Essential (R x) = x
typeCheckMetadata Basic (L x) = x
typeCheckMetadata _ _ = error "wrong metadata type"

type FileResult = ByteString

data FileType
instance Accept FileType where
  contentType __ = "*" // "*"

instance MimeUnrender FileType ByteString where
  mimeUnrender _ = pure . LB.toStrict

-----

data HydrusConfig = HydrusConfig
  { hydrusManager :: Manager
  , hydrusAPIUrl :: BaseUrl
  , hydrusKey :: Maybe HydrusKey
  }

mkHydrusConfig :: MonadIO m => BaseUrl -> m HydrusConfig
mkHydrusConfig u = liftIO $ do
  m <- newManager defaultManagerSettings
  pure (HydrusConfig m u Nothing)

mkHydrusConfig' :: Manager -> BaseUrl -> HydrusConfig
mkHydrusConfig' m u = HydrusConfig m u Nothing

withKey :: HydrusKey -> HydrusConfig -> HydrusConfig
withKey k c = c { hydrusKey = Just k }

type MonadHydrus m = (MonadReader HydrusConfig m, MonadIO m, MonadError ClientError m)

hoistH :: MonadHydrus m => ClientM a -> m a
hoistH c = do
  HydrusConfig { hydrusManager = m, hydrusAPIUrl = url } <- ask
  x <- liftIO (runClientM c (mkClientEnv m url))
  either throwError pure x

hoistH' :: MonadHydrus m
        => (Maybe AccessKey -> Maybe SessionKey -> ClientM a) -> m a
hoistH' f = asks hydrusKey >>= \case
  Nothing -> hoistH (f Nothing Nothing)
  Just (Left ak) -> hoistH (f (Just ak) Nothing)
  Just (Right sk) -> hoistH (f Nothing  (Just sk))

-- | Executes a computation in a modified configuration environment.
withConfig :: MonadHydrus m => (HydrusConfig -> HydrusConfig) -> m a -> m a
withConfig = local
