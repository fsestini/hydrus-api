{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hydrus.API.Internal where

import Hydrus.API.Utils
import Hydrus.API.Types

import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TIO
import Servant.API
import Servant.Client
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Client
  (newManager, defaultManagerSettings, ManagerSettings (..),
   Request (requestHeaders), Manager)
import Data.Aeson
import GHC.Generics (Generic)
import Data.ByteString (ByteString, toStrict)
import Data.Maybe (fromMaybe)
import Data.Aeson.Text (encodeToLazyText)

import Control.Monad.Free
import qualified Servant.Client.Free as CF

import qualified Servant.Client.Internal.HttpClient as I
import qualified Network.HTTP.Client                as HTTP
import Servant.Client.Core.RunClient (ClientF(RunRequest), RunClient)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Except (MonadError (throwError))
import Data.Set (Set)
import Data.Text (pack, Text)
import Control.Monad (void)
import Data.Map (Map)
import Data.Constraint (Dict(..))
import Servant.Client.Core (clientIn)

type HydrusAPI = "api_version" :> Get '[JSON] APIVersion
            :<|> "request_new_permissions"
                  :> QueryParam "name" Text
                  :> QueryParam "basic_permissions" BasicPermissionsParam
                  :> Get '[JSON] AccessKeyResult
            :<|> "session_key"
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] SessionKeyResult
            :<|> "verify_access_key"
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] PermissionInfoResult
            :<|> "get_service"
                  :> QueryParam "service_name" ServiceName
                  :> QueryParam "service_key" ServiceKey
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] ServiceInfoResult
            :<|> "get_services"
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] ServicesInfoResult
            :<|> "add_files" :> "add_file"
                  :> ReqBody '[JSON] AddFileRequestBody
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Put '[JSON] AddFileResult
            :<|> "add_files" :> "add_file"
                  :> ReqBody '[OctetStream] ByteString
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[JSON] AddFileResult
            :<|> "add_files" :> "delete_files"
                  :> ReqBody '[JSON] DeleteFilesRequest
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[] NoContent
            :<|> "add_files" :> "undelete_files"
                  :> ReqBody '[JSON] UndeleteFilesRequest
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[] NoContent
            :<|> "add_files" :> "archive_files"
                  :> ReqBody '[JSON] Files
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[] NoContent
            :<|> "add_files" :> "unarchive_files"
                  :> ReqBody '[JSON] Files
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[] NoContent
            :<|> "add_urls" :> "get_url_files"
                  :> QueryParam "url" Text
                  :> QueryParam "doublecheck_file_system" Bool
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] UrlFilesInfo
            :<|> "add_urls" :> "get_url_info"
                  :> QueryParam "url" Text
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] UrlInfo
            :<|> "add_urls" :> "add_url"
                  :> ReqBody '[JSON] AddUrlRequest
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[JSON] AddUrlResult
            :<|> "add_urls" :> "associate_url"
                  :> ReqBody '[JSON] AssociateUrlRequest
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[] NoContent

            :<|> "add_tags" :> "clean_tags"
                  :> QueryParam "tags" (PercentJSON [Tag])
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] CleanTagsResult
            :<|> "add_tags" :> "search_tags"
                  :> QueryParam "search" Text
                  :> QueryParam "file_service_key" ServiceKey
                  :> QueryParam "tag_service_key" Text
                  :> QueryParam "tag_display_type" TagDisplayType
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] TagSeachResult
            :<|> "add_tags" :> "add_tags"
                  :> ReqBody '[JSON] AddTagsRequest
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[] NoContent

            :<|> "add_notes" :> "set_notes"
                  :> ReqBody '[JSON] SetNotesRequestBody
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[JSON] SetNotesResult

            :<|> "add_notes" :> "delete_notes"
                  :> ReqBody '[JSON] DeleteNotesRequest
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Post '[] NoContent

            :<|> "get_files" :> "search_files"
                  :> QueryParam "tags" (PercentJSON [[Tag]])
                  :> QueryParam "file_service_key" ServiceKey
                  :> QueryParam "tag_service_key" ServiceKey
                  :> QueryParam "file_sort_type" FileSortType
                  :> QueryParam "file_sort_asc" Bool
                  :> QueryParam "return_file_ids" Bool
                  :> QueryParam "return_hashes" Bool
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] SearchFilesResult

            :<|> "get_files" :> "file_hashes"
                  :> QueryParam "hashes" (PercentJSON [SomeHash])
                  :> QueryParam "source_hash_type" (Some HashType)
                  :> QueryParam "desired_hash_type" (Some HashType)
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] FileHashesResult

            :<|> "get_files" :> "file_metadata"
                  :> QueryParam "file_ids" (PercentJSON [FileId])
                  :> QueryParam "hashes" (PercentJSON [SHA256])
                  :> QueryParam "create_new_file_ids" Bool
                  :> QueryParam "only_return_identifiers" Bool
                  :> QueryParam "only_return_basic_information" Bool
                  :> QueryParam "detailed_url_information" Bool
                  :> QueryParam "include_notes" Bool
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  :> Get '[JSON] FileMetadataResult

             :<|> "get_files" :> "file"
                  :> QueryParam "file_id" FileId
                  :> QueryParam "hash" SHA256
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  -- TODO: we might need to use more specific content types
                  -- for the result
                  :> Get '[FileType] FileResult

             :<|> "get_files" :> "thumbnail"
                  :> QueryParam "file_id" FileId
                  :> QueryParam "hash" SHA256
                  :> Header "Hydrus-Client-API-Access-Key" AccessKey
                  :> Header "Hydrus-Client-API-Session-Key" SessionKey
                  -- TODO: we might need to use more specific content types
                  -- for the result
                  :> Get '[FileType] FileResult

hydrusAPI :: Proxy HydrusAPI
hydrusAPI = Proxy

apiVersion_ :: ClientM APIVersion
requestNewPermissions_ :: Maybe Text -> Maybe (PercentJSON [Int]) -> ClientM AccessKeyResult
sessionKey_ :: Maybe Text -> Maybe SessionKey -> ClientM SessionKeyResult
verifyAccessKey_ :: Maybe Text -> Maybe SessionKey -> ClientM (PermissionInfoF [Int])
getService_ :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe SessionKey
  -> ClientM (ServiceInfoF Int)
getServices_ :: Maybe Text -> Maybe SessionKey -> ClientM (ServicesInfoF Int)
addFileJSON_ :: AddFileRequestBody
  -> Maybe Text -> Maybe SessionKey -> ClientM AddFileResult
archiveFiles_ :: Files -> Maybe Text -> Maybe SessionKey -> ClientM NoContent
unarchiveFiles_ :: Files -> Maybe Text -> Maybe SessionKey -> ClientM NoContent
getUrlFiles_ :: Maybe Text -> Maybe Bool -> Maybe AccessKey -> Maybe SessionKey
             -> ClientM UrlFilesInfo
getUrlInfo_ :: Maybe Text -> Maybe AccessKey -> Maybe SessionKey -> ClientM UrlInfo
addUrl_ :: AddUrlRequest -> Maybe AccessKey -> Maybe SessionKey -> ClientM AddUrlResult
searchTags_ :: Maybe Text
            -> Maybe Text -> Maybe Text -> Maybe TagDisplayType
            -> Maybe AccessKey -> Maybe SessionKey -> ClientM TagSeachResult
addTags_ :: AddTagsRequest -> Maybe AccessKey -> Maybe SessionKey -> ClientM NoContent
setNotes_ :: SetNotesRequestBody -> Maybe AccessKey -> Maybe SessionKey
          -> ClientM SetNotesResult
fileHashes_ :: Maybe (PercentJSON [SomeHash])
  -> Maybe (Some HashType)
  -> Maybe (Some HashType)
  -> Maybe Text
  -> Maybe SessionKey
  -> ClientM (KeyVal "hashes" (Map SomeHash SomeHash))
fileMetadata_ :: Maybe (PercentJSON [FileId])
  -> Maybe (PercentJSON [SHA256])
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe AccessKey
  -> Maybe SessionKey
  -> ClientM FileMetadataResult

apiVersion_ :<|> requestNewPermissions_
            :<|> sessionKey_
            :<|> verifyAccessKey_
            :<|> getService_
            :<|> getServices_
            :<|> addFileJSON_
            :<|> addFileBytes_
            :<|> deleteFiles_
            :<|> undeleteFiles_
            :<|> archiveFiles_
            :<|> unarchiveFiles_
            :<|> getUrlFiles_
            :<|> getUrlInfo_
            :<|> addUrl_
            :<|> associateUrl_
            :<|> cleanTags_
            :<|> searchTags_
            :<|> addTags_
            :<|> setNotes_
            :<|> deleteNotes_
            :<|> searchFiles_
            :<|> fileHashes_
            :<|> fileMetadata_
            :<|> file_
            :<|> thumbnail_
  = client (Proxy :: Proxy HydrusAPI)

newtype HydrusClient m = HydrusClient
  { hApiVersion :: m APIVersion
  }

hydrusClient :: forall m . RunClient m => HydrusClient m
hydrusClient = HydrusClient api_version
  where
    api_version :<|> _ = clientIn hydrusAPI (Proxy @m)
