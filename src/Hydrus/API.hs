{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Hydrus.API  where

import Hydrus.API.Utils
import Hydrus.API.Types
import Hydrus.API.Internal

import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Servant.API
import Servant.Client
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Client
  (newManager, defaultManagerSettings, ManagerSettings (..),
   Request (requestHeaders), Manager)
import Data.Aeson
import GHC.Generics (Generic)
import Data.ByteString (ByteString, toStrict, writeFile)
import Data.Maybe (fromMaybe)
import Data.Aeson.Text (encodeToLazyText)
import qualified Servant.Client.Internal.HttpClient as I
import qualified Network.HTTP.Client                as HTTP
import Servant.Client.Core.RunClient (ClientF(RunRequest))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), filterM)
import Control.Monad.Except (MonadError (throwError), ExceptT, runExceptT)
import Data.Set (Set)
import Data.Text (pack, Text)
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M (toList)
import Data.Coerce (coerce)
import System.Posix.Types (FileID)
import Data.Bifunctor (bimap)
import Data.Constraint (Dict(..))

-- | Gets the current Hydrus API version.
apiVersion :: MonadHydrus m => m APIVersion
apiVersion = hoistH apiVersion_

-- | Register a new external program with the client. This requires the 'add
-- from api request' mini-dialog under services->review services to be open,
-- otherwise it will 403.
requestNewPermissions :: MonadHydrus m => Text -> Set BasicPermission -> m AccessKey
requestNewPermissions nm bps =
  access_key <$> hoistH (requestNewPermissions_ (Just nm) (Just bplist))
  where bplist = toBasicPermissionsParams bps

-- | Get a new session key.
sessionKey :: MonadHydrus m => m SessionKey
sessionKey = session_key <$> hoistH' sessionKey_

-- | Check your access key is valid. The request errors with codes 401/403/419
-- and some error text if the provided access/session key is invalid.
verifyAccessKey :: MonadHydrus m => m PermissionInfo
verifyAccessKey = toPermissionInfo <$> hoistH' verifyAccessKey_

-- | Ask the client about a specific service.
--
-- At least one of Add Files, Add Tags, Manage Pages, or Search Files permission
-- needed.
getService :: MonadHydrus m => Either ServiceName ServiceKey -> m ServiceInfo
getService x = toServiceInfo <$> hoistH' (uncurry getService_ arg)
  where arg = either (\n -> (Just n, Nothing)) (\k -> (Nothing, Just k)) x

-- | Ask the client about its file and tag services.
--
-- At least one of Add Files, Add Tags, Manage Pages, or Search Files permission
-- needed.
getServices :: MonadHydrus m => m ServicesInfo
getServices = toServicesInfo <$> hoistH' getServices_

-- | Tell the client to import a file.
--
-- The file to import is represented by either a file system path or its
-- raw data.
--
-- Import Files permission needed.
addFile :: MonadHydrus m => Either FilePath ByteString -> m AddFileResult
addFile =
  hoistH' . either (addFileJSON_ . AddFileRequestBody . pack) addFileBytes_

-- | Like `deleteFiles' Nothing Nothing`.
deleteFiles :: MonadHydrus m => Files -> m ()
deleteFiles fs = deleteFiles' fs Nothing Nothing

-- | Tell the client to send files to the trash.
--
-- Allows to specify the targeted file domains, defaulting to 'all my files',
-- and a textual reason for deletion.
--
-- Import Files permission needed.
deleteFiles' :: MonadHydrus m
            => Files -> Maybe FileDomain -> Maybe Text -> m ()
deleteFiles' fs mfd mfr =
  void $ hoistH' (deleteFiles_ (fs :*: mfd :*: (DeleteFilesReason <$> mfr)))

-- | Like `undeleteFiles' Nothing Nothing`.
undeleteFiles :: MonadHydrus m => Files -> m ()
undeleteFiles fs = undeleteFiles' fs Nothing

-- | Tell the client to send files to the trash.
--
-- Allows to specify the targeted file domains, defaulting to 'all my files'.
--
-- Import Files permission needed.
undeleteFiles' :: MonadHydrus m
            => Files -> Maybe FileDomain -> m ()
undeleteFiles' fs mfd = void $ hoistH' (undeleteFiles_ (fs :*: mfd))

-- | Tell the client to archive inboxed files.
--
-- Import Files permission needed.
archiveFiles :: MonadHydrus m => Files -> m ()
archiveFiles fs = void $ hoistH' (archiveFiles_ fs)

-- | Tell the client to re-imbox archived files.
--
-- Import Files permission needed.
unarchiveFiles :: MonadHydrus m => Files -> m ()
unarchiveFiles fs = void $ hoistH' (unarchiveFiles_ fs)

-- | Ask the client about an URL's files.
--
-- Import URLs permission required.
getUrlFiles' :: MonadHydrus m => URL -> Bool -> m UrlFilesInfo
getUrlFiles' url b = hoistH' (getUrlFiles_ (Just url) (Just b))

-- | `getUrlFiles url = getUrlFiles url False`.
getUrlFiles :: MonadHydrus m => URL -> m UrlFilesInfo
getUrlFiles = flip getUrlFiles' False

-- | Ask the client for information about a URL.
--
-- Import URLs permission required.
getUrlInfo :: MonadHydrus m => URL -> m UrlInfo
getUrlInfo url = hoistH' (getUrlInfo_ (Just url))

-- | Tell the client to 'import' a URL. This triggers the exact same routine as
-- drag-and-dropping a text URL onto the main client window.
--
-- Import URLs permission required.
addUrl :: MonadHydrus m => AddUrlRequest -> m AddUrlResult
addUrl r = hoistH' (addUrl_ r)

-- | Manage which URLs the client considers to be associated with which files.
-- The function gets a list of urls you want to associate with the file(s),
-- followed by a list of urls you want to disassociate from the file(s).
--
-- Import URLs permission needed.
associateUrl' :: MonadHydrus m => Files -> [URL] -> [URL] -> m ()
associateUrl' fs add del =
  void $ hoistH' (associateUrl_ (fs :*: KeyVal add :*: KeyVal del))

-- | `associateUrl fs xs = associateUrl' fs xs []`
associateUrl :: MonadHydrus m => Files -> [URL] -> m ()
associateUrl fs add = associateUrl' fs add []

-- | Ask the client about how it will see certain tags.
--
-- Add Tags permission needed.
cleanTags :: MonadHydrus m => [Tag] -> m [Tag]
cleanTags = fmap getVal . hoistH' . cleanTags_ . Just . PercentJSON

-- | Search the client for tags.
--
-- Search for Files permission needed.
searchTags :: MonadHydrus m => SearchTagsRequest -> m (Map Tag Int)
searchTags (KeyVal q :*: KeyVal fk :*: KeyVal tk :*: KeyVal d :*: Nil) =
  tagResultsToMap . getVal <$> hoistH' (searchTags_ (Just q) fk tk d)

-- | Make changes to the tags that files have.
--
-- Add Tags permission needed.
addTags :: MonadHydrus m
        => Files
        -> Either (Map ServiceKey [Tag]) (Map ServiceKey (Map ContentUpdateAction [Tag]))
        -> m ()
addTags fs x = void (hoistH' (addTags_ (fs :*: args)))
  where args = either (\x -> KeyVal (Just x) :*: KeyVal Nothing)
                      (\x -> KeyVal Nothing :*: KeyVal (Just x)) x

-- | Add or update notes associated with a file.
--
-- Add Notes permission needed.
setNotes :: MonadHydrus m => SetNotesRequest -> m [(NoteName, Text)]
setNotes r = M.toList . notes <$> hoistH' (setNotes_ r)

-- | Remove notes associated with a file.
--
-- Add Notes permission needed.
deleteNotes :: MonadHydrus m => [NoteName] -> Either SHA256 FileId -> m ()
deleteNotes nms f = void $ hoistH' (deleteNotes_ (KeyVal nms :*: plusFromEither f))

-- | Search for the client's files.
--
-- Search for Files permission needed. Additional search permission limits may apply.
searchFiles
  :: MonadHydrus m
  => SearchFilesRequest
  -> FileResultType a
  -> m [a]
searchFiles (tgs :*: KeyVal fs :*: KeyVal ts :*: KeyVal so :*: KeyVal sa :*: Nil) ty =
  typeCheckFileResult ty <$> hoistH' (searchFiles_ (Just (PercentJSON (toCNF tgs))) fs ts so sa x y)
  where (x, y) = fileResultTyToFlags ty

-- | Lookup file hashes from other hashes.
--
-- Search for Files permission needed.
fileHashes :: MonadHydrus m
  => HashType h -> [h] -> HashType r -> m (Map h r)
fileHashes sty hs rty = case (hashTyEq sty, hashTyEq rty) of
  (Dict, Dict) ->
    fmap getVal (hoistH' (fileHashes_
                          (Just (PercentJSON hs))
                          (Just (Some sty)) (Just (Some rty))))

-- | Get metadata about files in the client.
--
-- Search for Files permission needed. Additional search permission limits may apply.
fileMetadata :: MonadHydrus m => FileMetadataRequest -> FileMetadataType ty -> m [ty]
fileMetadata (fs :*: KeyVal newIds :*: Nil) ty =
  fmap (typeCheckMetadata ty) . metadata <$> hoistH' (fileMetadata_ ids hs newIds x y z w)
  where
    (ids, hs) = bimap (fmap PercentJSON) (fmap PercentJSON) (filesToMaybes fs)
    (x,y,z,w) = metaTypeToFlags ty

-- | Get a file.
--
-- Search for Files permission needed. Additional search permission limits may
-- apply.
file :: MonadHydrus m => Either FileId SHA256 -> m FileResult
file x = hoistH' (file_ fid fh)
  where
    (fid, fh) = either ((,Nothing) . Just) ((Nothing,) . Just) x

-- | Get a file.
--
-- Search for Files permission needed. Additional search permission limits may
-- apply.
thumbnail :: MonadHydrus m => Either FileId SHA256 -> m FileResult
thumbnail x = hoistH' (thumbnail_ fid fh)
  where
    (fid, fh) = either ((,Nothing) . Just) ((Nothing,) . Just) x
