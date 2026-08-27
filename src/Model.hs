----------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
module Model where
----------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
----------------------------------------------------------------------
import Miso.Lens.TH (makeLenses)
import Miso.String (MisoString)
----------------------------------------------------------------------
-- | Key into the video catalog, ordered so "up next" is deterministic
newtype VideoId = VideoId { _videoIx :: Int }
  deriving (Eq, Ord)
----------------------------------------------------------------------
makeLenses ''VideoId
----------------------------------------------------------------------
-- | One catalog entry; duration and dimensions are discovered at
-- runtime through the Media API ('onLoadedMetadataWith')
data Video = Video
  { _videoTitle    :: MisoString
  , _videoChannel  :: MisoString
  , _videoSrc      :: MisoString
  , _videoThumbT   :: Int    -- ^ media-fragment second shown as thumbnail
  , _videoCategory :: MisoString
  , _videoViews    :: Int
  , _videoAge      :: MisoString
  , _videoLikes    :: Int
  , _videoDuration :: Maybe Double
  , _videoWidth    :: Int
  , _videoHeight   :: Int
  } deriving (Eq)
----------------------------------------------------------------------
makeLenses ''Video
----------------------------------------------------------------------
mkVideo
  :: MisoString -> MisoString -> MisoString -> Int
  -> MisoString -> Int -> MisoString -> Int -> Video
mkVideo title channel src thumbT cat views age likes =
  Video title channel src thumbT cat views age likes Nothing 0 0
----------------------------------------------------------------------
data Rating = Liked | Disliked
  deriving (Eq)
----------------------------------------------------------------------
-- | Transient player feedback: what to flash over the video and where
data Flash
  = FlashPlay
  | FlashPause
  | FlashBack
  | FlashForward
  | FlashVolume Int  -- ^ new volume in percent
  deriving (Eq)
----------------------------------------------------------------------
-- | One comment under a video
data Comment = Comment
  { _commentAuthor :: MisoString
  , _commentText   :: MisoString
  , _commentLikes  :: Int
  , _commentAge    :: MisoString
  , _commentVote   :: Maybe Rating  -- ^ the viewer's vote on it
  } deriving (Eq)
----------------------------------------------------------------------
makeLenses ''Comment
----------------------------------------------------------------------
mkComment :: MisoString -> MisoString -> Int -> MisoString -> Comment
mkComment author txt likes age = Comment author txt likes age Nothing
----------------------------------------------------------------------
-- | Which sidebar feed the home grid is showing
data Feed
  = FeedHome
  | FeedShorts
  | FeedTrending
  | FeedLibrary
  | FeedHistory
  | FeedChannel MisoString
  deriving (Eq)
----------------------------------------------------------------------
data Model = Model
  { _modelVideos   :: Map VideoId Video
  , _modelCurrent  :: Maybe VideoId  -- ^ 'Nothing' means the home grid
  , _modelFeed     :: Feed
  , _modelHistory  :: [VideoId]      -- ^ watched videos, most recent first
  , _modelPlaying  :: Bool
  , _modelTime     :: Double
  , _modelVolume   :: Double
  , _modelMuted    :: Bool
  , _modelRate     :: Double
  , _modelTheater  :: Bool
  , _modelSidebar  :: Bool
  , _modelSearch   :: MisoString
  , _modelCategory :: MisoString
  , _modelHover    :: Maybe VideoId
  , _modelThumbSound :: Set VideoId  -- ^ thumbnails whose hover preview plays audio
  , _modelFlash    :: Maybe (Int, Flash)  -- ^ counter restarts the fade-out
  , _modelCanVolume :: Bool  -- ^ 'False' on iOS, where volume is read-only
  , _modelRatings  :: Map VideoId Rating
  , _modelSubs     :: Set MisoString
  , _modelComments :: Map VideoId [Comment]
  , _modelDraft    :: MisoString  -- ^ comment being typed
  } deriving (Eq)
----------------------------------------------------------------------
makeLenses ''Model
----------------------------------------------------------------------
mkModel :: [(Video, [Comment])] -> Model
mkModel entries = Model
  { _modelVideos   = Map.fromList (zip ids (fst <$> entries))
  , _modelCurrent  = Nothing
  , _modelFeed     = FeedHome
  , _modelHistory  = []
  , _modelPlaying  = False
  , _modelTime     = 0
  , _modelVolume   = 1
  , _modelMuted    = False
  , _modelRate     = 1
  , _modelTheater  = False
  , _modelSidebar  = True
  , _modelSearch   = ""
  , _modelCategory = "All"
  , _modelHover    = Nothing
  , _modelThumbSound = Set.empty
  , _modelFlash    = Nothing
  , _modelCanVolume = True
  , _modelRatings  = Map.empty
  , _modelSubs     = Set.empty
  , _modelComments = Map.fromList (zip ids (snd <$> entries))
  , _modelDraft    = ""
  }
  where
    ids = VideoId <$> [1 ..]
----------------------------------------------------------------------
