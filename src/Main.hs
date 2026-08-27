----------------------------------------------------------------------
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
----------------------------------------------------------------------
-- | MisoTube, a little YouTube built with miso.
--
-- Everything on screen is rendered from Haskell: the home grid uses
-- @<video preload=metadata>@ elements as live thumbnails (real first
-- frames via media fragments, real durations via 'onLoadedMetadataWith',
-- muted previews on hover), and the watch page drives a chromeless
-- @<video>@ through miso's 'Miso.Media' API: play/pause, seeking,
-- volume, playback rate, theater mode, fullscreen and autoplay-next.
module Main where
----------------------------------------------------------------------
import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, void, when)
import Data.List (nub, sortOn)
import Data.Map ((!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Time.Clock (DiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
----------------------------------------------------------------------
import Miso
import Miso.Html hiding (title_)
import Miso.Html.Property
import Miso.Lens hiding (set)
import Miso.FFI.QQ (js)
import Miso.Media (Media(..), currentTime, duration, load, pause)
import qualified Miso.Media as Media
import Miso.Subscription.Util (createSub)
import qualified Miso.String as MS
----------------------------------------------------------------------
import qualified Miso.CSS as C
----------------------------------------------------------------------
import Model
----------------------------------------------------------------------
-- | The catalog: two local clips plus a shelf of open movies and
-- CC0 clips streamed from public servers.
theCatalog :: [(Video, [Comment])]
theCatalog =
  [ ( mkVideo "Haskell logo backloop \8212 ambient coding loop"
        "haskell-miso" "backloop_hs.webm" 2
        "Haskell" 1200000 "2 years ago" 48200
    , [ mkComment "@lambda_lounge" "I leave this on while writing Haskell. Productivity doubled." 420 "1 year ago"
      , mkComment "@recursion_fan" "Watched it loop 400 times, no space leak in sight" 187 "8 months ago"
      ]
    )
  , ( mkVideo "Building a WASM app with miso \8212 live dev session"
        "haskell-miso" "2023-09-04_001.mp4" 1
        "Haskell" 847000 "1 year ago" 31500
    , [ mkComment "@wasm_wizard" "GHC targeting WebAssembly still feels like magic" 233 "10 months ago"
      , mkComment "@newtype_nerd" "This is how I learned to set up miso, thank you!" 98 "5 months ago"
      ]
    )
  , ( mkVideo "Big Buck Bunny"
        "Blender Foundation" "https://media.w3.org/2010/05/bunny/movie.mp4" 18
        "Animation" 24000000 "16 years ago" 812000
    , [ mkComment "@bunnylover2008" "16 years later and still the best open source cinema ever made" 12000 "3 months ago"
      , mkComment "@render_farm" "Rendered on Blender before it was cool. Legends." 4800 "1 year ago"
      , mkComment "@butterflyfan" "the butterfly deserved better \128546" 9100 "2 years ago"
      ]
    )
  , ( mkVideo "Big Buck Bunny \8212 Official Trailer"
        "Blender Foundation" "https://media.w3.org/2010/05/bunny/trailer.mp4" 20
        "Animation" 9100000 "16 years ago" 402000
    , [ mkComment "@bigbuck_stan" "who\8217s here after watching the full movie?" 2100 "6 months ago"
      , mkComment "@trailerpark" "a trailer shorter than my attention span, perfect" 830 "1 year ago"
      ]
    )
  , ( mkVideo "Sintel \8212 Official Trailer"
        "Blender Foundation" "https://media.w3.org/2010/05/sintel/trailer.mp4" 40
        "Animation" 12000000 "14 years ago" 530000
    , [ mkComment "@dragonkeeper" "gorgeous even in trailer form. Scales!" 1400 "2 years ago" ]
    )
  , ( mkVideo "Elephants Dream \8212 Full Movie (2006)"
        "Blender Foundation" "https://archive.org/download/ElephantsDream/ed_1024_512kb.mp4" 180
        "Film" 5200000 "18 years ago" 176000
    , [ mkComment "@surrealcinema" "2006 and still the strangest thing Blender ever made" 3100 "1 year ago"
      , mkComment "@proog_defender" "Proog was right about everything" 1900 "7 months ago"
      ]
    )
  , ( mkVideo "Tears of Steel \8212 Full Movie (2012)"
        "Blender Foundation" "https://archive.org/download/Tears-of-Steel/tears_of_steel_720p.mp4" 220
        "Film" 6800000 "12 years ago" 244000
    , [ mkComment "@vfx_junkie" "the compositing holds up scary well for 2012" 2700 "1 year ago"
      , mkComment "@robot_rights" "forty years later and we still love our robot overlords" 1500 "4 months ago"
      ]
    )
  , ( mkVideo "Caminandes 1: Llama Drama"
        "Caminandes" "https://archive.org/download/Caminandes1LlamaDrama/01_llama_drama_1080p.mp4" 45
        "Shorts" 21000000 "11 years ago" 645000
    , [ mkComment "@llamallover" "llama drama is my favorite film genre" 5600 "1 year ago"
      , mkComment "@andes_andy" "the fence deserves its own spinoff" 3300 "9 months ago"
      ]
    )
  , ( mkVideo "Caminandes 2: Gran Dillama"
        "Caminandes" "https://archive.org/download/Caminandes2GranDillama/02_gran_dillama_1080p.mp4" 60
        "Shorts" 18000000 "11 years ago" 512000
    , [ mkComment "@grandillama" "Koro is the greatest character in open cinema" 4100 "2 years ago" ]
    )
  , ( mkVideo "Caminandes 3: Llamigos"
        "Caminandes" "https://archive.org/download/CaminandesLlamigos/Caminandes_%20Llamigos-1080p.mp4" 50
        "Shorts" 15000000 "8 years ago" 448000
    , [ mkComment "@penguin_patrol" "the penguin stole the whole show" 3900 "1 year ago"
      , mkComment "@snowdrift" "winter llamas > summer llamas" 2100 "6 months ago"
      ]
    )
  , ( mkVideo "Cosmos Laundromat: First Cycle"
        "Blender Foundation" "https://archive.org/download/CosmosLaundromatFirstCycle/Cosmos%20Laundromat%20-%20First%20Cycle%20%281080p%29.mp4" 240
        "Film" 4800000 "9 years ago" 152000
    , [ mkComment "@sheep_thrills" "a depressed sheep and a magic laundromat. peak cinema" 2600 "1 year ago"
      , mkComment "@first_cycle" "we NEED the rest of this movie" 6800 "2 years ago"
      ]
    )
  , ( mkVideo "We Are Going \8212 back to the Moon"
        "NASA" "https://archive.org/download/WeAreGoing/NASA%20-%20We%20Are%20Going%20%2814.05.2019%29.mp4" 30
        "Space" 6100000 "5 years ago" 230000
    , [ mkComment "@artemis_fan" "chills. every. time." 5100 "1 year ago"
      , mkComment "@moonbound" "my grandkids will watch this the way we watch Apollo footage" 3700 "8 months ago"
      ]
    )
  , ( mkVideo "Space Station module relocation \8212 timelapse"
        "NASA" "https://archive.org/download/SpaceStationPMMRelocation-4K/Leonardo%20move%20150x.ia.mp4" 20
        "Space" 2900000 "9 years ago" 87000
    , [ mkComment "@orbital_mechanic" "moving a whole room with a robot arm at 17500 mph, no big deal" 1800 "1 year ago" ]
    )
  , ( mkVideo "AT&T Archives: The UNIX Operating System (1982)"
        "Retro Tech" "https://archive.org/download/at-t-archives-the-unix-operating-system/AT%26T%20Archives%20The%20UNIX%20Operating%20System.mp4" 300
        "Tech" 3400000 "12 years ago" 128000
    , [ mkComment "@grep_enthusiast" "Ken and Dennis explaining pipes better than any modern tutorial" 8900 "2 years ago"
      , mkComment "@vim_or_die" "who else is watching this from a terminal" 6100 "1 year ago"
      , mkComment "@bell_labs_stan" "1982 narration makes \8216operating system\8217 sound majestic" 4200 "5 months ago"
      ]
    )
  , ( mkVideo "Interstellar \8212 Official Trailer #3"
        "Movie Trailers" "https://archive.org/download/interstellar-trailer-3/Interstellar_OfficialTrailer3_4K_51_prores.mp4" 60
        "Trailers" 31000000 "10 years ago" 890000
    , [ mkComment "@tars_talks" "Hans Zimmer\8217s organ should be classified as a controlled substance" 15000 "1 year ago"
      , mkComment "@cornfield_chase" "\8216we used to look up at the sky\8217 gets me every time" 9800 "6 months ago"
      ]
    )
  , ( mkVideo "Oppenheimer \8212 Official IMAX Trailer"
        "Movie Trailers" "https://archive.org/download/oppenheimer-trailer-e-4k-imax-prores/Oppenheimer-IMX_TLR-E-200723-2D_C_EN-XX_INT_IMAX5_4K_UP_20230504_prores.mp4" 45
        "Trailers" 28000000 "2 years ago" 760000
    , [ mkComment "@imax_purist" "I have watched this trailer more times than some full movies" 7200 "1 year ago"
      , mkComment "@nolan_notes" "the ticking. THE TICKING." 5600 "8 months ago"
      ]
    )
  , ( mkVideo "Dunkirk \8212 Main Trailer"
        "Movie Trailers" "https://archive.org/download/dunkirk-official-trailer-2/DunkirkMainTrailer.mp4" 60
        "Trailers" 19000000 "8 years ago" 520000
    , [ mkComment "@spitfire_spotter" "the Spitfire shots alone deserve an award" 4400 "2 years ago" ]
    )
  , ( mkVideo "Tenet \8212 Official Teaser"
        "Movie Trailers" "https://archive.org/download/tenet-official-teaser-trailer-4k-prores/Tenet_TLR-F1_S_EN-XX_INT-TD_51_4K_WR_20190723_MPEG_16-235_H264.mp4" 30
        "Trailers" 15000000 "5 years ago" 430000
    , [ mkComment "@entropy_reversed" "still don\8217t understand it. still hyped." 6600 "1 year ago"
      , mkComment "@palindrome_pete" "watching this teaser backwards just to be safe" 5100 "9 months ago"
      ]
    )
  , ( mkVideo "Bathyscaphe Trieste \8212 dive to Challenger Deep"
        "Deep Ocean Archive" "https://archive.org/download/50644UnderwaterDiverFootage/50644%20Underwater%20Diver%20Footage.mp4" 120
        "Deep Ocean" 1800000 "6 years ago" 64000
    , [ mkComment "@challenger_deep" "1960. two men. a steel sphere. the bottom of the world." 3200 "1 year ago"
      , mkComment "@pressure_test" "sixteen thousand psi outside and they just\8230 went" 2100 "7 months ago"
      ]
    )
  , ( mkVideo "Jacques Piccard \8212 3700 meters below the sea"
        "Deep Ocean Archive" "https://archive.org/download/20754JacquesPiccard3700MetersBelowTheSeaMos/20754%20Jacques%20Piccard%203700%20Meters%20Below%20The%20Sea_mos.mp4" 300
        "Deep Ocean" 947000 "5 years ago" 38000
    , [ mkComment "@bathyscaphe_bob" "Piccard was built different" 1500 "1 year ago" ]
    )
  , ( mkVideo "The Sea: Mysteries of the Deep (1970s)"
        "Deep Ocean Archive" "https://archive.org/download/21094theseamysteriesofthedeep_202003/21094%20The%20Sea%20Mysteries%20Of%20The%20Deep.mp4" 240
        "Deep Ocean" 1100000 "4 years ago" 42000
    , [ mkComment "@retro_oceanic" "1970s narration somehow makes the abyss feel cozy" 890 "3 months ago"
      , mkComment "@abyssal_annie" "the anglerfish jumpscare was not in the syllabus" 1300 "1 year ago"
      ]
    )
  ]
----------------------------------------------------------------------
theRates :: [Double]
theRates = [1, 1.25, 1.5, 2, 0.5, 0.75]
----------------------------------------------------------------------
-- | Action
data Action
  = ActionInit
  | ActionSetSidebar Bool
  | ActionHome
  | ActionFeed Feed
  | ActionOpen VideoId
  | ActionMeta VideoId Media
  | ActionSetMeta VideoId Double Int Int
  | ActionHover (Maybe VideoId)
  | ActionToggleThumbSound VideoId
  | ActionTogglePlay
  | ActionCanPlay
  | ActionAskTime Media
  | ActionSetTime Double
  | ActionSeek MisoString
  | ActionSkip Double
  | ActionVolume MisoString
  | ActionBumpVolume Double
  | ActionSetCanVolume Bool
  | ActionToggleMute
  | ActionCycleRate
  | ActionToggleTheater
  | ActionFullscreen
  | ActionNext
  | ActionRate VideoId Rating
  | ActionSubscribe MisoString
  | ActionDraft MisoString
  | ActionPostComment
  | ActionRateComment VideoId Int Rating
  | ActionSearch MisoString
  | ActionCategory MisoString
  | ActionToggleSidebar
----------------------------------------------------------------------
-- | View
handleView :: context -> props -> Model -> View context Model Action
handleView _ _ model = div_ [ class_ "app" ]
  [ viewTopbar model
  , case model ^. modelCurrent of
      Nothing  -> viewHome model
      -- on the watch page the sidebar becomes an overlay drawer
      Just vid -> div_ [ class_ drawerClass ]
        [ viewSidebar model, viewScrim, viewWatch model vid ]
  ]
  where
    drawerClass
      | model ^. modelSidebar = "shell drawer"
      | otherwise = "shell drawer rail-closed"
----------------------------------------------------------------------
viewTopbar :: Model -> View context Model Action
viewTopbar model = header_ [ class_ "topbar" ]
  [ div_ [ class_ "topbar-side" ]
    [ button_ [ class_ "icon-btn", title_ "Toggle sidebar", onClick ActionToggleSidebar ] [ "☰" ]
    , span_ [ class_ "logo", onClick (ActionFeed FeedHome) ]
      [ span_ [ class_ "logo-mark" ] [ "▶" ]
      , span_ [ class_ "logo-name" ] [ "MisoTube", sup_ [ class_ "logo-sup" ] [ "WASM" ] ]
      ]
    ]
  , div_ [ class_ "searchbox" ] $
    [ input_
      [ type_ "text"
      , placeholder_ "Search"
      , value_ (model ^. modelSearch)
      , onInput ActionSearch
      ]
    ] <>
    [ button_ [ class_ "search-clear", title_ "Clear search", onClick (ActionSearch "") ] [ "✕" ]
    | not (MS.null (model ^. modelSearch))
    ] <>
    [ span_ [ class_ "search-btn" ] [ "⌕" ] ]
  , div_ [ class_ "topbar-side" ]
    [ a_ [ class_ "gh-link", href_ "https://github.com/haskell-miso/miso-video" ] [ "GitHub" ]
    , span_ [ class_ "user-avatar", title_ "λ you" ] [ "λ" ]
    ]
  ]
----------------------------------------------------------------------
viewHome :: Model -> View context Model Action
viewHome model = div_ [ class_ shellClass ]
  [ viewSidebar model
  , viewScrim
  , main_ [ class_ "content" ]
    [ case model ^. modelFeed of
        FeedHome -> viewChips model
        FeedTrending -> viewChips model
        FeedChannel ch -> viewChannelHeader model ch
        feed -> h2_ [ class_ "feed-title" ] [ text (feedTitle feed) ]
    , if null hits
      then div_ [ class_ "empty" ] [ text emptyMsg ]
      else div_ [ class_ "grid" ] (viewCard model <$> hits)
    ]
  ]
  where
    shellClass
      | model ^. modelSidebar = "shell"
      | otherwise = "shell rail-closed"
    hits = visibleVideos model
    quietSearch = MS.null (MS.strip (model ^. modelSearch))
    emptyMsg
      | model ^. modelFeed == FeedLibrary && quietSearch =
          "Nothing in your library yet — tap 👍 on a video to save it here"
      | model ^. modelFeed == FeedHistory && quietSearch =
          "No watch history yet — go watch something"
      | otherwise = "No results for \"" <> model ^. modelSearch <> "\""

    feedTitle = \case
      FeedShorts -> "Shorts"
      FeedLibrary -> "Library"
      FeedHistory -> "History"
      _ -> ""
----------------------------------------------------------------------
-- | Channel page header shown when browsing one channel's uploads
viewChannelHeader :: Model -> MS.MisoString -> View context Model Action
viewChannelHeader model ch = div_ [ class_ "chan-header" ]
  [ viewAvatar "avatar xl" ch
  , div_ [ class_ "chan-text" ]
    [ span_ [ class_ "chan-name" ] [ text ch ]
    , span_ [ class_ "chan-sub" ] [ text (ms uploads <> " uploads") ]
    ]
  , viewSubscribe model ch
  ]
  where
    uploads = length
      [ () | v <- Map.elems (model ^. modelVideos), v ^. videoChannel == ch ]
----------------------------------------------------------------------
-- | Feed, category and search filter over the catalog
visibleVideos :: Model -> [(VideoId, Video)]
visibleVideos model = case model ^. modelFeed of
  FeedHome -> hits
  FeedShorts -> [ kv | kv@(_, v) <- hits, v ^. videoCategory == "Shorts" ]
  FeedTrending -> sortOn (negate . (^. videoViews) . snd) hits
  FeedLibrary -> [ kv | kv@(vid, _) <- hits, (model ^. modelRatings) !? vid == Just Liked ]
  FeedHistory ->
    [ (vid, v)
    | vid <- model ^. modelHistory
    , Just v <- [(model ^. modelVideos) !? vid]
    , keep (vid, v)
    ]
  FeedChannel ch -> [ kv | kv@(_, v) <- hits, v ^. videoChannel == ch ]
  where
    hits = filter keep (Map.toList (model ^. modelVideos))
    keep (_, v) =
      (cat == "All" || v ^. videoCategory == cat) &&
      (MS.null q
        || q `MS.isInfixOf` MS.toLower (v ^. videoTitle)
        || q `MS.isInfixOf` MS.toLower (v ^. videoChannel))
    cat = model ^. modelCategory
    q = MS.toLower (MS.strip (model ^. modelSearch))
----------------------------------------------------------------------
-- | Backdrop behind the sidebar when it floats as a drawer (watch
-- page, phones); tapping it closes the drawer
viewScrim :: View context Model Action
viewScrim = div_ [ class_ "scrim", onClick (ActionSetSidebar False) ] []
----------------------------------------------------------------------
viewSidebar :: Model -> View context Model Action
viewSidebar model = nav_ [ class_ "rail" ]
  [ railItem (feed == FeedHome) "🏠" "Home" (ActionFeed FeedHome)
  , railItem (feed == FeedShorts) "🎞" "Shorts" (ActionFeed FeedShorts)
  , railItem (feed == FeedTrending) "🔥" "Trending" (ActionFeed FeedTrending)
  , railItem (feed == FeedLibrary) "📚" "Library" (ActionFeed FeedLibrary)
  , railItem (feed == FeedHistory) "🕒" "History" (ActionFeed FeedHistory)
  , div_ [ class_ "rail-sec" ]
    (span_ [ class_ "rail-head" ] [ "Subscriptions" ] : (chanItem <$> channels))
  ]
  where
    feed = model ^. modelFeed
    railItem active icon label action = span_
      [ class_ (if active then "rail-item active" else "rail-item")
      , onClick action
      ]
      [ span_ [ class_ "rail-icon" ] [ text icon ], text label ]

    channels = nub [ v ^. videoChannel | v <- Map.elems (model ^. modelVideos) ]

    chanItem ch = span_
      [ class_ (if feed == FeedChannel ch then "rail-item active" else "rail-item")
      , onClick (ActionFeed (FeedChannel ch))
      ]
      [ viewAvatar "avatar sm" ch, text ch ]
----------------------------------------------------------------------
viewChips :: Model -> View context Model Action
viewChips model = div_ [ class_ "chips" ] (chip <$> cats)
  where
    cats = "All" : nub [ v ^. videoCategory | v <- Map.elems (model ^. modelVideos) ]
    chip cat = button_
      [ class_ (if model ^. modelCategory == cat then "chip active" else "chip")
      , onClick (ActionCategory cat)
      ]
      [ text cat ]
----------------------------------------------------------------------
viewCard :: Model -> (VideoId, Video) -> View context Model Action
viewCard model (vid, v) = div_ [ class_ "card", onClick (ActionOpen vid) ]
  [ viewThumb "thumb" (vid `Set.member` (model ^. modelThumbSound)) vid v
  , div_ [ class_ "card-meta" ]
    [ viewAvatar "avatar" (v ^. videoChannel)
    , div_ [ class_ "card-text" ]
      [ h3_ [ class_ "card-title" ] [ text (v ^. videoTitle) ]
      , p_ [ class_ "card-sub" ] [ text (v ^. videoChannel) ]
      , p_ [ class_ "card-sub" ] [ text (fmtViews v <> " • " <> v ^. videoAge) ]
      ]
    ]
  ]
----------------------------------------------------------------------
-- | The thumbnail box: a real @<video>@ for the live preview, the
-- duration badge, and a speaker button that unmutes the preview.
-- Hover is tracked on this wrapper (not the video) with
-- mouseenter\/mouseleave so moving onto the speaker button doesn't
-- read as leaving the thumbnail and reset the preview.
viewThumb :: MisoString -> Bool -> VideoId -> Video -> View context Model Action
viewThumb cls soundOn vid v = div_
  [ class_ cls
  , onMouseEnter (ActionHover (Just vid))
  , onMouseLeave (ActionHover Nothing)
  ]
  [ viewThumbVideo soundOn vid v
  , button_
    [ class_ "thumb-sound"
    , title_ (if soundOn then "Mute preview" else "Unmute preview")
    , onClickWithOptions stopPropagation (ActionToggleThumbSound vid)
    ]
    [ text (if soundOn then "🔊" else "🔇") ]
  , span_ [ class_ "dur" ] [ text (maybe "•••" fmtTime (v ^. videoDuration)) ]
  ]
----------------------------------------------------------------------
-- | The preview video itself: the media fragment picks the poster
-- frame, metadata gives us the true duration, and hovering plays it
-- (muted by default, or with sound once the speaker button is on).
viewThumbVideo :: Bool -> VideoId -> Video -> View context Model Action
viewThumbVideo soundOn vid v = video_
  [ id_ (thumbDomId vid)
  , src_ (v ^. videoSrc <> "#t=" <> ms (v ^. videoThumbT))
  , preload_ "metadata"
  , muted_ (not soundOn)
  , loop_ True
  , boolProp "playsinline" True
  , onLoadedMetadataWith (ActionMeta vid)
  ]
  []
----------------------------------------------------------------------
viewAvatar :: MisoString -> MisoString -> View context Model Action
viewAvatar cls ch = span_
  [ class_ cls
  , C.style_ [ C.backgroundColor (channelColor ch) ]
  ]
  [ text (MS.take 1 ch) ]
----------------------------------------------------------------------
viewWatch :: Model -> VideoId -> View context Model Action
viewWatch model vid = case (model ^. modelVideos) !? vid of
  Nothing -> viewHome model
  Just v -> div_ [ class_ watchClass ]
    [ div_ [ class_ "watch-main" ]
      [ viewPlayer model vid v
      , h1_ [ class_ "w-title" ] [ text (v ^. videoTitle) ]
      , viewChannelRow model vid v
      , viewDescription model v
      , viewComments model vid
      ]
    , div_ [ class_ "upnext" ]
        (span_ [ class_ "upnext-head" ] [ "Up next" ] : (upnextRow model <$> others))
    ]
  where
    watchClass
      | model ^. modelTheater = "watch theater"
      | otherwise = "watch"
    others =
      [ kv | kv@(k, _) <- Map.toList (model ^. modelVideos), k /= vid ]
----------------------------------------------------------------------
viewPlayer :: Model -> VideoId -> Video -> View context Model Action
viewPlayer model vid v = div_ [ id_ "player-shell", class_ shellClass ]
  [ video_
    [ id_ "player"
    , src_ (v ^. videoSrc)
    , volume_ (model ^. modelVolume)
    , muted_ (model ^. modelMuted)
    , boolProp "playsinline" True
    , onClick ActionTogglePlay
    , onCanPlay ActionCanPlay
    , onTimeUpdateWith ActionAskTime
    , onLoadedMetadataWith (ActionMeta vid)
    , onEnded ActionNext
    ]
    []
  , div_ [ class_ "bigplay", onClick ActionTogglePlay ] [ span_ [] [ "▶" ] ]
  , viewFlash (model ^. modelFlash)
  , div_ [ class_ "controls" ]
    [ input_
      [ class_ "seek"
      , type_ "range"
      , min_ "0"
      , max_ (ms totalSecs)
      , step_ "0.1"
      , value_ (ms time)
      , onInput ActionSeek
      , C.style_ [ sliderFill "#f03" "rgba(255,255,255,0.25)" time totalSecs ]
      ]
    , div_ [ class_ "ctrl-row" ] $
      [ ctrl "play-btn" playTitle (if playing then "❚❚" else "▶") ActionTogglePlay
      , ctrl "" "Next" "⏭" ActionNext
      , ctrl "" muteTitle (if muted then "🔇" else "🔊") ActionToggleMute
      ] <>
      -- no slider where the platform ignores volume writes (iOS)
      [ input_
        [ class_ "vol"
        , type_ "range"
        , min_ "0"
        , max_ "1"
        , step_ "0.02"
        , value_ (ms vol)
        , onInput ActionVolume
        , C.style_ [ sliderFill "#fff" "rgba(255,255,255,0.25)" (if muted then 0 else vol) 1 ]
        ]
      | model ^. modelCanVolume
      ] <>
      [ span_ [ class_ "time" ]
        [ text (fmtTime time <> " / " <> maybe "--:--" fmtTime (v ^. videoDuration)) ]
      , span_ [ class_ "ctrl-spacer" ] []
      , ctrl "rate-btn" "Playback speed" (fmtRate (model ^. modelRate)) ActionCycleRate
      , ctrl "" "Theater mode" "▭" ActionToggleTheater
      , ctrl "" "Fullscreen" "⛶" ActionFullscreen
      ]
    ]
  ]
  where
    playing = model ^. modelPlaying
    muted = model ^. modelMuted
    vol = model ^. modelVolume
    time = model ^. modelTime
    totalSecs = fromMaybe 0 (v ^. videoDuration)
    shellClass = "player-shell" <> (if playing then "" else " paused")
    playTitle = if playing then "Pause" else "Play"
    muteTitle = if muted then "Unmute" else "Mute"

    ctrl cls name glyph action = button_
      [ class_ ("ctrl-btn " <> cls), title_ name, onClick action ]
      [ text glyph ]
----------------------------------------------------------------------
-- | Transient feedback flashed over the video: play/pause and volume
-- in the middle, arrow-key skips as "5s" bubbles on the sides. The
-- animation-name alternates with the flash counter so a repeated
-- action restarts the fade-out even though the DOM node is reused.
viewFlash :: Maybe (Int, Flash) -> View context Model Action
viewFlash Nothing = text ""
viewFlash (Just (n, f)) = div_
  [ class_ (MS.unwords (["flash"] <> side <> [parity])) ]
  [ span_ [ class_ "flash-bubble" ] [ text glyph ] ]
  where
    parity = if even n then "flash-a" else "flash-b"
    -- no side class means a centered pill (volume)
    (side, glyph) = case f of
      FlashPlay      -> (["mid"], "▶")
      FlashPause     -> (["mid"], "❚❚")
      FlashBack      -> (["left"], "◂◂ 5s")
      FlashForward   -> (["right"], "5s ▸▸")
      FlashVolume 0  -> ([], "🔇")
      FlashVolume p  -> ([], "🔊 " <> ms p <> "%")
----------------------------------------------------------------------
-- | Paint the played part of a range input by hand
sliderFill :: MisoString -> MisoString -> Double -> Double -> C.Style
sliderFill filled track val total = "background" =: mconcat
  [ "linear-gradient(to right, ", filled, " ", pct, "%, ", track, " ", pct, "%)" ]
  where
    pct = if total <= 0 then "0" else ms (100 * val / total)
----------------------------------------------------------------------
viewChannelRow :: Model -> VideoId -> Video -> View context Model Action
viewChannelRow model vid v = div_ [ class_ "chanrow" ]
  [ viewAvatar "avatar lg" ch
  , div_ [ class_ "chan-text" ]
    [ span_
      [ class_ "chan-name chan-link", onClick (ActionFeed (FeedChannel ch)) ]
      [ text ch ]
    , span_ [ class_ "chan-sub" ] [ text (v ^. videoCategory <> " channel") ]
    ]
  , viewSubscribe model ch
  , div_ [ class_ "like-group" ]
    [ button_
      [ class_ (if rating == Just Liked then "like-btn active" else "like-btn")
      , title_ "I like this"
      , onClick (ActionRate vid Liked)
      ]
      [ text ("👍 " <> fmtCount likes) ]
    , span_ [ class_ "like-sep" ] []
    , button_
      [ class_ (if rating == Just Disliked then "like-btn active" else "like-btn")
      , title_ "Not for me"
      , onClick (ActionRate vid Disliked)
      ]
      [ "👎" ]
    ]
  ]
  where
    ch = v ^. videoChannel
    rating = (model ^. modelRatings) !? vid
    likes = v ^. videoLikes + (if rating == Just Liked then 1 else 0)
----------------------------------------------------------------------
viewSubscribe :: Model -> MS.MisoString -> View context Model Action
viewSubscribe model ch = button_
  [ class_ (if subscribed then "subscribe subscribed" else "subscribe")
  , onClick (ActionSubscribe ch)
  ]
  [ text (if subscribed then "Subscribed ✓" else "Subscribe") ]
  where
    subscribed = ch `Set.member` (model ^. modelSubs)
----------------------------------------------------------------------
viewDescription :: Model -> Video -> View context Model Action
viewDescription model v = div_ [ class_ "desc" ]
  [ p_ [ class_ "desc-stats" ] [ text (fmtViews v <> " • " <> v ^. videoAge) ]
  , p_ [] [ text techLine ]
  , p_ []
    [ "Source: "
    , a_ [ href_ (v ^. videoSrc) ] [ text (v ^. videoSrc) ]
    ]
  , p_ [ class_ "desc-note" ]
    [ "Every pixel of this page is rendered from Haskell compiled to WebAssembly with "
    , a_ [ href_ "https://github.com/dmjio/miso" ] [ "miso" ]
    , ". The player chrome — seek bar, volume, playback rate, theater mode, fullscreen, "
    , "autoplay-next and the live thumbnails on the home page — drives the "
    , "HTMLMediaElement through miso's Media API."
    ]
  ]
  where
    techLine = mconcat
      [ resolution
      , maybe "" (\d -> " • " <> fmtTime d) (v ^. videoDuration)
      , " • ", fmtRate (model ^. modelRate), " speed"
      ]
    resolution
      | v ^. videoWidth > 0 =
          ms (v ^. videoWidth) <> " × " <> ms (v ^. videoHeight)
      | otherwise = "resolution loading…"
----------------------------------------------------------------------
-- | Comment section: baked-in comments per video plus whatever the
-- viewer posts this session
viewComments :: Model -> VideoId -> View context Model Action
viewComments model vid = div_ [ class_ "comments" ] $
  [ span_ [ class_ "comments-head" ]
      [ text (ms (length cs) <> " Comments") ]
  , div_ [ class_ "addc" ]
    [ viewAvatar "avatar" "λ you"
    , input_
      [ class_ "cinput"
      , type_ "text"
      , placeholder_ "Add a comment…"
      , value_ draft
      , onInput ActionDraft
      ]
    , button_
      [ class_ "cpost"
      , boolProp "disabled" (MS.null (MS.strip draft))
      , onClick ActionPostComment
      ]
      [ "Comment" ]
    ]
  ] <> (viewComment <$> zip [0 ..] cs)
  where
    draft = model ^. modelDraft
    cs = Map.findWithDefault [] vid (model ^. modelComments)

    viewComment (ix, c) = div_ [ class_ "comment" ]
      [ viewAvatar "avatar" (MS.dropWhile (== '@') (c ^. commentAuthor))
      , div_ [ class_ "cbody" ]
        [ p_ [ class_ "cmeta" ]
          [ span_ [ class_ "cauthor" ] [ text (c ^. commentAuthor) ]
          , span_ [ class_ "cage" ] [ text (c ^. commentAge) ]
          ]
        , p_ [ class_ "ctext" ] [ text (c ^. commentText) ]
        , div_ [ class_ "cfoot" ]
          [ button_
            [ class_ (voteClass Liked)
            , title_ "Like"
            , onClick (ActionRateComment vid ix Liked)
            ]
            [ text ("👍 " <> fmtCount likes) ]
          , button_
            [ class_ (voteClass Disliked)
            , title_ "Dislike"
            , onClick (ActionRateComment vid ix Disliked)
            ]
            [ "👎" ]
          ]
        ]
      ]
      where
        likes = c ^. commentLikes + (if c ^. commentVote == Just Liked then 1 else 0)
        voteClass r
          | c ^. commentVote == Just r = "cvote active"
          | otherwise = "cvote"
----------------------------------------------------------------------
upnextRow :: Model -> (VideoId, Video) -> View context Model Action
upnextRow model (vid, v) = div_ [ class_ "up-row", onClick (ActionOpen vid) ]
  [ viewThumb "up-thumb" (vid `Set.member` (model ^. modelThumbSound)) vid v
  , div_ [ class_ "up-text" ]
    [ h4_ [ class_ "up-title" ] [ text (v ^. videoTitle) ]
    , p_ [ class_ "up-sub" ] [ text (v ^. videoChannel) ]
    , p_ [ class_ "up-sub" ] [ text (fmtViews v <> " • " <> v ^. videoAge) ]
    ]
  ]
----------------------------------------------------------------------
-- | Formatters
fmtTime :: Double -> MisoString
fmtTime secs = ms (formatTime defaultTimeLocale fmt (realToFrac secs :: DiffTime))
  where
    fmt = if secs >= 3600 then "%h:%02M:%02S" else "%M:%02S"

fmtRate :: Double -> MisoString
fmtRate r = trim (ms r) <> "×"
  where
    trim s = fromMaybe s (MS.stripSuffix ".0" s)

fmtViews :: Video -> MisoString
fmtViews v = fmtCount (v ^. videoViews) <> " views"

fmtCount :: Int -> MisoString
fmtCount n
  | n >= 1000000 = decimals (n `div` 100000) <> "M"
  | n >= 1000 = ms (n `div` 1000) <> "K"
  | otherwise = ms n
  where
    decimals tenths =
      let (a, b) = tenths `divMod` 10
      in if b == 0 then ms a else ms a <> "." <> ms b

channelColor :: MisoString -> C.Color
channelColor ch = C.hsl hue 55 45
  where
    hue = (47 * sum (fromEnum <$> MS.unpack ch)) `mod` 360

thumbDomId :: VideoId -> MisoString
thumbDomId vid = "thumb-" <> ms (vid ^. videoIx)
----------------------------------------------------------------------
-- | Play a media element by id. play() returns a promise that rejects
-- with AbortError when a load() or src change interrupts it (hover
-- previews, switching videos); swallow it instead of letting it reach
-- the console as an unhandled rejection.
playQuiet :: MisoString -> IO ()
playQuiet domId = [js|
  var el = document.getElementById(${domId});
  if (el) {
    var p = el.play();
    if (p && p.catch) { p.catch(function(){}); }
  }
|]
----------------------------------------------------------------------
-- | Watch-page keyboard shortcuts: space toggles play/pause, the
-- left/right arrows skip 5 seconds and up/down nudge the volume.
-- Started when a video opens and stopped on leaving the watch page.
-- The callback is synchronous so preventDefault can stop space and
-- the arrows from scrolling the page; keys aimed at form fields
-- (search box, comment input, the sliders) are left alone.
watchKeys :: Sub Action
watchKeys sink = createSub acquire release sink
  where
    acquire = do
      cb <- syncCallback1 onKey
      win <- jsg "window"
      void $ win # "addEventListener" $ ("keydown" :: MisoString, cb)
      pure cb
    release cb = do
      win <- jsg "window"
      void $ win # "removeEventListener" $ ("keydown" :: MisoString, cb)
    onKey ev = do
      mTag <- fromJSVal =<< ((! "tagName") =<< ev ! "target")
      mCode <- fromJSVal =<< ev ! "keyCode"
      let typing = maybe False (`elem` formTags) mTag
          formTags = [ "INPUT", "TEXTAREA", "SELECT" ] :: [MisoString]
          fire action = do
            eventPreventDefault ev
            sink action
      unless typing $ forM_ mCode $ \code -> case code :: Int of
        32 -> fire ActionTogglePlay
        37 -> fire (ActionSkip (-5))
        39 -> fire (ActionSkip 5)
        38 -> fire (ActionBumpVolume 0.1)
        40 -> fire (ActionBumpVolume (-0.1))
        _  -> pure ()
----------------------------------------------------------------------
-- | iOS WebKit ignores writes to @HTMLMediaElement.volume@ (only the
-- hardware buttons control it), so probe a detached element and hide
-- the volume slider when setting it doesn't stick.
probeVolume :: IO Bool
probeVolume = do
  doc <- jsg "document"
  el <- doc # ("createElement" :: MisoString) $ ("video" :: MisoString)
  set "volume" (0.5 :: Double) (Object el)
  vol <- fromJSValUnchecked =<< el ! "volume"
  pure (vol == (0.5 :: Double))
----------------------------------------------------------------------
-- | Update
handleUpdate :: Action -> Effect context props Model Action
handleUpdate = \case
  ActionInit -> do
    -- start with the sidebar closed on phone-sized screens
    io (ActionSetSidebar . (>= 800) <$> windowInnerWidth)
    io (ActionSetCanVolume <$> probeVolume)
  ActionSetSidebar open ->
    modelSidebar .= open
  ActionHome -> do
    modelCurrent .= Nothing
    modelPlaying .= False
    modelTime .= 0
    modelFlash .= Nothing
    stopSub ("watch-keys" :: MisoString)
  ActionFeed feed -> do
    -- switching feeds starts from a clean filter state
    modelFeed .= feed
    modelSearch .= ""
    modelCategory .= "All"
    issue ActionHome
    -- reopen the home sidebar when the screen is wide enough
    issue ActionInit
  ActionOpen vid -> do
    mCurrent <- use modelCurrent
    when (mCurrent /= Just vid) (modelTime .= 0)
    modelCurrent .= Just vid
    modelPlaying .= True
    modelHover .= Nothing
    modelHistory %= ((vid :) . filter (/= vid))
    modelDraft .= ""
    modelFlash .= Nothing
    -- the watch page starts with the drawer closed
    modelSidebar .= False
    -- keyboard shortcuts only live while a video is open
    startSub ("watch-keys" :: MisoString) watchKeys
    -- opening from deep in a scrolled feed starts at the top
    io_ [js| window.scrollTo(0, 0); |]
  ActionMeta vid media ->
    io (ActionSetMeta vid <$> duration media <*> Media.videoWidth media <*> Media.videoHeight media)
  ActionSetMeta vid d w h ->
    modelVideos %= Map.adjust upVideo vid
    where
      upVideo v = v & videoDuration ?~ d
                    & videoWidth .~ w
                    & videoHeight .~ h
  ActionHover (Just vid) -> do
    mHover <- use modelHover
    when (mHover /= Just vid) $ do
      modelHover .= Just vid
      io_ (playQuiet (thumbDomId vid))
  ActionHover Nothing -> do
    mHover <- use modelHover
    forM_ mHover $ \vid -> do
      modelHover .= Nothing
      -- back to muted, so the next hover always starts silently
      modelThumbSound %= Set.delete vid
      -- reloading rewinds the preview back to its poster frame
      io_ (withThumb load vid)
  ActionToggleThumbSound vid ->
    modelThumbSound %= \s -> if vid `Set.member` s then Set.delete vid s else Set.insert vid s
  ActionTogglePlay -> do
    playing <- use modelPlaying
    modelPlaying .= not playing
    flash (if playing then FlashPause else FlashPlay)
    io_ (if playing then withPlayer pause else playQuiet "player")
  ActionCanPlay -> do
    -- a fresh source resets the element, resync rate and playback
    rate <- use modelRate
    playing <- use modelPlaying
    io_ $ do
      el <- getElementById "player"
      set "playbackRate" rate (Object el)
      when playing (playQuiet "player")
  ActionAskTime media ->
    io (ActionSetTime <$> currentTime media)
  ActionSetTime secs ->
    modelTime .= secs
  ActionSeek str ->
    forM_ (MS.fromMisoStringEither str) $ \secs -> do
      modelTime .= secs
      io_ $ do
        el <- getElementById "player"
        set "currentTime" (secs :: Double) (Object el)
  ActionSkip delta -> do
    videos <- use modelVideos
    mCurrent <- use modelCurrent
    forM_ mCurrent $ \vid -> do
      time <- use modelTime
      -- clamp to the clip; the browser clamps to duration anyway
      let mDur = (^. videoDuration) =<< (videos !? vid)
          secs = maybe id min mDur (max 0 (time + delta))
      modelTime .= secs
      flash (if delta < 0 then FlashBack else FlashForward)
      io_ $ do
        el <- getElementById "player"
        set "currentTime" (secs :: Double) (Object el)
  ActionVolume str ->
    forM_ (MS.fromMisoStringEither str) $ \vol -> do
      modelVolume .= vol
      modelMuted .= (vol <= (0 :: Double))
  ActionBumpVolume delta -> do
    vol <- use modelVolume
    muted <- use modelMuted
    -- nudging the volume up also unmutes
    let next = min 1 (max 0 ((if muted then 0 else vol) + delta))
    modelVolume .= next
    modelMuted .= (next <= 0)
    flash (FlashVolume (round (100 * next)))
  ActionSetCanVolume can ->
    modelCanVolume .= can
  ActionToggleMute ->
    modelMuted %= not
  ActionCycleRate -> do
    rate <- use modelRate
    let next = nextRate rate
    modelRate .= next
    io_ $ do
      el <- getElementById "player"
      set "playbackRate" next (Object el)
  ActionToggleTheater ->
    modelTheater %= not
  ActionFullscreen ->
    io_ $ do
      el <- getElementById "player-shell"
      void $ el # ("requestFullscreen" :: MisoString) $ ()
  ActionNext -> do
    -- advance through the catalog, wrapping at the end
    videos <- use modelVideos
    mCurrent <- use modelCurrent
    let mNext = (fst <$> (flip Map.lookupGT videos =<< mCurrent))
            <|> (fst <$> Map.lookupMin videos)
    forM_ mNext (issue . ActionOpen)
  ActionRate vid rating ->
    modelRatings %= Map.alter toggle vid
    where
      toggle old = if old == Just rating then Nothing else Just rating
  ActionSubscribe ch ->
    modelSubs %= \subs ->
      if ch `Set.member` subs then Set.delete ch subs else Set.insert ch subs
  ActionDraft str ->
    modelDraft .= str
  ActionPostComment -> do
    draft <- MS.strip <$> use modelDraft
    mCurrent <- use modelCurrent
    unless (MS.null draft) $ forM_ mCurrent $ \vid -> do
      modelComments %= Map.adjust (mkComment "λ you" draft 0 "just now" :) vid
      modelDraft .= ""
  ActionRateComment vid ix rating ->
    modelComments %= Map.adjust (mapNth ix toggle) vid
    where
      toggle c = c & commentVote %~ \old ->
        if old == Just rating then Nothing else Just rating
      mapNth n f xs = [ if i == n then f x else x | (i, x) <- zip [0 ..] xs ]
  ActionSearch str ->
    modelSearch .= str
  ActionCategory cat -> do
    modelCategory .= cat
    issue ActionHome
  ActionToggleSidebar ->
    modelSidebar %= not
  where
    flash f = modelFlash %= \mOld -> Just (maybe 0 (succ . fst) mOld, f)
    withPlayer f = f . Media =<< getElementById "player"
    withThumb f vid = f . Media =<< getElementById (thumbDomId vid)
    nextRate rate = case dropWhile (/= rate) theRates of
      (_ : next : _) -> next
      _ -> 1
----------------------------------------------------------------------
-- | Style, a dark YouTube-inspired theme
theStyle :: C.StyleSheet
theStyle = C.sheet_
  [ C.selector_ "*"
    [ C.boxSizing "border-box" ]
  , C.selector_ "body"
    [ C.margin "0"
    , C.backgroundColor (C.hex "0f0f0f")
    , C.color (C.hex "f1f1f1")
    , C.fontFamily "Roboto, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif"
    -- clip (not hidden) keeps the sticky topbar working
    , C.overflowX "clip"
    ]
  , C.selector_ "::-webkit-scrollbar"
    [ C.width (C.px 8) ]
  , C.selector_ "::-webkit-scrollbar-thumb"
    [ C.background "#3f3f3f"
    , C.borderRadius (C.px 4)
    ]
  -- top bar
  , C.selector_ ".topbar"
    [ C.position "sticky"
    , C.top "0"
    , C.zIndex 100
    , C.display "flex"
    , C.alignItems "center"
    , C.justifyContent "space-between"
    , C.gap (C.px 16)
    , C.height (C.px 56)
    , C.padding "0 16px"
    , C.backgroundColor (C.hex "0f0f0f")
    ]
  , C.selector_ ".topbar-side"
    [ C.display "flex"
    , C.alignItems "center"
    , C.gap (C.px 8)
    ]
  , C.selector_ ".icon-btn"
    [ C.width (C.px 40)
    , C.height (C.px 40)
    , C.display "grid"
    , "place-items" =: "center"
    , C.background "transparent"
    , C.border "none"
    , C.borderRadius (C.pct 50)
    , C.color (C.hex "f1f1f1")
    , C.fontSize (C.px 18)
    , C.cursor "pointer"
    ]
  , C.selector_ ".icon-btn:hover"
    [ C.backgroundColor (C.hex "272727") ]
  , C.selector_ ".logo"
    [ C.display "flex"
    , C.alignItems "center"
    , C.gap (C.px 7)
    , C.cursor "pointer"
    , "user-select" =: "none"
    ]
  , C.selector_ ".logo-mark"
    [ C.width (C.px 30)
    , C.height (C.px 21)
    , C.display "grid"
    , "place-items" =: "center"
    , C.backgroundColor (C.hex "f03")
    , C.borderRadius (C.px 6)
    , C.color (C.hex "fff")
    , C.fontSize (C.px 11)
    , "padding-left" =: "2px"
    ]
  , C.selector_ ".logo-name"
    [ C.fontSize (C.px 20)
    , C.fontWeight "700"
    , "letter-spacing" =: "-0.5px"
    ]
  , C.selector_ ".logo-sup"
    [ C.color (C.hex "aaa")
    , C.fontSize (C.px 9)
    , C.fontWeight "400"
    , "letter-spacing" =: "0.5px"
    , C.marginLeft (C.px 3)
    ]
  , C.selector_ ".searchbox"
    [ C.flex "1"
    , C.display "flex"
    , C.maxWidth (C.px 560)
    -- without this the input's intrinsic width makes the topbar
    -- overflow on phones
    , C.minWidth "0"
    , C.position "relative"
    ]
  , C.selector_ ".search-clear"
    [ C.position "absolute"
    , C.right (C.px 68)
    , C.top (C.pct 50)
    , C.transform "translateY(-50%)"
    , C.width (C.px 28)
    , C.height (C.px 28)
    , C.display "grid"
    , "place-items" =: "center"
    , C.background "transparent"
    , C.border "none"
    , C.borderRadius (C.pct 50)
    , C.color (C.hex "aaaaaa")
    , C.fontSize (C.px 14)
    , C.cursor "pointer"
    ]
  , C.selector_ ".search-clear:hover"
    [ C.backgroundColor (C.hex "272727")
    , C.color (C.hex "ffffff")
    ]
  , C.selector_ ".searchbox input"
    [ C.flex "1"
    , C.minWidth "0"
    , C.width "0"
    , C.height (C.px 40)
    , C.padding "0 36px 0 16px"
    , C.backgroundColor (C.hex "121212")
    , C.border "1px solid #303030"
    , C.borderRadius "20px 0 0 20px"
    , C.color (C.hex "f1f1f1")
    , C.fontSize (C.px 15)
    , "outline" =: "none"
    ]
  , C.selector_ ".searchbox input:focus"
    [ C.borderColor (C.hex "1c62b9") ]
  , C.selector_ ".search-btn"
    [ C.width (C.px 60)
    , C.display "grid"
    , "place-items" =: "center"
    , C.backgroundColor (C.hex "222")
    , C.border "1px solid #303030"
    , C.borderLeft "none"
    , C.borderRadius "0 20px 20px 0"
    , C.fontSize (C.px 20)
    , C.color (C.hex "f1f1f1")
    ]
  , C.selector_ ".gh-link"
    [ C.color (C.hex "aaa")
    , C.textDecoration "none"
    , C.fontSize (C.px 14)
    ]
  , C.selector_ ".gh-link:hover"
    [ C.color (C.hex "f1f1f1") ]
  , C.selector_ ".user-avatar"
    [ C.width (C.px 32)
    , C.height (C.px 32)
    , C.display "grid"
    , "place-items" =: "center"
    , C.borderRadius (C.pct 50)
    , C.background "linear-gradient(135deg, #8E24AA, #5E35B1)"
    , C.fontWeight "700"
    ]
  -- sidebar
  , C.selector_ ".shell"
    [ C.display "flex" ]
  , C.selector_ ".rail"
    [ C.width (C.px 220)
    , "flex-shrink" =: "0"
    , C.position "sticky"
    , C.top (C.px 56)
    , C.height "calc(100vh - 56px)"
    , C.overflowY "auto"
    , C.padding (C.px 12)
    , C.transition "width 0.15s ease, padding 0.15s ease"
    , C.overflowX "hidden"
    , C.whiteSpace "nowrap"
    ]
  , C.selector_ ".shell.rail-closed .rail"
    [ C.width "0"
    , C.padding "12px 0"
    , C.boxShadow "none"
    ]
  -- drawer variant: the rail floats over the content (watch page, phones)
  , C.selector_ ".shell.drawer .rail"
    [ C.position "fixed"
    , C.left "0"
    , C.top (C.px 56)
    , C.bottom "0"
    , C.height "auto"
    , C.width (C.px 230)
    , C.backgroundColor (C.hex "0f0f0f")
    , C.zIndex 90
    , C.boxShadow "8px 0 24px rgba(0,0,0,0.5)"
    ]
  , C.selector_ ".shell.drawer.rail-closed .rail"
    [ C.width "0"
    , C.padding "0"
    , C.boxShadow "none"
    ]
  -- backdrop that closes the drawer on tap; only shown while the
  -- rail floats over the content (watch page always, phones via the
  -- media query below)
  , C.selector_ ".scrim"
    [ C.display "none"
    , C.position "fixed"
    , C.left "0"
    , C.right "0"
    , C.top (C.px 56)
    , C.bottom "0"
    , C.backgroundColor (C.rgba 0 0 0 0.5)
    , C.zIndex 80
    ]
  , C.selector_ ".shell.drawer .scrim"
    [ C.display "block" ]
  , C.selector_ ".shell.rail-closed .scrim"
    [ C.display "none" ]
  , C.selector_ ".rail-item"
    [ C.display "flex"
    , C.alignItems "center"
    , C.gap (C.px 16)
    , C.height (C.px 40)
    , C.padding "0 12px"
    , C.borderRadius (C.px 10)
    , C.fontSize (C.px 14)
    , C.cursor "pointer"
    ]
  , C.selector_ ".rail-item:hover"
    [ C.backgroundColor (C.hex "272727") ]
  , C.selector_ ".rail-item.active"
    [ C.backgroundColor (C.hex "272727")
    , C.fontWeight "500"
    ]
  , C.selector_ ".rail-icon"
    [ C.width (C.px 24)
    , C.textAlign "center"
    , C.fontSize (C.px 16)
    ]
  , C.selector_ ".rail-sec"
    [ C.marginTop (C.px 12)
    , C.paddingTop (C.px 12)
    , C.borderTop "1px solid #272727"
    ]
  , C.selector_ ".rail-head"
    [ C.display "block"
    , C.padding "4px 12px 8px"
    , C.color (C.hex "aaa")
    , C.fontSize (C.px 13)
    ]
  -- avatars
  , C.selector_ ".avatar"
    [ C.width (C.px 36)
    , C.height (C.px 36)
    , "flex-shrink" =: "0"
    , C.display "grid"
    , "place-items" =: "center"
    , C.borderRadius (C.pct 50)
    , C.color (C.hex "fff")
    , C.fontSize (C.px 15)
    , C.fontWeight "700"
    , C.textTransform "uppercase"
    ]
  , C.selector_ ".avatar.sm"
    [ C.width (C.px 24)
    , C.height (C.px 24)
    , C.fontSize (C.px 11)
    ]
  , C.selector_ ".avatar.lg"
    [ C.width (C.px 42)
    , C.height (C.px 42)
    , C.fontSize (C.px 17)
    ]
  , C.selector_ ".avatar.xl"
    [ C.width (C.px 64)
    , C.height (C.px 64)
    , C.fontSize (C.px 26)
    ]
  -- feed headers
  , C.selector_ ".feed-title"
    [ C.margin "18px 0 8px"
    , C.fontSize (C.px 20)
    , C.fontWeight "700"
    ]
  , C.selector_ ".chan-header"
    [ C.display "flex"
    , C.alignItems "center"
    , C.gap (C.px 16)
    , C.padding "20px 0 12px"
    ]
  , C.selector_ ".chan-header .chan-name"
    [ C.fontSize (C.px 20)
    , C.fontWeight "700"
    ]
  , C.selector_ ".chan-link"
    [ C.cursor "pointer" ]
  , C.selector_ ".chan-link:hover"
    [ C.color (C.hex "ffffff") ]
  -- home content
  , C.selector_ ".content"
    [ C.flex "1"
    , C.minWidth "0"
    , C.padding "0 24px 24px"
    ]
  , C.selector_ ".chips"
    [ C.position "sticky"
    , C.top (C.px 56)
    , C.zIndex 50
    , C.display "flex"
    , C.gap (C.px 8)
    , C.padding "12px 0"
    , C.backgroundColor (C.hex "0f0f0f")
    , C.overflowX "auto"
    ]
  , C.selector_ ".chip"
    [ C.height (C.px 32)
    , C.padding "0 12px"
    , C.backgroundColor (C.hex "272727")
    , C.border "none"
    , C.borderRadius (C.px 8)
    , C.color (C.hex "f1f1f1")
    , C.fontSize (C.px 14)
    , C.cursor "pointer"
    , C.whiteSpace "nowrap"
    ]
  , C.selector_ ".chips::-webkit-scrollbar"
    [ C.display "none" ]
  , C.selector_ ".chip:hover"
    [ C.backgroundColor (C.hex "3f3f3f") ]
  , C.selector_ ".chip.active"
    [ C.backgroundColor (C.hex "f1f1f1")
    , C.color (C.hex "0f0f0f")
    , C.fontWeight "500"
    ]
  , C.selector_ ".grid"
    [ C.display "grid"
    , C.gridTemplateColumns "repeat(auto-fill, minmax(320px, 1fr))"
    , C.gap "40px 16px"
    , C.marginTop (C.px 8)
    ]
  , C.selector_ ".empty"
    [ C.padding "80px 0"
    , C.textAlign "center"
    , C.color (C.hex "aaa")
    ]
  -- video cards
  , C.selector_ ".card"
    [ C.cursor "pointer" ]
  , C.selector_ ".thumb"
    [ C.position "relative"
    , C.aspectRatio "16 / 9"
    , C.borderRadius (C.px 12)
    , C.overflow "hidden"
    , C.backgroundColor (C.hex "000")
    , C.transition "border-radius 0.2s ease"
    ]
  , C.selector_ ".card:hover .thumb"
    [ C.borderRadius "0" ]
  , C.selector_ ".thumb video, .up-thumb video"
    [ C.width (C.pct 100)
    , C.height (C.pct 100)
    , "object-fit" =: "cover"
    , C.display "block"
    ]
  , C.selector_ ".dur"
    [ C.position "absolute"
    , C.right (C.px 8)
    , C.bottom (C.px 8)
    , C.padding "1px 6px"
    , C.backgroundColor (C.rgba 0 0 0 0.8)
    , C.borderRadius (C.px 4)
    , C.fontSize (C.px 12)
    , C.fontWeight "500"
    , "pointer-events" =: "none"
    , "font-variant-numeric" =: "tabular-nums"
    ]
  , C.selector_ ".thumb-sound"
    [ C.position "absolute"
    , C.top (C.px 8)
    , C.right (C.px 8)
    , C.width (C.px 28)
    , C.height (C.px 28)
    , C.display "grid"
    , "place-items" =: "center"
    , C.backgroundColor (C.rgba 0 0 0 0.8)
    , C.border "none"
    , C.borderRadius (C.pct 50)
    , C.color (C.hex "fff")
    , C.fontSize (C.px 13)
    , C.cursor "pointer"
    , C.opacity 0
    , C.transition "opacity 0.15s ease"
    ]
  , C.selector_ ".thumb:hover .thumb-sound, .up-thumb:hover .thumb-sound"
    [ C.opacity 1 ]
  , C.selector_ ".thumb-sound:hover"
    [ C.backgroundColor (C.hex "000") ]
  , C.selector_ ".card-meta"
    [ C.display "flex"
    , C.gap (C.px 12)
    , C.marginTop (C.px 12)
    ]
  , C.selector_ ".card-text"
    [ C.minWidth "0" ]
  , C.selector_ ".card-title"
    [ C.margin "0"
    , C.fontSize (C.px 15)
    , C.fontWeight "500"
    , C.lineHeight "1.4"
    , C.display "-webkit-box"
    , "-webkit-line-clamp" =: "2"
    , "-webkit-box-orient" =: "vertical"
    , C.overflow "hidden"
    ]
  , C.selector_ ".card-sub"
    [ C.margin "2px 0 0"
    , C.color (C.hex "aaa")
    , C.fontSize (C.px 13)
    ]
  -- watch page
  , C.selector_ ".watch"
    [ C.display "flex"
    , C.alignItems "flex-start"
    , C.gap (C.px 24)
    , C.padding (C.px 24)
    , C.maxWidth (C.px 1700)
    , C.margin "0 auto"
    ]
  , C.selector_ ".watch.theater"
    [ C.flexDirection "column"
    , C.maxWidth "none"
    ]
  , C.selector_ ".watch.theater .upnext"
    [ C.width (C.pct 100) ]
  , C.selector_ ".watch-main"
    [ C.flex "1"
    , C.minWidth "0"
    , C.width (C.pct 100)
    ]
  , C.selector_ ".player-shell"
    [ C.position "relative"
    , C.width (C.pct 100)
    , C.aspectRatio "16 / 9"
    , C.backgroundColor (C.hex "000")
    , C.borderRadius (C.px 12)
    , C.overflow "hidden"
    ]
  , C.selector_ ".player-shell video"
    [ C.width (C.pct 100)
    , C.height (C.pct 100)
    , C.display "block"
    , "object-fit" =: "contain"
    ]
  , C.selector_ ".bigplay"
    [ C.position "absolute"
    , "inset" =: "0"
    , C.display "grid"
    , "place-items" =: "center"
    , C.opacity 0
    , "pointer-events" =: "none"
    , C.transition "opacity 0.15s ease"
    ]
  , C.selector_ ".player-shell.paused .bigplay"
    [ C.opacity 1
    , "pointer-events" =: "auto"
    , C.cursor "pointer"
    ]
  , C.selector_ ".bigplay span"
    [ C.width (C.px 84)
    , C.height (C.px 84)
    , C.display "grid"
    , "place-items" =: "center"
    , C.backgroundColor (C.rgba 0 0 0 0.6)
    , C.borderRadius (C.pct 50)
    , C.fontSize (C.px 30)
    , "padding-left" =: "7px"
    , C.transition "transform 0.15s ease"
    ]
  , C.selector_ ".bigplay:hover span"
    [ C.transform "scale(1.08)" ]
  -- transient key/click feedback flashed over the video
  , C.selector_ ".flash"
    [ C.position "absolute"
    , "inset" =: "0"
    , C.display "flex"
    , C.alignItems "center"
    , C.justifyContent "center"
    , "pointer-events" =: "none"
    ]
  , C.selector_ ".flash.left"
    [ C.justifyContent "flex-start"
    , C.paddingLeft (C.pct 12)
    ]
  , C.selector_ ".flash.right"
    [ C.justifyContent "flex-end"
    , C.paddingRight (C.pct 12)
    ]
  , C.selector_ ".flash-bubble"
    [ C.display "grid"
    , "place-items" =: "center"
    , C.padding "10px 18px"
    , C.backgroundColor (C.rgba 0 0 0 0.65)
    , C.borderRadius (C.px 999)
    , C.color (C.hex "fff")
    , C.fontSize (C.px 15)
    , C.fontWeight "500"
    , C.whiteSpace "nowrap"
    , C.opacity 0
    ]
  , C.selector_ ".flash.mid .flash-bubble"
    [ C.width (C.px 76)
    , C.height (C.px 76)
    , C.padding "0"
    , C.borderRadius (C.pct 50)
    , C.fontSize (C.px 26)
    ]
  , C.selector_ ".flash-a .flash-bubble"
    [ C.animation "flash-fade-a 0.55s ease-out" ]
  , C.selector_ ".flash-b .flash-bubble"
    [ C.animation "flash-fade-b 0.55s ease-out" ]
  , C.keyframes_ "flash-fade-a"
    [ C.from_ [ C.opacity 1, C.transform "scale(0.9)" ]
    , C.to_   [ C.opacity 0, C.transform "scale(1.3)" ]
    ]
  , C.keyframes_ "flash-fade-b"
    [ C.from_ [ C.opacity 1, C.transform "scale(0.9)" ]
    , C.to_   [ C.opacity 0, C.transform "scale(1.3)" ]
    ]
  -- player controls
  , C.selector_ ".controls"
    [ C.position "absolute"
    , C.left "0"
    , C.right "0"
    , C.bottom "0"
    , C.padding "0 12px 6px"
    , C.background "linear-gradient(transparent, rgba(0,0,0,0.8))"
    , C.opacity 0
    , C.transition "opacity 0.2s ease"
    ]
  , C.selector_ ".player-shell:hover .controls, .player-shell.paused .controls"
    [ C.opacity 1 ]
  , C.selector_ ".controls input[type=range]"
    [ "-webkit-appearance" =: "none"
    , C.appearance "none"
    , C.height (C.px 4)
    , C.borderRadius (C.px 2)
    , C.cursor "pointer"
    , C.display "block"
    ]
  , C.selector_ ".controls input[type=range]::-webkit-slider-thumb"
    [ "-webkit-appearance" =: "none"
    , C.width (C.px 13)
    , C.height (C.px 13)
    , C.borderRadius (C.pct 50)
    , C.backgroundColor (C.hex "f03")
    , C.border "none"
    ]
  , C.selector_ ".controls input[type=range]::-moz-range-thumb"
    [ C.width (C.px 13)
    , C.height (C.px 13)
    , C.borderRadius (C.pct 50)
    , C.backgroundColor (C.hex "f03")
    , C.border "none"
    ]
  , C.selector_ ".controls .vol::-webkit-slider-thumb"
    [ C.backgroundColor (C.hex "fff")
    , C.width (C.px 11)
    , C.height (C.px 11)
    ]
  , C.selector_ ".controls .vol::-moz-range-thumb"
    [ C.backgroundColor (C.hex "fff")
    , C.width (C.px 11)
    , C.height (C.px 11)
    ]
  , C.selector_ ".seek"
    [ C.width (C.pct 100)
    , C.marginBottom (C.px 4)
    ]
  , C.selector_ ".vol"
    [ C.width (C.px 68) ]
  , C.selector_ ".ctrl-row"
    [ C.display "flex"
    , C.alignItems "center"
    , C.gap (C.px 4)
    ]
  , C.selector_ ".ctrl-btn"
    [ C.minWidth (C.px 36)
    , C.height (C.px 36)
    , C.display "grid"
    , "place-items" =: "center"
    , C.background "transparent"
    , C.border "none"
    , C.color (C.hex "fff")
    , C.fontSize (C.px 15)
    , C.cursor "pointer"
    , C.opacity 0.9
    ]
  , C.selector_ ".ctrl-btn:hover"
    [ C.opacity 1 ]
  , C.selector_ ".play-btn"
    [ C.fontSize (C.px 18) ]
  , C.selector_ ".rate-btn"
    [ C.fontSize (C.px 13)
    , C.fontWeight "600"
    ]
  , C.selector_ ".time"
    [ C.margin "0 8px"
    , C.color (C.hex "ddd")
    , C.fontSize (C.px 13)
    , "font-variant-numeric" =: "tabular-nums"
    , C.whiteSpace "nowrap"
    ]
  , C.selector_ ".ctrl-spacer"
    [ C.flex "1" ]
  -- watch metadata
  , C.selector_ ".w-title"
    [ C.margin "16px 0 10px"
    , C.fontSize (C.px 20)
    , C.fontWeight "700"
    , C.lineHeight "1.3"
    ]
  , C.selector_ ".chanrow"
    [ C.display "flex"
    , C.alignItems "center"
    , C.gap (C.px 12)
    , C.flexWrap "wrap"
    , C.marginBottom (C.px 16)
    ]
  , C.selector_ ".chan-text"
    [ C.display "flex"
    , C.flexDirection "column"
    ]
  , C.selector_ ".chan-name"
    [ C.fontSize (C.px 16)
    , C.fontWeight "500"
    ]
  , C.selector_ ".chan-sub"
    [ C.color (C.hex "aaa")
    , C.fontSize (C.px 12)
    ]
  , C.selector_ ".subscribe"
    [ C.height (C.px 36)
    , C.padding "0 16px"
    , C.backgroundColor (C.hex "f1f1f1")
    , C.border "none"
    , C.borderRadius (C.px 18)
    , C.color (C.hex "0f0f0f")
    , C.fontSize (C.px 14)
    , C.fontWeight "500"
    , C.cursor "pointer"
    ]
  , C.selector_ ".subscribe:hover"
    [ C.backgroundColor (C.hex "d9d9d9") ]
  , C.selector_ ".subscribe.subscribed"
    [ C.backgroundColor (C.hex "272727")
    , C.color (C.hex "f1f1f1")
    ]
  , C.selector_ ".subscribe.subscribed:hover"
    [ C.backgroundColor (C.hex "3f3f3f") ]
  , C.selector_ ".like-group"
    [ C.display "flex"
    , C.alignItems "center"
    , C.height (C.px 36)
    , C.marginLeft "auto"
    , C.backgroundColor (C.hex "272727")
    , C.borderRadius (C.px 18)
    , C.overflow "hidden"
    ]
  , C.selector_ ".like-btn"
    [ C.height (C.pct 100)
    , C.padding "0 14px"
    , C.background "transparent"
    , C.border "none"
    , C.color (C.hex "f1f1f1")
    , C.fontSize (C.px 14)
    , C.cursor "pointer"
    ]
  , C.selector_ ".like-btn:hover"
    [ C.backgroundColor (C.hex "3f3f3f") ]
  , C.selector_ ".like-btn.active"
    [ C.color (C.hex "3ea6ff") ]
  , C.selector_ ".like-sep"
    [ C.width (C.px 1)
    , C.height (C.px 22)
    , C.backgroundColor (C.hex "3f3f3f")
    ]
  , C.selector_ ".desc"
    [ C.padding "12px 16px"
    , C.backgroundColor (C.hex "272727")
    , C.borderRadius (C.px 12)
    , C.fontSize (C.px 14)
    , C.lineHeight "1.5"
    , "overflow-wrap" =: "anywhere"
    ]
  , C.selector_ ".desc p"
    [ C.margin "0 0 6px" ]
  , C.selector_ ".desc-stats"
    [ C.fontWeight "600" ]
  , C.selector_ ".desc a"
    [ C.color (C.hex "3ea6ff")
    , C.textDecoration "none"
    ]
  , C.selector_ ".desc-note"
    [ C.color (C.hex "ccc") ]
  -- up next
  , C.selector_ ".upnext"
    [ C.width (C.px 380)
    , "flex-shrink" =: "0"
    ]
  , C.selector_ ".upnext-head"
    [ C.display "block"
    , C.margin "2px 4px 10px"
    , C.fontSize (C.px 15)
    , C.fontWeight "600"
    ]
  , C.selector_ ".up-row"
    [ C.display "flex"
    , C.gap (C.px 8)
    , C.padding (C.px 4)
    , C.marginBottom (C.px 4)
    , C.borderRadius (C.px 10)
    , C.cursor "pointer"
    ]
  , C.selector_ ".up-row:hover"
    [ C.backgroundColor (C.hex "272727") ]
  , C.selector_ ".up-thumb"
    [ C.position "relative"
    , C.width (C.px 168)
    , "flex-shrink" =: "0"
    , C.aspectRatio "16 / 9"
    , C.borderRadius (C.px 8)
    , C.overflow "hidden"
    , C.backgroundColor (C.hex "000")
    ]
  , C.selector_ ".up-text"
    [ C.minWidth "0" ]
  , C.selector_ ".up-title"
    [ C.margin "0 0 4px"
    , C.fontSize (C.px 14)
    , C.fontWeight "500"
    , C.lineHeight "1.3"
    , C.display "-webkit-box"
    , "-webkit-line-clamp" =: "2"
    , "-webkit-box-orient" =: "vertical"
    , C.overflow "hidden"
    ]
  , C.selector_ ".up-sub"
    [ C.margin "0"
    , C.color (C.hex "aaa")
    , C.fontSize (C.px 12)
    ]
  -- comments
  , C.selector_ ".comments"
    [ C.marginTop (C.px 24) ]
  , C.selector_ ".comments-head"
    [ C.display "block"
    , C.marginBottom (C.px 16)
    , C.fontSize (C.px 16)
    , C.fontWeight "700"
    ]
  , C.selector_ ".addc"
    [ C.display "flex"
    , C.alignItems "center"
    , C.gap (C.px 12)
    , C.marginBottom (C.px 24)
    ]
  , C.selector_ ".cinput"
    [ C.flex "1"
    , C.minWidth "0"
    , C.padding "8px 2px"
    , C.background "transparent"
    , C.border "none"
    , C.borderBottom "1px solid #3f3f3f"
    , C.color (C.hex "f1f1f1")
    , C.fontSize (C.px 14)
    , "outline" =: "none"
    ]
  , C.selector_ ".cinput:focus"
    [ "border-bottom-color" =: "#f1f1f1" ]
  , C.selector_ ".cpost"
    [ C.height (C.px 36)
    , C.padding "0 16px"
    , C.backgroundColor (C.hex "3ea6ff")
    , C.border "none"
    , C.borderRadius (C.px 18)
    , C.color (C.hex "0f0f0f")
    , C.fontSize (C.px 14)
    , C.fontWeight "500"
    , C.cursor "pointer"
    , C.whiteSpace "nowrap"
    ]
  , C.selector_ ".cpost:disabled"
    [ C.backgroundColor (C.hex "272727")
    , C.color (C.hex "717171")
    , C.cursor "default"
    ]
  , C.selector_ ".comment"
    [ C.display "flex"
    , C.gap (C.px 12)
    , C.marginBottom (C.px 20)
    ]
  , C.selector_ ".cbody"
    [ C.minWidth "0" ]
  , C.selector_ ".cmeta"
    [ C.margin "0"
    , C.fontSize (C.px 13)
    ]
  , C.selector_ ".cauthor"
    [ C.fontWeight "500" ]
  , C.selector_ ".cage"
    [ C.marginLeft (C.px 8)
    , C.color (C.hex "aaa")
    , C.fontSize (C.px 12)
    ]
  , C.selector_ ".ctext"
    [ C.margin "4px 0"
    , C.fontSize (C.px 14)
    , C.lineHeight "1.4"
    , "overflow-wrap" =: "anywhere"
    ]
  , C.selector_ ".cfoot"
    [ C.display "flex"
    , C.alignItems "center"
    , C.gap (C.px 4)
    ]
  , C.selector_ ".cvote"
    [ C.padding "3px 10px"
    , C.background "transparent"
    , C.border "none"
    , C.borderRadius (C.px 12)
    , C.color (C.hex "aaa")
    , C.fontSize (C.px 12)
    , C.cursor "pointer"
    ]
  , C.selector_ ".cvote:hover"
    [ C.backgroundColor (C.hex "272727")
    , C.color (C.hex "f1f1f1")
    ]
  , C.selector_ ".cvote.active"
    [ C.color (C.hex "3ea6ff") ]
  -- small screens
  , C.media_ (C.MediaQuery "(max-width: 1100px)")
    [ C.rule_ ".watch" [ C.flexDirection "column" ]
    , C.rule_ ".upnext" [ C.width (C.pct 100) ]
    ]
  -- on phones the sidebar becomes a drawer over the content,
  -- toggled by the topbar hamburger
  , C.media_ (C.MediaQuery "(max-width: 800px)")
    [ C.rule_ ".rail"
      [ C.position "fixed"
      , C.left "0"
      , C.top (C.px 56)
      , C.bottom "0"
      , C.height "auto"
      , C.width (C.px 230)
      , C.backgroundColor (C.hex "0f0f0f")
      , C.zIndex 90
      , C.boxShadow "8px 0 24px rgba(0,0,0,0.5)"
      ]
    , C.rule_ ".shell.rail-closed .rail"
      [ C.width "0"
      , C.padding "0"
      , C.boxShadow "none"
      ]
    , C.rule_ ".shell .scrim" [ C.display "block" ]
    , C.rule_ ".gh-link" [ C.display "none" ]
    , C.rule_ ".watch" [ C.padding (C.px 12) ]
    , C.rule_ ".content" [ C.padding "0 12px 12px" ]
    , C.rule_ ".grid"
      [ C.gridTemplateColumns "repeat(auto-fill, minmax(280px, 1fr))"
      , C.gap "24px 12px"
      ]
    , C.rule_ ".w-title" [ C.fontSize (C.px 17) ]
    , C.rule_ ".up-thumb" [ C.width (C.px 140) ]
    ]
  , C.media_ (C.MediaQuery "(max-width: 560px)")
    [ C.rule_ ".vol" [ C.display "none" ]
    , C.rule_ ".time"
      [ C.fontSize (C.px 12)
      , C.margin "0 4px"
      ]
    , C.rule_ ".ctrl-btn" [ C.minWidth (C.px 30) ]
    , C.rule_ ".logo-sup" [ C.display "none" ]
    , C.rule_ ".topbar"
      [ C.gap (C.px 8)
      , C.padding "0 12px"
      ]
    , C.rule_ ".search-btn" [ C.width (C.px 44) ]
    , C.rule_ ".search-clear" [ C.right (C.px 50) ]
    ]
  ]
----------------------------------------------------------------------
main :: IO ()
main = startApp (defaultEvents <> mediaEvents <> mouseEvents) app

app :: App Model Action
app = (component (mkModel theCatalog) handleUpdate handleView)
  { styles = [ Sheet theStyle ]
  , mount = Just ActionInit
  , logLevel = DebugAll
  }
----------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------
