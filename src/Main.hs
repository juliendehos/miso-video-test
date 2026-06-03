----------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
module Main where
----------------------------------------------------------------------
import Data.Map.Strict as Map ((!?), adjust, keys, notMember)
----------------------------------------------------------------------
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Lens as Lens
import Miso.Media (Media(..), videoHeight, videoWidth)
----------------------------------------------------------------------
import Model
----------------------------------------------------------------------
theSmallWidth :: Int
theSmallWidth = 400
----------------------------------------------------------------------
thePlaylist :: [MisoString]
thePlaylist =
  [ "2023-09-04_001.mp4"
  , "backloop_hs.webm"
  ]
----------------------------------------------------------------------
data Action
  = ActionAskVideo MisoString
  | ActionAskSwitch
  | ActionAskSize ClipId Media
  | ActionSetSize ClipId Int Int
----------------------------------------------------------------------
handleView :: props -> Model -> View Model Action
handleView _ model = vfrag
  [ p_ []
    [ a_ [ href_ "https://github.com/haskell-miso/miso-video" ] [ text "source" ]
    , text " - "
    , a_ [ href_ "https://haskell-miso.github.io/miso-video/" ] [ text "demo" ]
    ]
  , p_ [] 
    [ select_ [ onChange ActionAskVideo ] myOptions
    , button_ [ onClick ActionAskSwitch ] [ mySmall ]
    ]
  , p_ [] myVideo
  ]
  where
    myOptions = 
      option_ [ value_ "" ] [ "--" ] :
      map fmtClip (keys $ model^.modelPlaylist)

    fmtClip cId = let i = cId^.clipId in option_ [ value_ i ] [ text i ]
    mySmall = if model^.modelSmall then "switch to original size" else "switch to small size"

    myVideo = 
      let mPlaying = model^.modelPlaying
          mClip = mPlaying >>= ((model^.modelPlaylist) !?)
      in case (mPlaying, mClip)  of
        (Just i, Just c) -> 
          [ video_ 
            ( src_ (i^.clipId) : 
              fmtSize c ++
              [ id_ "myvideo"
              , controls_ True
              , onCanPlayWith (ActionAskSize i)
              ]
            )
            []
          ]
        _ -> []

    fmtSize c =
      let w = max 1 (c^.clipWidth)
          h = c^.clipHeight
      in if model^.modelSmall
      then [ width_ (ms theSmallWidth), height_ (ms $ theSmallWidth*h `div` w) ]
      else [ width_ (ms w), height_ (ms h) ]
----------------------------------------------------------------------
handleUpdate :: Action -> Effect parent props Model Action
handleUpdate (ActionAskVideo str) = do
  playlist <- use modelPlaylist
  modelPlaying .= if str == ""  || notMember (mkClipId str) playlist
    then Nothing 
    else Just (mkClipId str)
handleUpdate ActionAskSwitch = 
  modelSmall %= not
handleUpdate (ActionAskSize cId m) = 
  io (ActionSetSize cId <$> videoWidth m <*> videoHeight m)
handleUpdate (ActionSetSize cId w h) = 
  modelPlaylist %= adjust upClip cId
  where
    upClip c = c & clipWidth .~ w
                 & clipHeight .~ h
----------------------------------------------------------------------
main :: IO ()
main = do
  let model = mkModel thePlaylist
  startApp (defaultEvents <> mediaEvents) (component model handleUpdate handleView)
    { logLevel = DebugAll
    }
----------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------
