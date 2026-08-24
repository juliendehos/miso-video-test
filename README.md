# 🍜 📽️ MisoTube

A YouTube-style video app written entirely in Haskell, compiled to WebAssembly
with [Miso](https://haskell-miso.org/).

Everything on screen is rendered from Haskell — the home grid uses live
`<video>` elements as thumbnails (real first frames via media fragments, real
durations from `loadedmetadata`, muted previews on hover), and the watch page
drives a chromeless `<video>` through Miso's `Miso.Media` API: play/pause,
seeking, volume, playback rate, theater mode, fullscreen and autoplay-next.

## Features

- 🏠 Home grid with hover previews, duration badges and category chips
- 🔍 Search across titles and channels
- 📺 Channel pages with subscribe/unsubscribe
- 🎞 Shorts, 🔥 Trending, 📚 Library (liked videos) and 🕒 History feeds
- ▶️ Custom player chrome, entirely Haskell-driven
- 📱 Mobile friendly, with a drawer sidebar

## Try online

- [https://video.haskell-miso.org](https://video.haskell-miso.org)

## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop
make
make serve
```
