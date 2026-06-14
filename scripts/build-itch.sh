#!/bin/bash
# Build a self-contained, static bundle of the game for itch.io.
#
# The client is fully client-side (state lives in localStorage, no server
# calls), so the game runs as a plain HTML5 upload. This script:
#   1. Links the optimized production JS (fullLinkJS) into static/dist.
#   2. Minifies that JS and bundles the CSS via the existing vite script.
#   3. Assembles a clean dist dir with a sandbox-friendly index.html that
#      uses relative paths and drops the service worker / livereload (neither
#      works inside itch.io's cross-origin iframe).
#   4. Zips it into an itch.io-ready archive (index.html at the zip root).
#
# Usage: ./scripts/build-itch.sh
set -euo pipefail
cd "$(dirname "$0")/.."

STATIC_DIST="jvm/src/main/resources/static/dist"
STATIC_DIR="jvm/src/main/resources/static"
OUT_DIR="target/itch/files"
ZIP_PATH="target/itch/cardbench-itch.zip"

cyan() { printf '\033[36m%s\033[0m\n' "$1"; }
green() { printf '\033[32m%s\033[0m\n' "$1"; }

cyan "→ Linking production JS (fullLinkJS)…"
sbt -J-Xmx4G 'project js' 'set Compile/scalaJSStage := FullOptStage' fullLinkJS

cyan "→ Minifying JS and bundling CSS…"
npm run --silent build:prod:assets

cyan "→ Assembling $OUT_DIR…"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"
cp "$STATIC_DIST/main.js" "$OUT_DIR/main.js"
cp "$STATIC_DIST/app-bundle.css" "$OUT_DIR/app-bundle.css"
cp "$STATIC_DIR/favicon.svg" "$OUT_DIR/favicon.svg"

cat > "$OUT_DIR/index.html" <<'HTML'
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>Cardbench</title>
    <meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover" />
    <meta name="theme-color" content="#4ade80" />
    <meta name="mobile-web-app-capable" content="yes" />
    <link rel="icon" type="image/svg+xml" href="./favicon.svg" />
    <link rel="stylesheet" href="./app-bundle.css" />
    <script defer src="./main.js"></script>
    <noscript>This app requires JavaScript. Please enable it and reload.</noscript>
    <style>@keyframes app-spin{to{transform:rotate(360deg)}}</style>
  </head>
  <body>
    <div id="app" class="app-container">
      <div class="app-loader">
        <div class="app-spinner"></div>
        <div class="app-loading-text">Loading Cardbench…</div>
      </div>
    </div>
  </body>
</html>
HTML

cyan "→ Zipping $ZIP_PATH…"
rm -f "$ZIP_PATH"
ZIP_ABS="$(pwd)/$ZIP_PATH"
# Zip from inside OUT_DIR so index.html sits at the archive root (itch.io requirement).
(cd "$OUT_DIR" && zip -qr "$ZIP_ABS" .)

green "✓ itch.io bundle ready:"
green "    $OUT_DIR/        (unpacked files)"
green "    $ZIP_PATH   (upload this; set as 'This file will be played in the browser')"
