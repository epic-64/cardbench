#!/bin/bash
# Watch-mode dev server: build client assets, start the JVM server with
# DEV_LIVE_ASSETS=true, then on every save rebuild what changed into static/dist.
# The server reads HTML/JS/CSS from disk on each request in this mode, and the
# page polls the .livereload marker, so the browser hot-reloads itself.
#
# Usage: ./scripts/dev-watch.sh
set -e
cd "$(dirname "$0")/.."

if ! command -v inotifywait >/dev/null 2>&1; then
  echo "ERROR: inotifywait not found. Install: sudo apt install inotify-tools" >&2
  exit 1
fi
if ! command -v lsof >/dev/null 2>&1; then
  echo "ERROR: lsof not found. Install: sudo apt install lsof" >&2
  exit 1
fi

if [ -f .env ]; then set -a; source .env; set +a; fi

DIST_DIR="jvm/src/main/resources/static/dist"
LINK_OUTPUT=".bloop/js/js-js/main.js"
RELOAD_MARKER="$DIST_DIR/.livereload"
APP_PORT="${PORT:-8080}"

cyan() { printf '\033[36m%s\033[0m\n' "$1"; }
green() { printf '\033[32m%s\033[0m\n' "$1"; }
red() { printf '\033[31m%s\033[0m\n' "$1"; }

mark_reload() { printf '%s %s\n' "$(date +%s)" "$1" > "$RELOAD_MARKER"; }

rebuild_js() {
  cyan "→ Scala change detected, relinking JS…"
  if bloop link js && cp "$LINK_OUTPUT" "$DIST_DIR/main.js"; then
    mark_reload js; green "✓ JS rebuilt"; return 0
  fi
  red "✗ JS rebuild failed (fix and save again)"; return 1
}

rebuild_css() {
  cyan "→ CSS change detected, rebundling…"
  if SKIP_JS_MINIFY=1 npm run --silent minify:assets; then
    mark_reload css; green "✓ CSS rebundled"; return 0
  fi
  red "✗ CSS rebundle failed (fix and save again)"; return 1
}

port_listener() { lsof -ti tcp:"$APP_PORT" -sTCP:LISTEN 2>/dev/null; }

SERVER_PID=""
start_server() {
  DEV_LIVE_ASSETS=true bloop run jvm &
  SERVER_PID=$!
  for _ in $(seq 1 120); do
    curl -sf -o /dev/null "http://localhost:$APP_PORT/api/version" && break
    sleep 0.5
  done
  green "✓ Server up on :$APP_PORT"
}

stop_server() {
  kill "$SERVER_PID" 2>/dev/null || true
  local listener; listener="$(port_listener)"
  [ -n "$listener" ] && kill $listener 2>/dev/null || true
  wait "$SERVER_PID" 2>/dev/null || true
  SERVER_PID=""
  for _ in $(seq 1 60); do [ -z "$(port_listener)" ] && break; sleep 0.1; done
}

restart_server() { cyan "↻ Restarting server…"; stop_server; start_server; mark_reload server; }

mkdir -p "$DIST_DIR"
cyan "Initial build…"
bloop link js
cp "$LINK_OUTPUT" "$DIST_DIR/main.js"
SKIP_JS_MINIFY=1 npm run --silent minify:assets
mark_reload init

cyan "Starting JVM server…"
start_server

exec 3< <(inotifywait -m -q -r -e close_write,move,create,delete --format '%w%f' \
  js/src/main shared/src/main jvm/src/main/resources/static/css jvm/src/main/scala)
INOTIFY_PID=$!

cleanup() { echo ""; cyan "Shutting down…"; kill "$INOTIFY_PID" 2>/dev/null || true; stop_server; }
trap cleanup EXIT INT TERM

green "Watching for changes — edit & save, the browser hot-reloads itself."
cyan "Press Ctrl+C to stop."

classify() {
  case "$1" in
    shared/src/*.scala) NEED_JS=1; NEED_SERVER=1 ;;
    js/src/*.scala)     NEED_JS=1 ;;
    jvm/src/*.scala)    NEED_SERVER=1 ;;
    *.css)              NEED_CSS=1 ;;
  esac
}

while read -r path <&3; do
  NEED_JS=0; NEED_CSS=0; NEED_SERVER=0
  classify "$path"
  while read -r -t 0.4 more <&3; do classify "$more"; done

  rebuilt_ok=1
  [ "$NEED_JS" = 1 ] && { rebuild_js || rebuilt_ok=0; }
  [ "$NEED_CSS" = 1 ] && { rebuild_css || rebuilt_ok=0; }

  if [ "$rebuilt_ok" != 1 ]; then
    red "Build failed — keeping current server. Fix and save again."
  elif [ "$NEED_SERVER" = 1 ]; then
    restart_server
  else
    green "✓ Hot-reloaded."
  fi
done
