#!/bin/bash
# Full dev rebuild: link JS, copy to resources, then compile and run the server.
# Usage: ./scripts/dev-server.sh
set -e
cd "$(dirname "$0")/.."

if [ -f .env ]; then set -a; source .env; set +a; fi

echo "Building JS…"
bloop link js

echo "Copying JS to resources…"
mkdir -p jvm/src/main/resources/static/dist
cp .bloop/js/js-js/main.js jvm/src/main/resources/static/dist/main.js

echo "Bundling assets (unminified JS for debugging)…"
SKIP_JS_MINIFY=1 npm run minify:assets

echo "Compiling and running the JVM server…"
bloop run jvm
