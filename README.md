# Cardbench

A Scala 3 fullstack app: Cask (JVM server) + ScalaJS + Laminar (client), as a
cross-project with shared code. Scaffolded from a Hello World template.

## Layout

```
build.sbt            # cross-project: js / jvm / shared
shared/              # code compiled for both platforms
jvm/                 # Cask web server + static assets (HTML, CSS, service worker)
js/                  # ScalaJS + Laminar client
scripts/             # dev + build helpers
```

## Quick start (sbt only)

```bash
sbt fastLinkJS      # compile the client into jvm/.../static/dist/main.js
sbt jvm/run         # start the server
# open http://localhost:8080
```

## Dev workflow (bloop, with hot reload)

```bash
sbt bloopInstall              # once, and after editing build.sbt
npm install                   # once (jsdom for tests, vite for asset bundling)
./scripts/dev-watch.sh        # edit & save — the browser hot-reloads itself
```

## Tests

```bash
bloop test jvm                # JVM tests
sbt js/test                   # JS tests
./scripts/run-all-tests.sh    # everything
```

## Build / compile

- `bloop compile jvm` / `bloop compile js`
- All warnings are errors (`-Werror`). Fix warnings immediately.

## Production build

`nixpacks.toml` builds the client with `fullLinkJS`, bundles assets, and stages
the server via sbt-native-packager (`./target/universal/stage/bin/main`).
