package server

import cask.main.MainRoutes
import io.undertow.server.HttpHandler
import io.undertow.server.HttpServerExchange
import org.slf4j.LoggerFactory

// Configuration via environment variables.
object Config:
  val port: Int             = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(8080)
  val cacheDuration: Int    = sys.env.get("CACHE_MAX_AGE").flatMap(_.toIntOption).getOrElse(3600)
  // Dev-only: serve HTML/JS/CSS straight from the source dir (no classpath, no
  // caching) so rebuilt assets are picked up without restarting the server.
  val devLiveAssets: Boolean = sys.env.get("DEV_LIVE_ASSETS").contains("true")
  val devStaticDir: String   = sys.env.getOrElse("DEV_STATIC_DIR", "jvm/src/main/resources/static")

object WebServer extends MainRoutes:
  private val logger    = LoggerFactory.getLogger(getClass)
  private val startTime = System.currentTimeMillis()

  def serverHandler: HttpHandler =
    if Config.devLiveAssets then devAssetHandler(defaultHandler) else defaultHandler

  // Map a request path onto a file under the dev static dir, or None to fall
  // through. Guards against path traversal by requiring the resolved path to
  // stay within the base dir.
  private def devResolve(requestPath: String): Option[java.nio.file.Path] =
    val base = java.nio.file.Paths.get(Config.devStaticDir).toAbsolutePath.normalize
    val relative = requestPath match
      case "/"                           => Some("index.html")
      case "/sw.js"                      => Some("sw.js")
      case p if p.startsWith("/static/") => Some(p.stripPrefix("/static/"))
      case _                             => None
    relative
      .map(rel => base.resolve(rel).normalize)
      .filter(_.startsWith(base))
      .filter(java.nio.file.Files.isRegularFile(_))

  private val devContentTypes = Map(
    "html"        -> "text/html; charset=utf-8",
    "js"          -> "application/javascript",
    "css"         -> "text/css",
    "json"        -> "application/json",
    "map"         -> "application/json",
    "webmanifest" -> "application/manifest+json",
    "png"         -> "image/png",
    "svg"         -> "image/svg+xml",
    "ico"         -> "image/x-icon",
  )

  private def devContentType(file: java.nio.file.Path): String =
    val name = file.getFileName.toString
    val ext  = name.drop(name.lastIndexOf('.') + 1)
    devContentTypes
      .get(ext)
      .orElse(Option(java.nio.file.Files.probeContentType(file)))
      .getOrElse("application/octet-stream")

  private def devAssetHandler(next: HttpHandler): HttpHandler = (exchange: HttpServerExchange) =>
    devResolve(exchange.getRequestPath) match
      case Some(file) =>
        val bytes = java.nio.file.Files.readAllBytes(file)
        exchange.getResponseHeaders.put(io.undertow.util.Headers.CONTENT_TYPE, devContentType(file))
        exchange.getResponseHeaders.put(io.undertow.util.Headers.CACHE_CONTROL, "no-store, no-cache, must-revalidate")
        exchange.getResponseSender.send(java.nio.ByteBuffer.wrap(bytes))
      case None =>
        next.handleRequest(exchange)

  @cask.get("/api/version")
  def version(): ujson.Value =
    ujson.Obj(
      "version"  -> sys.env.getOrElse("APP_VERSION", "dev"),
      "uptimeMs" -> (System.currentTimeMillis() - startTime),
    )

  // Serve the app shell at the root URL.
  @cask.get("/")
  def index(): cask.Response[java.io.InputStream] =
    servePage("index.html")

  // Serve the service worker at root scope.
  @cask.get("/sw.js")
  def serviceWorker(): cask.Response[java.io.InputStream] =
    cask.Response(
      data = getClass.getClassLoader.getResourceAsStream("static/sw.js"),
      statusCode = 200,
      headers = Seq(
        "Content-Type"  -> "application/javascript",
        "Cache-Control" -> "no-cache",
      ),
    )

  private def servePage(htmlFile: String): cask.Response[java.io.InputStream] =
    cask.Response(
      data = getClass.getClassLoader.getResourceAsStream(s"static/$htmlFile"),
      statusCode = 200,
      headers = Seq("Content-Type" -> "text/html"),
    )

  // Cask handles content types, ETag, Last-Modified and 304s for these.
  @cask.staticResources("/static", headers = Seq("Cache-Control" -> s"public, max-age=${Config.cacheDuration}"))
  def staticResourceRoutes() = "static"

  override def port: Int    = Config.port
  override def host: String = "0.0.0.0"

  override def main(args: Array[String]): Unit =
    val undertow = io.undertow.Undertow.builder
      .addHttpListener(port, host)
      .setHandler(serverHandler)
      .build
    Runtime.getRuntime.addShutdownHook(Thread(() => undertow.stop()))
    undertow.start()
    logger.info("=" * 60)
    logger.info("Gigadev server started")
    logger.info(s"Listening on http://$host:$port")
    if Config.devLiveAssets then logger.info(s"Dev live assets: serving from ${Config.devStaticDir} (no cache)")
    logger.info("=" * 60)

  initialize()
