import { promises as fs } from "node:fs";
import path from "node:path";
import { build, transformWithEsbuild } from "vite";

const projectRoot = process.cwd();
const staticDir = path.join(projectRoot, "jvm", "src", "main", "resources", "static");
const cssDir = path.join(staticDir, "css");
const distDir = path.join(staticDir, "dist");
const jsEntry = path.join(distDir, "main.js");
const skipJsMinify = process.env.SKIP_JS_MINIFY === "1";
const cssBundleName = "app-bundle.css";
const cssBundlePath = path.join(distDir, cssBundleName);
const sourceCssFiles = ["base.css"];

async function minifyFile(filePath, loader) {
  const source = await fs.readFile(filePath, "utf8");
  const transformed = await transformWithEsbuild(source, filePath, {
    loader,
    minify: true,
    target: "es2020",
    legalComments: "none"
  });
  await fs.writeFile(filePath, transformed.code, "utf8");
}

async function buildCssBundle() {
  const cssFiles = sourceCssFiles.map(name => path.join(cssDir, name));
  const tempEntryPath = path.join(projectRoot, ".vite-css-bundle-entry.mjs");
  const importLines = cssFiles.map(filePath => `import ${JSON.stringify(filePath)};`).join("\n");
  await fs.writeFile(tempEntryPath, `${importLines}\n`, "utf8");

  try {
    const viteResult = await build({
      logLevel: "silent",
      build: {
        write: false,
        emptyOutDir: false,
        cssCodeSplit: false,
        minify: "esbuild",
        target: "es2020",
        rollupOptions: { input: tempEntryPath }
      }
    });

    const outputs = Array.isArray(viteResult) ? viteResult : [viteResult];
    const cssAsset = outputs
      .flatMap(result => result.output ?? [])
      .find(asset => asset.type === "asset" && asset.fileName.endsWith(".css"));

    if (!cssAsset) throw new Error("Vite did not emit a CSS asset.");

    const cssSource = typeof cssAsset.source === "string"
      ? cssAsset.source
      : Buffer.from(cssAsset.source).toString("utf8");

    await fs.writeFile(cssBundlePath, cssSource, "utf8");
  } finally {
    await fs.rm(tempEntryPath, { force: true });
  }

  return cssFiles.length;
}

async function run() {
  await fs.mkdir(distDir, { recursive: true });
  await fs.access(jsEntry);
  if (!skipJsMinify) await minifyFile(jsEntry, "js");
  const cssCount = await buildCssBundle();
  const jsSummary = skipJsMinify ? "Kept JS unminified" : "Minified 1 JS file";
  console.log(`${jsSummary} and bundled ${cssCount} CSS file(s) into ${cssBundleName}.`);
}

run().catch(err => {
  console.error("Asset minification failed:", err);
  process.exitCode = 1;
});
