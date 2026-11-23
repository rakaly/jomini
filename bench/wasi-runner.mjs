// Runs a wasm32-wasip1 benchmark on V8. Set as the cargo runner in
// .cargo/config.toml.
import fs from "node:fs";
import path from "node:path";
import { WASI } from "node:wasi";

const [wasm, ...args] = process.argv.slice(2);
const root = path.dirname(import.meta.dirname);

// Keep the results apart from the native ones, so that a wasm run is not
// compared with a native baseline.
const criterion = path.join(root, "bench", "target", "criterion-wasm");
fs.mkdirSync(criterion, { recursive: true });

const preopens = { [root]: root, "/criterion": criterion };
const env = { CRITERION_HOME: "/criterion" };

// The sandbox does not follow a symlink out of the repository, so the tokens
// get a mount of their own.
const tokens = process.env.PDX_TOKENS_DIR ?? path.join(root, "assets", "tokens");
if (fs.existsSync(tokens)) {
  preopens["/tokens"] = fs.realpathSync(tokens);
  env.PDX_TOKENS_DIR = "/tokens";
}

const wasi = new WASI({
  version: "preview1",
  args: [path.basename(wasm), ...args],
  env,
  preopens,
});
const module = await WebAssembly.compile(fs.readFileSync(wasm));
const instance = await WebAssembly.instantiate(module, wasi.getImportObject());
process.exitCode = wasi.start(instance);
