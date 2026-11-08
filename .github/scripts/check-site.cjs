const assert = require("node:assert/strict");
const { once } = require("node:events");
const { readFile, readdir, mkdir } = require("node:fs/promises");
const { createServer } = require("node:http");
const { extname, join, resolve, sep } = require("node:path");
const { chromium } = require("playwright");

const site = resolve(__dirname, "../../site");
const prefix = "/haskell-game-io/";
const previews = process.argv[2];
const mediaTypes = {
  ".html": "text/html; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".svg": "image/svg+xml",
  ".png": "image/png",
  ".webp": "image/webp",
  ".jpg": "image/jpeg",
};

const server = createServer(async (request, response) => {
  try {
    const pathname = decodeURIComponent(new URL(request.url, "http://localhost").pathname);
    const file = resolve(site, pathname.slice(prefix.length) || "index.html");
    if (!pathname.startsWith(prefix) || !file.startsWith(site + sep)) {
      response.writeHead(404).end();
      return;
    }
    const body = await readFile(file);
    response.writeHead(200, { "Content-Type": mediaTypes[extname(file)] }).end(body);
  } catch {
    response.writeHead(404).end();
  }
});

async function checkLinks(page) {
  const broken = await page.evaluate(async () => {
    const seen = new Map();
    const broken = [];
    for (const anchor of document.querySelectorAll("a[href]")) {
      const url = new URL(anchor.href);
      if (url.origin !== location.origin) continue;
      const id = decodeURIComponent(url.hash.slice(1));
      url.hash = "";
      if (!seen.has(url.href)) {
        const response = await fetch(url);
        const target = response.ok
          ? new DOMParser().parseFromString(await response.text(), "text/html")
          : null;
        seen.set(url.href, target);
      }
      const target = seen.get(url.href);
      if (!target || (id && !target.getElementById(id))) broken.push(anchor.getAttribute("href"));
    }
    return broken;
  });
  assert.deepEqual(broken, [], "Broken site links");
}

async function checkDemos(page, device) {
  const next = page.locator("[data-input-next]");
  const time = page.locator("[data-input-time]");
  assert.equal(await time.textContent(), "0 ms");
  await next.press("Enter");
  assert.equal(await time.textContent(), "3 ms");
  assert.equal(await page.locator("[data-input-state]").textContent(), "キーを押している");
  await next.click();
  assert.equal(await time.textContent(), "4 ms");
  await next.click();
  assert.equal(await time.textContent(), "16 ms");
  assert.equal(await page.locator("[data-input-state]").textContent(), "キーは離れている");
  assert.match(await page.locator("[data-input-events]").textContent(), /押下と解放の2件/);
  assert.equal(await next.getAttribute("aria-disabled"), "true");
  if (device !== "narrow") {
    await page.locator("[data-input-demo]").screenshot({ path: join(previews, `input-${device}.png`) });
  }
  await page.locator("[data-input-reset]").click();
  assert.equal(await time.textContent(), "0 ms");

  const slider = page.locator("#audio-split");
  const blocks = page.locator("[data-audio-blocks]");
  const track = page.locator("[data-audio-track]");
  const cut = () => track.evaluate((element) => getComputedStyle(element).getPropertyValue("--sound-switch"));
  const initialCut = await cut();
  assert.equal(await blocks.textContent(), "127 ＋ 353フレーム");
  await slider.press("End");
  assert.equal(await blocks.textContent(), "479 ＋ 1フレーム");
  assert.equal(await cut(), initialCut);
  assert.ok(await track.evaluate((element) => parseFloat(element.style.getPropertyValue("--block-split")) > 99));
  await slider.press("ArrowLeft");
  assert.equal(await blocks.textContent(), "478 ＋ 2フレーム");
  if (device !== "narrow") {
    await page.locator("[data-audio-demo]").screenshot({ path: join(previews, `audio-${device}.png`) });
  }
}

async function check() {
  const routes = (await readdir(site)).filter((name) => name.endsWith(".html")).sort();
  assert.equal(routes.length, 2, "Expected a home page and a technical article");
  assert.ok(routes.includes("index.html"), "Missing home page");
  await mkdir(previews, { recursive: true });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const origin = `http://127.0.0.1:${server.address().port}`;
  const browser = await chromium.launch({ headless: true });
  try {
    for (const [device, width, height] of [
      ["desktop", 1440, 1000],
      ["mobile", 390, 844],
      ["narrow", 320, 800],
    ]) {
      const page = await browser.newPage({
        viewport: { width, height },
        reducedMotion: "reduce",
        isMobile: device !== "desktop",
        hasTouch: device !== "desktop",
      });
      for (const route of routes) {
        const errors = [];
        const pageError = (error) => errors.push(error.message);
        const consoleError = (message) => {
          if (message.type() === "error") errors.push(message.text());
        };
        page.on("pageerror", pageError);
        page.on("console", consoleError);
        const response = await page.goto(origin + prefix + route);
        assert.ok(response.ok(), `Failed to load ${route}`);
        await page.locator("img").evaluateAll((images) => Promise.all(images.map((image) => {
          image.loading = "eager";
          return image.decode();
        })));
        await page.screenshot({
          path: join(previews, `${route.slice(0, -5)}-${device}-full.png`),
          fullPage: true,
          animations: "disabled",
        });
        if (device !== "narrow") {
          await page.screenshot({ path: join(previews, `${route.slice(0, -5)}-${device}.png`) });
        }
        assert.equal(await page.locator("h1").count(), 1, `Heading on ${route}`);
        assert.equal(await page.locator("html").getAttribute("lang"), "ja");
        const contentWidth = await page.evaluate(() => document.documentElement.scrollWidth);
        assert.ok(contentWidth <= width, `${route} overflows at ${width}px (${contentWidth}px)`);
        await checkLinks(page);
        if (route === "implementation.html") {
          await checkDemos(page, device);
        } else {
          await page.getByRole("link", { name: "仕組みを読む" }).click();
          assert.equal(new URL(page.url()).pathname, prefix + "implementation.html");
        }
        assert.deepEqual(errors, [], `Browser errors on ${route}`);
        page.off("pageerror", pageError);
        page.off("console", consoleError);
        console.log(`${route} at ${width}px: links, layout, controls, and console OK`);
      }
      await page.close();
    }
    const staticPage = await browser.newPage({ javaScriptEnabled: false });
    await staticPage.goto(origin + prefix + "implementation.html");
    assert.equal(await staticPage.locator("[data-input-at]").count(), 4);
    assert.equal(await staticPage.locator("[data-input-controls]").isVisible(), false);
    assert.equal(await staticPage.locator("[data-audio-blocks]").textContent(), "127 ＋ 353フレーム");
    await staticPage.close();
  } finally {
    await browser.close();
  }
}

check().catch((error) => {
  console.error(error);
  process.exitCode = 1;
}).finally(() => server.close());
