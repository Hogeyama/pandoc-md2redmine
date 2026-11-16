// Saves a screenshot of each case's wiki page next to its golden file.
import { chromium } from 'playwright';
import { readdirSync } from 'node:fs';

const base = process.env.REDMINE_URL;
const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 960, height: 600 } });

for (const name of readdirSync('/cases').sort()) {
  await page.goto(`${base}/projects/cases/wiki/${name}`);
  // mermaid marks each diagram it has rendered with data-processed.
  await page.waitForFunction(() =>
    [...document.querySelectorAll('.mermaid')].every(e => e.dataset.processed));
  await page.locator('div.wiki-page').screenshot({ path: `/cases/${name}/redmica.png` });
  console.log(name);
}
await browser.close();
