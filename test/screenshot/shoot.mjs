// Saves a screenshot of each case's wiki page next to its golden file.
import { chromium } from 'playwright';
import { readdirSync } from 'node:fs';

const base = process.env.REDMINE_URL;
const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 960, height: 600 } });

for (const name of readdirSync('/cases').sort()) {
  await page.goto(`${base}/projects/cases/wiki/${name}`);
  // Open every {{collapse}}, including ones that an opened one reveals.
  const closed = page.locator('div.wiki-page a.collapsible.icon-collapsed:visible');
  while (await closed.count() > 0) {
    await closed.first().click();
    await page.waitForFunction(() => jQuery(':animated').length === 0);
  }
  // Keep the last clicked link from showing its hover style.
  await page.mouse.move(0, 0);
  // mermaid marks each diagram it has rendered with data-processed.
  await page.waitForFunction(() =>
    [...document.querySelectorAll('.mermaid')].every(e => e.dataset.processed));
  await page.locator('div.wiki-page').screenshot({ path: `/cases/${name}/redmica.png` });
  console.log(name);
}
await browser.close();
