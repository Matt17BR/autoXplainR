import json
import os
from pathlib import Path
from playwright.sync_api import sync_playwright
root=Path(__file__).resolve().parents[2]
with sync_playwright() as p:
 browser=p.chromium.launch(executable_path=os.environ.get('CHROME_PATH', '/usr/bin/google-chrome'),headless=True)
 page=browser.new_page()
 out={}
 for width in [1280,390]:
  page.set_viewport_size({'width':width,'height':900})
  page.goto((root/'pkgdown/assets/model-report.html').as_uri(),wait_until='load')
  page.add_script_tag(path=os.environ['AXE_PATH'])
  audit=page.evaluate('async () => await axe.run(document, {runOnly: {type: "tag", values: ["wcag2a", "wcag2aa", "wcag21aa", "wcag22aa"]}})')
  summary=page.evaluate('''() => ({height:document.documentElement.scrollHeight,width:document.documentElement.scrollWidth,words:document.body.innerText.trim().split(/\\s+/).length,cards:document.querySelectorAll('.metric').length,sections:[...document.querySelectorAll('main>section')].map(e=>({title:e.querySelector('h2')?.innerText,top:e.getBoundingClientRect().top,height:e.getBoundingClientRect().height})),effect_text:[...document.querySelectorAll('.effect-plot')].map(e=>({text:e.textContent,aria:e.getAttribute('aria-label')})),navPosition:getComputedStyle(document.querySelector('nav')).position})''')
  out[str(width)]={'measurement':summary,'axe_version':audit['testEngine']['version'],'violations':[{'id':v['id'],'impact':v['impact'],'description':v['description'],'nodes':[{'html':n['html'],'failureSummary':n.get('failureSummary'),'target':n['target']} for n in v['nodes']]} for v in audit['violations']],'incomplete':[{'id':v['id'],'count':len(v['nodes'])} for v in audit['incomplete']]}
 browser.close()
(root/'validation/audit-evidence/browser-audit.json').write_text(json.dumps(out,indent=2))
print(json.dumps(out,indent=2))
