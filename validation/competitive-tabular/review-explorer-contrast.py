"""Read-only follow-up for the exact explorer axe incomplete records."""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
from playwright.sync_api import sync_playwright

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--checks", type=Path, required=True)
parser.add_argument("--report", type=Path, required=True)
parser.add_argument("--output", type=Path, required=True)
args = parser.parse_args()
original = json.loads(args.checks.read_text())
report_hash = hashlib.sha256(args.report.read_bytes()).hexdigest()
assert report_hash == next(row["evidence"] for row in original["checks"]
                           if row["name"] == "regression: fixture identity")
args.output.mkdir(parents=True, exist_ok=False)

READ = r"""items => {
  const rect = r => ({left:r.left,right:r.right,top:r.top,bottom:r.bottom,width:r.width,height:r.height});
  const identify = e => e ? {tag:e.tagName,id:e.id,class:String(e.className?.baseVal ?? e.className ?? '')} : null;
  const rgba = value => {let v=value.match(/[\d.]+/g); return v ? v.map(Number) : null;};
  const over = (fg,bg) => fg.slice(0,3).map((c,i)=>c*(fg[3]??1)+bg[i]*(1-(fg[3]??1)));
  const lum = rgb => rgb.map(c=>c/255).map(c=>c<=.04045?c/12.92:((c+.055)/1.055)**2.4)
    .reduce((s,c,i)=>s+c*[.2126,.7152,.0722][i],0);
  function inspect(el) {
    let box=el.getBoundingClientRect(), style=getComputedStyle(el), svg=el.closest('svg');
    let ancestors=[], opacity=1;
    for(let p=el;p;p=p.parentElement){const s=getComputedStyle(p); opacity*=Number(s.opacity);
      ancestors.push({node:identify(p),background:s.backgroundColor,image:s.backgroundImage,
        opacity:s.opacity,overflowX:s.overflowX,overflowY:s.overflowY,box:rect(p.getBoundingClientRect()),
        scrollWidth:p.scrollWidth,clientWidth:p.clientWidth,scrollLeft:p.scrollLeft});}
    let background=[255,255,255];
    for(const a of [...ancestors].reverse()){const color=rgba(a.background);if(color)background=over(color,background);}
    let paint=svg ? style.fill : style.color, foreground=rgba(paint);
    if(foreground){foreground[3]=(foreground[3]??1)*opacity*(svg?Number(style.fillOpacity):1);foreground=over(foreground,background);}
    let ratio=foreground?(Math.max(lum(foreground),lum(background))+.05)/(Math.min(lum(foreground),lum(background))+.05):null;
    let clips=ancestors.slice(1).filter(a=>a.node.tag!=='HTML'&&a.node.tag!=='BODY')
      .filter(a=>(a.overflowX!=='visible'&&(box.left<a.box.left-1||box.right>a.box.right+1))||
                 (a.overflowY!=='visible'&&(box.top<a.box.top-1||box.bottom>a.box.bottom+1)));
    let range=document.createRange();range.selectNodeContents(el);
    let textRects=[...range.getClientRects()].filter(r=>r.width&&r.height).map(r=>{
      let x=(r.left+r.right)/2,y=(r.top+r.bottom)/2,hit=document.elementFromPoint(x,y);
      return {box:rect(r),inViewport:x>=0&&x<innerWidth&&y>=0&&y<innerHeight,
        topElement:identify(hit),unobscured:!!hit&&(el===hit||el.contains(hit)||hit.contains(el))};});
    let parentColor=getComputedStyle(el.parentElement).color,parentRgb=rgba(parentColor);
    let parentRatio=parentRgb?(Math.max(lum(parentRgb),lum(background))+.05)/(Math.min(lum(parentRgb),lum(background))+.05):null;
    return {node:identify(el),text:el.textContent,html:el.outerHTML,box:rect(box),
      parentText:el.parentElement.textContent,parentColor,parentTextContrastRatio:parentRatio,
      rendered:!!box.width&&!!box.height&&style.display!=='none'&&style.visibility==='visible',
      ariaHidden:el.closest('[aria-hidden="true"]')!==null,paint,color:style.color,fill:style.fill,
      fontSize:style.fontSize,fontWeight:style.fontWeight,opacity,background,foreground,
      contrastRatio:ratio,backgroundImages:ancestors.filter(a=>a.image!=='none'),
      svg:svg?{box:rect(svg.getBoundingClientRect()),label:svg.getAttribute('aria-label')}:null,
      clippedByAncestors:clips,textRects,ancestors};
  }
  return items.map(item=>{let matches=[...document.querySelectorAll(item.target.join(' '))];
    let visible=matches.filter(el=>{let b=el.getBoundingClientRect(),s=getComputedStyle(el);return b.width&&b.height&&s.display!=='none'&&s.visibility==='visible';});
    return {...item,matchingCount:matches.length,renderedMatches:visible.map(inspect)};});
}"""

records = []
navigation = []
with sync_playwright() as runtime:
    browser = runtime.chromium.launch()
    context = browser.new_context(viewport={"width": 1440, "height": 1000})
    context.route("http://**/*", lambda route: route.abort())
    context.route("https://**/*", lambda route: route.abort())
    page = context.new_page()
    page.goto(args.report.resolve().as_uri())
    page.select_option("#resource-select", "prediction_time_ms")
    page.select_option("#metric-select", "rmse")
    for scan in original["accessibility_incomplete"]:
        page.set_viewport_size({"width": scan["width"], "height": 1000})
        page.locator(f'[data-page-link="{scan["tab"]}"]').click()
        page.evaluate("()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))")
        nodes = []
        for group in scan["incomplete_details"]:
            for node in group["nodes"]:
                nodes.append({"target": node["target"], "sourceHtml": node["html"],
                              "reasons": [check.get("data", {}).get("messageKey")
                                          for check in node["any"] + node["all"] + node["none"]]})
        inspected = page.evaluate(READ, nodes)
        scrolled = []
        for node in inspected:
            if node["reasons"] == ["elmPartiallyObscured"]:
                continue  # Mobile navigation receives the separate keyboard check below.
            if not any(r["inViewport"] for e in node["renderedMatches"] for r in e["textRects"]):
                page.locator(" ".join(node["target"])).evaluate_all("""elements=>{
                  const visible=elements.filter(el=>{let b=el.getBoundingClientRect(),s=getComputedStyle(el);
                    return b.width&&b.height&&s.display!=='none'&&s.visibility==='visible';});
                  if(visible.length!==1)throw Error('Scroll target is not uniquely rendered');
                  visible[0].scrollIntoView({block:'center',inline:'nearest',behavior:'instant'});
                }""")
                page.evaluate("()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))")
                reread = page.evaluate(READ, [{key:node[key] for key in ["target","sourceHtml","reasons"]}])[0]
                scrolled.append({"target":node["target"],"textRects":[r for e in reread["renderedMatches"] for r in e["textRects"]]})
        page.evaluate("scrollTo({top:0,behavior:'instant'})")
        records.append({"width": scan["width"], "tab": scan["tab"],
                        "state": page.evaluate("AutoXplainRReport.getState()"),
                        "nodes": inspected,"scrolledVisibility":scrolled})
        if (scan["width"], scan["tab"]) in [(320,"overview"),(390,"selection"),(1440,"patterns")]:
            page.screenshot(path=str(args.output / f'{scan["tab"]}-{scan["width"]}.png'),full_page=True)
    for width in [320,390]:
        page.set_viewport_size({"width": width,"height":1000})
        page.locator('[data-page-link="overview"]').click()
        page.locator('[data-page-link="overview"]').focus()
        for i in range(7):
            navigation.append({"width":width,"step":i,"evidence":page.evaluate("""()=>{
              const e=document.activeElement,b=e.getBoundingClientRect(),p=e.parentElement.getBoundingClientRect();
              return {id:e.id,selected:e.getAttribute('aria-selected'),left:b.left,right:b.right,
                parentLeft:p.left,parentRight:p.right,fullyVisible:b.left>=p.left-1&&b.right<=p.right+1,
                scrollLeft:e.parentElement.scrollLeft,activePage:document.querySelector('.workspace-page:not([hidden])')?.id};}""")})
            page.keyboard.press("ArrowRight")
            page.evaluate("()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))")
    version = browser.version
    browser.close()

reasons = Counter(reason for scan in records for node in scan["nodes"] for reason in node["reasons"])
summary = {"scope":"Exact saved report; read-only computed-style, geometry and keyboard review of prior incomplete occurrences.",
           "report":str(args.report.resolve()),"report_sha256":report_hash,
           "source_checks_sha256":hashlib.sha256(args.checks.read_bytes()).hexdigest(),
           "browser":version,"scans":len(records),"occurrences":sum(len(s["nodes"]) for s in records),
           "reason_counts":reasons,"records":records,"navigation":navigation}
(args.output/"computed-proof.json").write_text(json.dumps(summary,indent=2)+"\n")
print(json.dumps({"occurrences":summary["occurrences"],"reason_counts":reasons,
                  "missing_rendered_nodes":sum(not n["renderedMatches"] for s in records for n in s["nodes"]),
                  "keyboard_failures":sum(not n["evidence"]["fullyVisible"] for n in navigation)},indent=2))
