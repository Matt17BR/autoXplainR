"""Calibrate visible graphics from numerical axis ticks and external R answers.

No chart data-value attribute or JavaScript scale helper supplies expected values.
The checks also reject missing labels, wrong model identities and incorrect support.
"""
import math


def on_axis(actual, expected, slope):
    return math.isclose(actual, expected, rel_tol=0, abs_tol=abs(slope) * .02)


def calibration(svg, horizontal=False):
    labels = svg.evaluate('''(svg, horizontal) => {
      const axis=svg.querySelector('.axr-axis'), bottom=+axis.getAttribute('y1');
      const left=+axis.getAttribute('x1');
      return [...svg.querySelectorAll('text')].filter(t=>t.textContent.trim()!=='' && Number.isFinite(Number(t.textContent)))
        .filter(t=>horizontal ? t.getAttribute('text-anchor')==='middle' && +t.getAttribute('y')>bottom && +t.getAttribute('y')<bottom+30 :
          t.getAttribute('text-anchor')==='end' && +t.getAttribute('x')<left && +t.getAttribute('y')<=bottom+5)
        .map(t=>({value:Number(t.textContent),pixel:horizontal ? +t.getAttribute('x') :
          [...svg.querySelectorAll('.axr-grid')].map(g=>+g.getAttribute('y1'))
            .sort((a,b)=>Math.abs(a-(+t.getAttribute('y')))-Math.abs(b-(+t.getAttribute('y'))))[0]}));
    }''', horizontal)
    if len({x['value'] for x in labels}) < 2:
        raise ValueError('The numeric axis has fewer than two usable labeled ticks')
    lo, hi = min(labels, key=lambda x:x['value']), max(labels, key=lambda x:x['value'])
    slope = (hi['value']-lo['value'])/(hi['pixel']-lo['pixel'])
    return lambda pixel: lo['value']+(pixel-lo['pixel'])*slope, slope


def points(svg, model=None):
    dots = svg.locator('[data-chart-point]').evaluate_all('''nodes=>nodes.map(n=>({
      model:n.dataset.modelId, x:+n.querySelector('.axr-point').getAttribute('cx'),
      y:+n.querySelector('.axr-point').getAttribute('cy'),
      frontier:n.querySelector('.axr-point').style.strokeWidth==='2.5'}))''')
    return [dot for dot in dots if model is None or dot['model']==model]


def label_collisions(svg):
    return svg.locator('.axr-model-label').evaluate_all('''labels => {
      const boxes=labels.map(label=>({text:label.textContent,box:label.getBoundingClientRect()}));
      return boxes.flatMap((a,i)=>boxes.slice(i+1).filter(b=>a.box.left<b.box.right &&
        a.box.right>b.box.left && a.box.top<b.box.bottom && a.box.bottom>b.box.top)
        .map(b=>[a.text,b.text]));
    }''')


def cost_geometry(plot, rows, metric, resource, higher):
    try:
        svg=plot.locator('svg'); x_value,x_slope=calibration(svg,True); y_value,y_slope=calibration(svg)
        dots=points(svg); available=[row for row in rows if row.get(metric) is not None and row.get(resource) is not None]
        if {dot['model'] for dot in dots}!={row['model_id'] for row in available} or len(dots)!=len(available):
            return False,'Plot omits or duplicates a measured model'
        if y_slope>=0 or x_slope<=0:
            return False,'Numerical axes must increase to the right and upward'
        labels=svg.locator('.axr-model-label').all_text_contents()
        normalize=lambda text:''.join(text.split())
        if sorted(map(normalize,labels))!=sorted(normalize(row['model']) for row in available):
            return False,'Direct labels do not identify every plotted model'
        collisions=label_collisions(svg)
        if collisions:
            return False,dict(overlapping_model_labels=collisions)
        for dot in dots:
            row=next(row for row in available if row['model_id']==dot['model'])
            if not on_axis(x_value(dot['x']),row[resource],x_slope) or not on_axis(y_value(dot['y']),row[metric],y_slope):
                return False,dict(model=dot['model'],plotted=[x_value(dot['x']),y_value(dot['y'])],expected=[row[resource],row[metric]])
            loss=-row[metric] if higher else row[metric]
            dominated=any(other[resource]<=row[resource] and (-other[metric] if higher else other[metric])<=loss and
                          (other[resource]<row[resource] or (-other[metric] if higher else other[metric])<loss) for other in available)
            if dot['frontier']==dominated:
                return False,f'Incorrect frontier outline for {dot["model"]}'
        return True,None
    except (ValueError,ZeroDivisionError) as error:
        return False,str(error)


def effect_geometry(panel, curve, model=None):
    try:
        figure=panel.locator('.axr-chart').first
        if not figure.count(): return False,'Missing effect chart'
        model=model or figure.get_attribute('data-primary-model')
        svg=figure.locator('svg'); dots=points(svg,model)
        inputs=next(iter(curve.values())); values=curve.get('accumulated_effect',curve.get('partial_dependence'))
        if len(dots)!=len(values):return False,'One plotted estimate is required per retained curve row'
        numeric=svg.get_attribute('data-axis-type')=='numeric'
        value,slope=calibration(svg,horizontal=not numeric)
        if (slope>=0 if numeric else slope<=0):return False,'The effect numerical axis is reversed'
        if numeric:
            x_value,x_slope=calibration(svg,True)
            joined=svg.locator('.axr-line').first.evaluate('n=>[...n.points].map(p=>[p.x,p.y])')
            if len(joined)!=len(dots) or any(abs(a-b)>.01 for p,q in zip(joined,[[d['x'],d['y']] for d in dots]) for a,b in zip(p,q)):
                return False,'The fitted line does not join its retained estimates'
        else:
            labels=svg.locator('text[text-anchor="end"]').evaluate_all('nodes=>nodes.filter(n=>+n.getAttribute("x")<+n.closest("svg").querySelector(".axr-axis").getAttribute("x1")).map(n=>({text:n.textContent,y:n.getBBox().y+n.getBBox().height/2}))')
            normalize=lambda text:''.join(text.split())
            if [normalize(label['text']) for label in labels]!=[normalize(str(x)) for x in inputs]:
                return False,'Categorical labels differ from the fitted levels'
        for i,dot in enumerate(dots):
            if not on_axis(value(dot['y'] if numeric else dot['x']),values[i],slope):
                return False,dict(row=i,plotted=value(dot['y'] if numeric else dot['x']),expected=values[i])
            if numeric and not on_axis(x_value(dot['x']),inputs[i],x_slope):return False,f'Wrong input spacing at row {i}'
            if not numeric and min(range(len(labels)),key=lambda j:abs(labels[j]['y']-dot['y']))!=i:
                return False,f'Estimate appears beside the wrong category at row {i}'
        if numeric and curve.get('n') and any(n is not None for n in curve['n']):
            bars=svg.locator('.axr-support').evaluate_all('nodes=>nodes.map(n=>({x:+n.getAttribute("x"),w:+n.getAttribute("width"),h:+n.getAttribute("height")}))')
            expected=[(i,n) for i,n in enumerate(curve['n']) if n is not None]
            if len(bars)!=len(expected):return False,'ALE support bars omit intervals'
            maximum=max(n for _,n in expected)
            for bar,(i,n) in zip(bars,expected):
                if i>0 and (not on_axis(x_value(bar['x']),inputs[i-1],x_slope) or
                            not on_axis(x_value(bar['x']+bar['w']),inputs[i],x_slope)):
                    return False,f'ALE support uses wrong bin boundaries at row {i}'
                if not math.isclose(bar['h']/max(b['h'] for b in bars),n/maximum,abs_tol=.00001):
                    return False,f'ALE support height does not represent its row count at row {i}'
        return True,None
    except (ValueError,ZeroDivisionError) as error:
        return False,str(error)


def importance_geometry(panel, rows):
    measured=panel.locator('[data-pick-feature]').evaluate_all('''nodes=>nodes.map(n=>{
      const track=n.querySelector('.importance-track').getBoundingClientRect(), zero=n.querySelector('.importance-zero').getBoundingClientRect();
      const bar=n.querySelector('.importance-bar').getBoundingClientRect(), interval=n.querySelector('.importance-interval')?.getBoundingClientRect();
      return {feature:n.dataset.pickFeature,zero:zero.left-track.left,start:bar.left-track.left,end:bar.right-track.left,
        low:interval?interval.left-track.left:null,high:interval?interval.right-track.left:null};})''')
    reference=next((row for row in rows if row.get('importance')),None)
    if reference is None:
        return all(abs(item['end']-item['start'])<.05 for item in measured),None
    ref=next(item for item in measured if item['feature']==reference['feature'])
    pixels_per_unit=(ref['end']-ref['start'])/abs(reference['importance'])
    for row in rows:
        item=next((item for item in measured if item['feature']==row['feature']),None)
        if item is None:return False,'A retained importance row is absent'
        expected_end=item['zero']+row['importance']*pixels_per_unit
        end=item['end'] if row['importance']>=0 else item['start']
        if abs(end-expected_end)>.15:return False,f'Importance length/sign differs for {row["feature"]}'
        for key,endpoint in [('low','conf_low'),('high','conf_high')]:
            expected=row.get(endpoint)
            if expected is not None and (item[key] is None or abs(item[key]-(item['zero']+expected*pixels_per_unit))>.15):
                return False,f'Visible shuffle interval differs for {row["feature"]} {endpoint}'
    return True,None
