import sys,json,copy,base64,zlib,os
from pathlib import Path
sys.path.insert(0,str(Path.cwd()/'validation'))
from report_payload import decode_data_payload, decode_block
root=Path(os.environ.get('AXR_COMPACT_REVIEW_OUTPUT', Path.home()/'.cache/autoxplain-scale-0.7.0/independent-review/compact'))
cells=0
for source in sorted(root.glob('*-legacy.json')):
    prefix=source.name.removesuffix('legacy.json')
    old=json.loads(source.read_text())
    records=decode_data_payload(json.loads((root/(prefix+'records.json')).read_text()))
    columns=decode_data_payload(json.loads((root/(prefix+'columns.json')).read_text()))
    assert records==columns, prefix+' record/direct mismatch'
    old_rows=old['rows']
    if isinstance(old_rows,dict): old_rows=[old_rows]
    for before,after in zip(old_rows,columns['rows'],strict=True):
        for stage in ('raw','processed'):
            flags=before['nonfinite'][stage]
            before['nonfinite'][stage]=[flags] if isinstance(flags,str) else flags
            if before[stage] is not None:
                value=before[stage]['x']
                if isinstance(value,list):
                    assert len(value)==1
                    before[stage]['x']=value[0]
                cells+=len(before[stage])
        assert before==after,(prefix,before,after)
    for stage,data in old['profile']['stages'].items():
        for name,value in data['columns'].items():
            axis=value['axis']
            compact=columns['profile']['stages'][stage]['columns'][name]['axis']
            if 'known_levels' not in axis: continue
            def array(x): return x if isinstance(x,list) else [x]
            def classify(value,axis):
                if value is None:return None
                if value in array(axis['levels']):return 'displayed'
                return 'other' if value in array(axis['known_levels']) else 'novel'
            for row in columns['rows']:
                if row[stage] is not None:
                    assert classify(row[stage][name],axis)==classify(row[stage][name],compact)
            axis.pop('known_levels'); compact.pop('known_levels')
    old['rows']=columns['rows']
    assert old==columns,prefix+' full remaining payload mismatch'
compressed=json.loads((root/'compressed.json').read_text())
assert decode_block(compressed)==['</script> 你好']*4000
print(json.dumps({'cases':8,'raw_processed_cells':cells,'record_direct_equal':True,'legacy_equal_except_AsIs_scalar_normalization_and_wire_known_levels':True,'all_exported_category_classifications_equal':True,'independent_python_zlib_hostile_unicode':True},indent=2))
