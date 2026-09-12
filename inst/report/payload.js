(() => {
  'use strict';
  const decoded = new WeakMap();
  function verifyZlibChecksum(bytes, compressed) {
    // The codec decodes DEFLATE but does not check zlib's Adler-32 trailer.
    // Check it here so a damaged block cannot silently change a data value.
    let a = 1, b = 0;
    for (let start = 0; start < bytes.length; start += 5552) {
      const end = Math.min(start + 5552, bytes.length);
      for (let i = start; i < end; i++) { a += bytes[i]; b += a; }
      a %= 65521; b %= 65521;
    }
    const n = compressed.length;
    const expected = ((compressed[n - 4] << 24) | (compressed[n - 3] << 16) |
      (compressed[n - 2] << 8) | compressed[n - 1]) >>> 0;
    if ((((b << 16) | a) >>> 0) !== expected) throw new Error('An embedded data block failed its integrity check.');
  }
  function decodeBlock(block) {
    if (!block || typeof block !== 'object' || !block.encoding) return block;
    if (decoded.has(block)) return decoded.get(block);
    let value;
    if (block.encoding === 'json') value = block.value;
    else if (block.encoding === 'zlib-json-v1') {
      if (!window.fflate?.unzlibSync) throw new Error('The embedded data decoder is unavailable.');
      const text = atob(block.data), compressed = new Uint8Array(text.length);
      for (let i = 0; i < text.length; i++) compressed[i] = text.charCodeAt(i);
      const bytes = window.fflate.unzlibSync(compressed);
      if (bytes.length !== block.bytes) throw new Error('An embedded data block is incomplete.');
      verifyZlibChecksum(bytes, compressed);
      value = JSON.parse(window.fflate.strFromU8(bytes));
    } else throw new Error('This report uses an unsupported data encoding.');
    decoded.set(block, value);
    return value;
  }
  function dataStore(payload) {
    if (payload.schema_version != null && payload.schema_version !== 2) throw new Error('Unsupported report data version.');
    const compact = payload.schema_version === 2;
    const profile = compact ? decodeBlock(payload.profile) : payload.profile;
    const rowData = payload.rows;
    const legacy = compact ? null : (Array.isArray(rowData) ? rowData : rowData ? [rowData] : []);
    if (compact && rowData && rowData.layout !== 'columns-v1') throw new Error('Unsupported row-data layout.');
    const length = compact ? (rowData ? rowData.length : 0) : legacy.length;
    if (!Number.isSafeInteger(length) || length < 0) throw new Error('The embedded row count is invalid.');
    const metadata = new Map(), columns = {raw: new Map(), processed: new Map()}, flags = {raw: new Map(), processed: new Map()};
    const own = (object, key) => object && Object.prototype.hasOwnProperty.call(object, key) ? object[key] : undefined;
    function vector(stage, name) {
      if (columns[stage].has(name)) return columns[stage].get(name);
      const block = own(rowData?.[stage], name);
      let result;
      if (!block) result = null;
      else if (block.encoding === 'reference') {
        if (stage !== 'processed' || block.stage !== 'raw') throw new Error('Unsupported column reference.');
        result = vector('raw', block.column);
        if (!result) throw new Error('An embedded column reference is incomplete.');
      } else result = decodeBlock(block);
      if (result && (!Array.isArray(result) || result.length !== length)) throw new Error('An embedded column has the wrong row count.');
      columns[stage].set(name, result);
      return result;
    }
    function meta(name, index) {
      if (!compact) return legacy[index]?.[name];
      if (!metadata.has(name)) {
        const values = decodeBlock(rowData.meta[name]);
        if (!Array.isArray(values) || values.length !== length) throw new Error('Embedded row identities are incomplete.');
        metadata.set(name, values);
      }
      return metadata.get(name)[index];
    }
    function nonfinite(index, name, stage) {
      if (!compact) {
        const values = legacy[index]?.nonfinite?.[stage];
        return Array.isArray(values) ? values.includes(name) : values === name;
      }
      if (!flags[stage].has(name)) flags[stage].set(name, new Set(own(rowData?.nonfinite?.[stage], name) || []));
      return flags[stage].get(name).has(index + 1);
    }
    return {
      profile, length, indices: Array.from({length}, (_, i) => i), meta, nonfinite,
      value: (index, name, stage) => compact ? vector(stage, name)?.[index] : legacy[index]?.[stage]?.[name],
      findKey: key => {
        if (!compact) return legacy.findIndex(row => row.row_key === key);
        if (!metadata.has('row_key') && length) meta('row_key', 0);
        return metadata.get('row_key')?.indexOf(key) ?? -1;
      },
      loadedColumns: () => ({raw: [...columns.raw.keys()], processed: [...columns.processed.keys()]})
    };
  }
  window.AutoXplainRPayload = {decodeBlock, dataStore};
})();
