var __commonJS = (cb, mod) => function __require() {
  return mod || (0, cb[Object.keys(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
};
var __toBinary = /* @__PURE__ */ (() => {
  var table = new Uint8Array(128);
  for (var i = 0; i < 64; i++)
    table[i < 26 ? i + 65 : i < 52 ? i + 71 : i < 62 ? i - 4 : i * 4 - 205] = i;
  return (base64) => {
    var n = base64.length, bytes2 = new Uint8Array((n - (base64[n - 1] == "=") - (base64[n - 2] == "=")) * 3 / 4 | 0);
    for (var i2 = 0, j = 0; i2 < n; ) {
      var c0 = table[base64.charCodeAt(i2++)], c1 = table[base64.charCodeAt(i2++)];
      var c2 = table[base64.charCodeAt(i2++)], c3 = table[base64.charCodeAt(i2++)];
      bytes2[j++] = c0 << 2 | c1 >> 4;
      bytes2[j++] = c1 << 4 | c2 >> 2;
      bytes2[j++] = c2 << 6 | c3;
    }
    return bytes2;
  };
})();

// wasm-binary:./sha512.wat
var require_sha512 = __commonJS({
  "wasm-binary:./sha512.wat"(exports2, module2) {
    module2.exports = __toBinary("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");
  }
});

// wasm-module:./sha512.wat
var bytes = require_sha512();
var compiled = new WebAssembly.Module(bytes);
module.exports = (imports) => {
  const instance = new WebAssembly.Instance(compiled, imports);
  return instance.exports;
};
