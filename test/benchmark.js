const sha512 = require('../')
const crypto = require('crypto')
const sha512js = require('js-sha512').sha512

// timing benchmark
{
  console.log('single instance: 10000 x 16kB buffer.\n')
  const buf = crypto.randomBytes(16384)

  const hash = sha512()
  const jsHash = sha512js.create()
  const refHash = crypto.createHash('sha512')

  console.time('wasm')
  for (let i = 0; i < 10000; i++) {
    hash.update(buf)
  }
  const res = hash.digest('hex')
  console.timeEnd('wasm')

  console.time('js')
  for (let i = 0; i < 10000; i++) {
    jsHash.update(buf)
  }
  const jsRes = jsHash.hex()
  console.timeEnd('js')

  console.time('native')
  for (let i = 0; i < 10000; i++) {
    refHash.update(buf)
  }
  const refRes = refHash.digest('hex')
  console.timeEnd('native')

  if (res !== refRes || res !== jsRes) throw new Error('hash failed.')
}

{
  console.log('\n10000 instances: 256B buffer.\n')
  const buf = crypto.randomBytes(256 * 10000)

  console.time('wasm')
  for (let i = 0; i < 10000; i++) {
    const hash = sha512()
    hash.update(buf.subarray(256 * i, 256 * (i + 1))).digest()
  }
  console.timeEnd('wasm')

  console.time('js')
  for (let i = 0; i < 10000; i++) {
    const jsHash = sha512js.create()
    jsHash.update(buf.subarray(256 * i, 256 * (i + 1))).digest()
  }
  console.timeEnd('js')

  console.time('native')
  for (let i = 0; i < 10000; i++) {
    const refHash = crypto.createHash('sha512')
    refHash.update(buf.subarray(256 * i, 256 * (i + 1))).digest()
  }
  console.timeEnd('native')

  for (let i = 0; i < 10000; i++) {
    const hash = sha512()
    const refHash = crypto.createHash('sha512')

    const wasm = hash.update(buf.subarray(256 * i, 256 * (i + 1))).digest('hex')
    const native = refHash.update(buf.subarray(256 * i, 256 * (i + 1))).digest('hex')

    if (wasm !== native) console.log('fail')
  }
}
