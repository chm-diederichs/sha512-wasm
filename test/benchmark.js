const sha512 = require('../')
const crypto = require('crypto')
const sha512js = require('js-sha512').sha512

// timing benchmark
{
  const buf = Buffer.alloc(16384)
  crypto.randomFillSync(buf)

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
