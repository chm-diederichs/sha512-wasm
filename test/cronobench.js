const cronometro = require('cronometro')
const { createHash, randomBytes } = require('crypto')
const sha512 = require('../')
const wasm = require('../sha512')()
const shortData = randomBytes(64)
const mediumData = randomBytes(384)
const longData = randomBytes(4096)
const megaData = randomBytes(2 ** 23)
const output512 = Buffer.alloc(64)

function size (name, data) {
  return cronometro({
    'crypto wasm sha512 (no prealloc)': function () {
      sha512().update(data).digest()
    },
    'crypt wasm sha512 (prealloc)': function () {
      sha512().update(data).digest(output512)
    },
    'crypto sha512 (no prealloc)': function () {
      createHash('sha512').update(data).digest()
    },
    'crypto sha512 (prealloc)': function () {
      createHash('sha512').update(data).digest(output512)
    }
    // 'crypto sha512 (no prealloc, digest)': function () {
    //   createHash('sha512').update(data).digest('hex')
    // }
  }, {
    iterations: 1000,
    print: {
      compare: true,
      compareMode: 'base'
    }
  })
}

;(async () => {
  await size('64 bytes', shortData)
  await size('384 bytes', mediumData)
  await size('4096 bytes', longData)
  await size('4 MB', megaData)
})()
