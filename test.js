const sha512 = require('./')
const test = require('tape')

require('sha-test').sha512(sha512)

test('pass error argument if ready callback fail', t => {
  const wasm = require('./sha512.js')({
    imports: null // importing null will generate an error
  })

  wasm.onload(err => {
    t.ok(err)
    t.end()
  })
})
