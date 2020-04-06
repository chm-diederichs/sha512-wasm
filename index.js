const assert = require('nanoassert')
const wasm = require('./sha512.js')({
  imports: {
    debug: {
      log (...args) {
        console.log(...args.map(int => (int >>> 0).toString(16).padStart(8, '0')))
      },
      log_tee (arg) {
        console.log((arg >>> 0).toString(16).padStart(8, '0'))
        return arg
      }
    }
  }
})

let head = 0
// assetrt head % 8 === 0 to guarantee alignment
const freeList = []

module.exports = Sha512
const SHA512_BYTES = module.exports.SHA512_BYTES = 64

function Sha512 () {
  if (!(this instanceof Sha512)) return new Sha512()
  if (!(wasm && wasm.exports)) throw new Error('WASM not loaded. Wait for Sha512.ready(cb)')

  if (!freeList.length) {
    freeList.push(head)
    head += 208
  }

  this.finalized = false
  this.digestLength = SHA512_BYTES
  this.pointer = freeList.pop()
  this.alignOffset = 0

  wasm.memory.fill(0, this.pointer, this.pointer + 208)

  if (this.pointer + this.digestLength > wasm.memory.length) wasm.realloc(this.pointer + 208)
}

Sha512.prototype.update = function (input, enc) {
  assert(this.finalized === false, 'Hash instance finalized')

  if (head % 8 !== 0) head += 8 - head % 8
  assert(head % 8 === 0, 'input should be aligned for int64')

  let [ inputBuf, length ] = formatInput(input, enc)

  assert(inputBuf instanceof Uint8Array, 'input must be Uint8Array or Buffer')

  if (head + input.length > wasm.memory.length) wasm.realloc(head + input.length)

  wasm.memory.fill(0, head, head + this.alignOffset)
  wasm.memory.set(inputBuf, head + this.alignOffset)

  this.alignOffset = wasm.exports.sha512_monolith(this.pointer, head, head + length + this.alignOffset, 0)

  return this
}

Sha512.prototype.digest = function (enc, offset = 0) {
  assert(this.finalized === false, 'Hash instance finalized')
  this.finalized = true

  freeList.push(this.pointer)

  wasm.memory.fill(0, head, head + 16)
  wasm.exports.sha512_monolith(this.pointer, head, head, 1)

  const resultBuf = Buffer.from(wasm.memory.subarray(this.pointer, this.pointer + this.digestLength))
  // const resultBuf = int64reverse(wasm.memory, this.pointer, this.digestLength)
  
  if (!enc) {
    return resultBuf
  }

  if (typeof enc === 'string') {
    return resultBuf.toString(enc)
  }

  assert(enc instanceof Uint8Array, 'input must be Uint8Array or Buffer')
  assert(enc.byteLength >= this.digestLength + offset, 'input must be Uint8Array or Buffer')

  for (let i = 0; i < this.digestLength; i++) {
    enc[i + offset] = resultBuf[i]
  }

  return enc
}

Sha512.ready = function (cb) {
  if (!cb) cb = noop
  if (!wasm) return cb(new Error('WebAssembly not supported'))

  var p = new Promise(function (reject, resolve) {
    wasm.onload(function (err) {
      if (err) resolve(err)
      else reject()
      cb(err)
    })
  })

  return p
}

Sha512.prototype.onetime = function (input, enc) {
  assert(this.finalized === false, 'Hash instance finalized')
  this.finalized = true

  if (head % 8 !== 0) head += 8 - head % 8
  assert(head % 8 === 0, 'input should be aligned for int64')

  let [ inputBuf, length ] = formatInput(input, enc)

  assert(inputBuf instanceof Uint8Array, 'input must be Uint8Array or Buffer')

  if (head + input.length > wasm.memory.length) wasm.realloc(head + input.length)

  if (this.leftover != null) {
    wasm.memory.set(this.leftover, head)
    wasm.memory.set(inputBuf, this.leftover.byteLength + head)
  } else {
    wasm.memory.set(inputBuf, head)
  }

  const overlap = this.leftover ? this.leftover.byteLength : 0
  const leftover = wasm.exports.sha512_monolith(this.pointer, head, head + length + overlap, 1)
  const resultBuf = Buffer.from(wasm.memory.subarray(this.pointer, this.pointer + this.digestLength))
  // const resultBuf = int64reverse(wasm.memory, this.pointer, this.digestLength)

  this.leftover = inputBuf.slice(inputBuf.byteLength - leftover)

  if (!enc) {
    return resultBuf
  }

  if (typeof enc === 'string') {
    return resultBuf.toString(enc)
  }

  assert(enc instanceof Uint8Array, 'input must be Uint8Array or Buffer')
  assert(enc.byteLength >= this.digestLength + offset, 'input must be Uint8Array or Buffer')

  for (let i = 0; i < this.digestLength; i++) {
    enc[i + offset] = resultBuf[i]
  }

  return enc
}

Sha512.prototype.double = function (input, enc) {
  assert(this.finalized === false, 'Hash instance finalized')
  this.finalized = true

  if (head % 8 !== 0) head += 8 - head % 8
  assert(head % 8 === 0, 'input should be aligned for int64')

  let [ inputBuf, length ] = formatInput(input, enc)

  assert(inputBuf instanceof Uint8Array, 'input must be Uint8Array or Buffer')

  if (head + input.length > wasm.memory.length) wasm.realloc(head + input.length)

  if (this.leftover != null) {
    wasm.memory.set(this.leftover, head)
    wasm.memory.set(inputBuf, this.leftover.byteLength + head)
  } else {
    wasm.memory.set(inputBuf, head)
  }

  // console.log(hexSlice(wasm.memory, head, 64, 'head'))
  const overlap = this.leftover ? this.leftover.byteLength : 0
  var leftover = wasm.exports.sha512_monolith(this.pointer, head, head + length + overlap, 1)
  // console.log(hexSlice(wasm.memory, this.pointer, 64), 'state')

  leftover = wasm.exports.sha512_monolith(this.pointer, this.pointer, this.pointer + 64, 1)
  // console.log(hexSlice(wasm.memory, this.pointer + 64, 64), 'state double')
  const resultBuf = Buffer.from(wasm.memory.subarray(this.pointer, this.pointer + this.digestLength))

  if (!enc) {
    return resultBuf
  }

  if (typeof enc === 'string') {
    return resultBuf.toString(enc)
  }

  assert(enc instanceof Uint8Array, 'input must be Uint8Array or Buffer')
  assert(enc.byteLength >= this.digestLength + offset, 'input must be Uint8Array or Buffer')

  for (let i = 0; i < this.digestLength; i++) {
    enc[i + offset] = resultBuf[i]
  }

  return enc
}

Sha512.prototype.ready = Sha512.ready

function noop () {}

function formatInput (input, enc) {
  var result = Buffer.isBuffer(input) ? input : Buffer.from(input, enc)

  return [result, result.byteLength]
}

function int64reverse (buf, start, len) {
  // TODO change to dataview
  const result = Buffer.allocUnsafe(len)

  for (let i = 0; i < len; i++) {
    const index = Math.floor(i / 8) * 8 + 7 - i % 8
    result[index] = buf[start + i]
  }

  return result
}

function hexSlice (buf, start, len) {
  var str = ''
  for (var i = 0; i < len; i++) str += toHex(buf[start + i])
  return str
}

function toHex (n) {
  if (n < 16) return '0' + n.toString(16)
  return n.toString(16)
}

