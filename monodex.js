const assert = require('nanoassert')
const wasm = require('./monolith.js')({
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

let head = 704
// assetrt head % 8 === 0 to guarantee alignment
const freeList = []

module.exports = Sha512

const hashLength = 64
const wordConstantsLength = 512


function Sha512 () {
  if (!(this instanceof Sha512)) return new Sha512()
  if (!(wasm && wasm.exports)) throw new Error('WASM not loaded. Wait for Blake2b.ready(cb)')

  if (!freeList.length) {
    freeList.push(head)
    head += 696
  }

  this.finalized = false
  this.digestLength = 64
  this.leftover = 0
  this.pointer = freeList.pop()

  wasm.memory.fill(0, 0, hashLength + wordConstantsLength)

  if (this.pointer + hashLength + wordConstantsLength > wasm.memory.length) wasm.realloc(this.pointer + 312)
  
  // wasm.exports.sha512_init(0 , this.digestLength) //(this.pointer, this.digestLength)
}

Sha512.prototype.update = function (input) {
  // assert input % 8 === 0 for alignment

  let [ inputBuf, length ] = formatInput(input)
  assert(this.finalized === false, 'Hash instance finalized')
  assert(inputBuf instanceof Uint8Array, 'input must be Uint8Array or Buffer')
  if (head + input.length > wasm.memory.length) wasm.realloc(head + input.length)

  // console.log(this.leftover + head)
  wasm.memory.set(inputBuf, this.leftover + head)
  // console.log(inputBuf)
  console.log(hexSlice(wasm.memory, 1400, 128))

  // console.log(input)
  console.log(head)
  this.leftover = wasm.exports.sha512_monolith(this.pointer, head, head + length + this.leftover, 0)

  // head += length

  return this
}

Sha512.prototype.digest = function (enc) {
  // console.log(wasm.memory.subarray(288, 388), 'input data')
  assert(this.finalized === false, 'Hash instance finalized')
  this.finalized = true
  // console.log(hexSlice(wasm.memory, 1400, 128))

  freeList.push(this.pointer)
  console.log(12345678)
  console.log(hexSlice(wasm.memory, 1400, 128))

  wasm.exports.sha512_monolith(704, 1400, 1400 + this.leftover, 1)
  // console.log(hexSlice(wasm.memory, 704, 128))
  // console.log(hexSlice(wasm.memory, 1400, 128))
  // console.log(wasm.memory.subarray(this.pointer, this.pointer + 32), head, this.pointer)


  // if (!enc || end === 'binary') {    
  //   return wasm.memory.slice(this.pointer, this.pointer + 32)
  // }

  return int64reverse(wasm.memory, 0, 64)
  if (enc === 'hex') {
    return hexSlice(wasm.memory, 0, 32)
  }

  assert(enc instanceof Uint8Array && enc.length >= 32, 'input must be Uint8Array or Buffer')
  for (let i = 0; i < 32; i++) {
    enc[i] = wasm.memory[this.pointer + 32 + i]
  }

  return enc
}

Sha512.ready = function (cb) {
  if (!cb) cb = noop
  if (!wasm) return cb(new Error('WebAssembly not supported'))

  var p = new Promise(function (reject, result) {
    wasm.onload(function (err) {
      if (err) resolve(err)
      else reject()
      cb(err)
    })
  })

  return p
}

Sha512.prototype.ready = Sha512.ready

function noop () {}

function formatInput (input) {
  const value = new Uint8Array(Buffer.from(input))
  return [value, value.byteLength]

  if (input instanceof Uint8Array) return input

  const inputArray = new Uint32Array(Math.ceil(input.length / 4))

  const buf = Buffer.alloc(inputArray.byteLength)
  buf.set(Buffer.from(input, 'utf8'), 0)

  let i = 0

  for (; i < buf.byteLength / 4; i++) {
    inputArray[i] = buf.readUInt32LE(4 * i)
  }

  return [
    new Uint8Array(inputArray.buffer),
    input.length
  ]
}

function int64reverse (buf, start, len) {
  var str = ''
  var chars = []

  for (let i = 0; i < len; i++) {
    chars.push(toHex(buf[start + i]))

    if ((i + 1) % 8 === 0) {
      str += chars.reverse().join('')
      chars = []
    }
  }

  return str
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
