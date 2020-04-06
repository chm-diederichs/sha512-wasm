const sha512 = require('../')
const crypto = require('crypto')
const tape = require('tape')
const sha512js = require('js-sha512').sha512
const vectors = require('./vectors.json')

tape('empty input', function (t) {
  const hash = sha512().digest('hex')
  const ref = crypto.createHash('sha512').digest('hex')
  t.equal(hash, ref, 'consistent for empty input')
  t.end()
})

tape('check each byte length < 128', function (t) {
  for (let j = 0; j < 128; j++) {
    var buf = Buffer.alloc(j)
    for (let i = 0; i < buf.byteLength; i++) {
      buf[i] = i
    }

    const hash = new sha512().update(buf).digest()
    const ref = crypto.createHash('sha512').update(buf).digest()
    t.same(hash, ref)
  }

  t.end()
})

tape('naive input fuzz', function (t) {
  for (let i = 0; i < 10; i++) {
    const buf = crypto.randomBytes(2 ** 18 * Math.random())

    const hash = sha512().update(buf).digest('hex')
    const ref = crypto.createHash('sha512').update(buf).digest('hex')
    t.ok(hash === ref)
  }
  t.end()
})

tape('test power of 2 length buffers', function (t) {
  for (let i = 0; i < 27; i++) {
    const hash = sha512()
    const refHash = crypto.createHash('sha512')
    
    const buf = Buffer.alloc(2 ** i)

    const test = hash.update(buf).digest('hex')
    const ref = refHash.update(buf).digest('hex')

    t.same(test, ref, `2^${i}`)
  }
  t.end()
})

tape('fuzz multiple updates', function (t) {
  const hash = sha512()
  const refHash = crypto.createHash('sha512')

  for (let i = 0; i < 1; i++) {
    const buf = crypto.randomBytes(2**16 * Math.random())

    hash.update(buf)
    refHash.update(buf)
  }

  t.same(hash.digest(), refHash.digest(), 'multiple updates consistent')
  t.end()
})

tape('crypto-browserify test vectors', function (t) {
  let i = 0
  for (let vector of vectors) {
    const buf = Buffer.from(vector.input, 'base64')
    const hash = sha512().update(buf).digest('hex')
    t.equal(hash, vector.hash, `input ${i}`)
    i++
  }
  t.end()
})

tape('several instances updated simultaneously', function (t) {
  const hash1 = sha512() 
  const hash2 = sha512()
  const refHash = crypto.createHash('sha512')

  const buf = Buffer.alloc(1024)

  for (let i = 0; i < 10; i++) {
    crypto.randomFillSync(buf)

    if (Math.random() < 0.5) {
      hash1.update(buf)
      hash2.update(buf)
    } else {
      hash2.update(buf)
      hash1.update(buf)
    }
    refHash.update(buf)
  }

  const res = refHash.digest('hex')
  const res1 = hash1.digest('hex')
  const res2 = hash2.digest('hex')

  t.equal(res, res1, 'consistent with reference')
  t.equal(res1, res2, 'consistent with eachother')
  t.end()
})

tape('reported bugs', function (t) {
  const testBuf = Buffer.from('hello')

  const res = crypto.createHash('sha512').update(testBuf).digest('hex')
  const res1 = sha512().update(testBuf).digest('hex')
  const res2 = sha512().update(testBuf).digest('hex')
    
  t.equal(res, res1)
  t.equal(res1, res2)
  t.end()
})
