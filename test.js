const sha512 = require('./')
const crypto = require('crypto')

const hash = sha512()
  .update('abc')
  .digest('hex')

const refHash = crypto.createHash('sha512')
  .update('abc')
  .digest('hex')

console.log(hash)
console.log(refHash)
