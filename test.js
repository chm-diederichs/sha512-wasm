const sha512 = require('./')
const crypto = require('crypto')
const sodium = require('sodium-native')
const ref = require('js-sha512').sha512


// console.time('sha512')
// for (let i = 0; i < 1000; i++) {
//   const hash = sha512()
//     .update('abcdefgh')
//     // .update('qwertyuio')
//     // .update('the quck brown')
//     // .update('!')
//     // .update("now let's see if you can handle an exceptionally e, hopefully one that fills the block size and then some... now wouldn't that be an interesting test case, i'm sure i'd like to know the result of that. Wouldn't you?")
//     // .update('the lazy dog @')
//     // .update('jumped over it, hoping ^')
//     // .update('!@`$%*&iii#')
//     // .update('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu')
//     .digest('hex')
// }
// console.timeEnd('sha512')

// console.time('js')
// for (let i = 0; i < 1000; i++) {
//   const hash = ref.create()
//     .update('abcdefgh')
//     // .update('qwertyuio')
//     // .update('the quck brown')
//     // .update('!')
//     // .update("now let's see if you can handle an exceptionally e, hopefully one that fills the block size and then some... now wouldn't that be an interesting test case, i'm sure i'd like to know the result of that. Wouldn't you?")
//     // .update('the lazy dog @')
//     // .update('jumped over it, hoping ^')
//     // .update('!@`$%*&iii#')
//     // .update('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu')
//     .digest('hex')
// }
// console.timeEnd('js')

// console.time('native')
// for (let i = 0; i < 1000; i++) {
//   const refHash = crypto.createHash('sha512')
//     .update('abcdefgh')
//     // .update('qwertyuio')
//     // .update('the quck brown')
//     // .update('!')
//     // .update("now let's see if you can handle an exceptionally e, hopefully one that fills the block size and then some... now wouldn't that be an interesting test case, i'm sure i'd like to know the result of that. Wouldn't you?")
//     // .update('the lazy dog @')
//     // .update('jumped over it, hoping ^')
//     // .update('!@`$%*&iii#')
//     // .update('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu')
//     .digest('hex')
// }
// console.timeEnd('native')

console.time('monolith')
for (let i = 0; i < 1000; i++) {
  const hash = sha512()
    .update('abc')
  //   .update('qwertyuio')
  //   .update('the quck brown')
  //   .update('!')
  //   .update("now let's see if you can handle an exceptionally e, hopefully one that fills the block size and then some... now wouldn't that be an interesting test case, i'm sure i'd like to know the result of that. Wouldn't you?")
  //   .update('the lazy dog @')
  //   .update('jumped over it, hoping ^')
  //   .update('!@`$%*&iii#')
  //   .update('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu')
    .digest('hex')
}
console.timeEnd('monolith')

// const output = Buffer.alloc(sodium.crypto_hash_sha512_BYTES)

// const sodiumHash = sodium.crypto_hash_sha512_instance()

// sodiumHash.update(Buffer.from('abcdefg'))
// sodiumHash.final(output)
console.time('ref')

for (let i = 0; i < 1000; i++) {
  const refHash = crypto.createHash('sha512')
    .update('abc')
  //   .update('qwertyuio')
  //   .update('the quck brown')
  //   .update('!')
  //   .update("now let's see if you can handle an exceptionally e, hopefully one that fills the block size and then some... now wouldn't that be an interesting test case, i'm sure i'd like to know the result of that. Wouldn't you?")
  //   .update('the lazy dog @')
  //   .update('jumped over it, hoping ^')
  //   .update('!@`$%*&iii#')
  //   .update('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu')
    .digest('hex')
}
console.timeEnd('ref')

    
// console.log(hash)
// console.log(refHash)
// console.log(output.toString('hex'))
