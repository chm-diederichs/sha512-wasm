const fs = require('fs')

const rollWordBE = `
(i64.load8_u (i32.add (i32.const 7) (get_local $ptr)))
(i64.shl (i64.load8_u (i32.add (i32.const 6) (get_local $ptr))) (i64.const 8))
(i64.or)
(i64.shl (i64.load8_u (i32.add (i32.const 5) (get_local $ptr))) (i64.const 16))
(i64.or)
(i64.shl (i64.load8_u (i32.add (i32.const 4) (get_local $ptr))) (i64.const 24))
(i64.or)
(i64.shl (i64.load8_u (i32.add (i32.const 3) (get_local $ptr))) (i64.const 32))
(i64.or)
(i64.shl (i64.load8_u (i32.add (i32.const 2) (get_local $ptr))) (i64.const 40))
(i64.or)
(i64.shl (i64.load8_u (i32.add (i32.const 1) (get_local $ptr))) (i64.const 48))
(i64.or)
(i64.shl (i64.load8_u (get_local $ptr)) (i64.const 56))
(i64.or)
`

function loadWordBE (wordIndex) {
  return `${rollWordBE}
(set_local $w${wordIndex})

(set_local $block_position (i32.add (get_local $block_position) (i32.const 8)))`
}

function makeBlock (depth) {
  if (depth === 0) return `(block $switch`

  return `(block $${16 - depth}
    ${makeBlock(depth - 1)}
(set_local $w${16 - depth} (get_local $last_word))
(br $break)`
//${loadWordBE(16 - depth)}
}

function loadWordsFromMemory () {
  for (let i = 16; i > 0; i--) {
    str.write(`(set_local $w${i-1} (i64.load offset=${64 + 8 * (i - 1)} (get_local $ctx)))\n`)
  }
}

const str = fs.createWriteStream('output.txt')

loadWordsFromMemory()
