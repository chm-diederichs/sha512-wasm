(module
  (func $i32.log (import "debug" "log") (param i32))
  (func $i32.log_tee (import "debug" "log_tee") (param i32) (result i32))
  ;; No i64 interop with JS yet - but maybe coming with WebAssembly BigInt
  ;; So we can instead fake this by splitting the i64 into two i32 limbs,
  ;; however these are WASM functions using i32x2.log:
  (func $i32x2.log (import "debug" "log") (param i32) (param i32))
    ;; i64 logging by splitting into two i32 limbs
  (func $i64.log
    (param $0 i64)
    (call $i32x2.log
      ;; Upper limb
      (i32.wrap/i64
        (i64.shr_s (get_local $0)
          (i64.const 32)))
      ;; Lower limb
      (i32.wrap/i64 (get_local $0))))

  (func $i64.log_tee
    (param $0 i64)
    (result i64)
    (call $i64.log (get_local $0))
    (return (get_local $0)))


  (memory (export "memory") 0)

  ;; registers
  (global $a (mut i64) (i64.const 0))
  (global $b (mut i64) (i64.const 0))
  (global $c (mut i64) (i64.const 0))
  (global $d (mut i64) (i64.const 0))
  (global $e (mut i64) (i64.const 0))
  (global $f (mut i64) (i64.const 0))
  (global $g (mut i64) (i64.const 0))
  (global $h (mut i64) (i64.const 0))

  (global $k0  i64 (i64.const 0x428a2f98d728ae22))
  (global $k1  i64 (i64.const 0x7137449123ef65cd))
  (global $k2  i64 (i64.const 0xb5c0fbcfec4d3b2f))
  (global $k3  i64 (i64.const 0xe9b5dba58189dbbc))
  (global $k4  i64 (i64.const 0x3956c25bf348b538))
  (global $k5  i64 (i64.const 0x59f111f1b605d019))
  (global $k6  i64 (i64.const 0x923f82a4af194f9b))
  (global $k7  i64 (i64.const 0xab1c5ed5da6d8118))
  (global $k8  i64 (i64.const 0xd807aa98a3030242))
  (global $k9  i64 (i64.const 0x12835b0145706fbe))
  (global $k10 i64 (i64.const 0x243185be4ee4b28c))
  (global $k11 i64 (i64.const 0x550c7dc3d5ffb4e2))
  (global $k12 i64 (i64.const 0x72be5d74f27b896f))
  (global $k13 i64 (i64.const 0x80deb1fe3b1696b1))
  (global $k14 i64 (i64.const 0x9bdc06a725c71235))
  (global $k15 i64 (i64.const 0xc19bf174cf692694))
  (global $k16 i64 (i64.const 0xe49b69c19ef14ad2))
  (global $k17 i64 (i64.const 0xefbe4786384f25e3))
  (global $k18 i64 (i64.const 0x0fc19dc68b8cd5b5))
  (global $k19 i64 (i64.const 0x240ca1cc77ac9c65))
  (global $k20 i64 (i64.const 0x2de92c6f592b0275))
  (global $k21 i64 (i64.const 0x4a7484aa6ea6e483))
  (global $k22 i64 (i64.const 0x5cb0a9dcbd41fbd4))
  (global $k23 i64 (i64.const 0x76f988da831153b5))
  (global $k24 i64 (i64.const 0x983e5152ee66dfab))
  (global $k25 i64 (i64.const 0xa831c66d2db43210))
  (global $k26 i64 (i64.const 0xb00327c898fb213f))
  (global $k27 i64 (i64.const 0xbf597fc7beef0ee4))
  (global $k28 i64 (i64.const 0xc6e00bf33da88fc2))
  (global $k29 i64 (i64.const 0xd5a79147930aa725))
  (global $k30 i64 (i64.const 0x06ca6351e003826f))
  (global $k31 i64 (i64.const 0x142929670a0e6e70))
  (global $k32 i64 (i64.const 0x27b70a8546d22ffc))
  (global $k33 i64 (i64.const 0x2e1b21385c26c926))
  (global $k34 i64 (i64.const 0x4d2c6dfc5ac42aed))
  (global $k35 i64 (i64.const 0x53380d139d95b3df))
  (global $k36 i64 (i64.const 0x650a73548baf63de))
  (global $k37 i64 (i64.const 0x766a0abb3c77b2a8))
  (global $k38 i64 (i64.const 0x81c2c92e47edaee6))
  (global $k39 i64 (i64.const 0x92722c851482353b))
  (global $k40 i64 (i64.const 0xa2bfe8a14cf10364))
  (global $k41 i64 (i64.const 0xa81a664bbc423001))
  (global $k42 i64 (i64.const 0xc24b8b70d0f89791))
  (global $k43 i64 (i64.const 0xc76c51a30654be30))
  (global $k44 i64 (i64.const 0xd192e819d6ef5218))
  (global $k45 i64 (i64.const 0xd69906245565a910))
  (global $k46 i64 (i64.const 0xf40e35855771202a))
  (global $k47 i64 (i64.const 0x106aa07032bbd1b8))
  (global $k48 i64 (i64.const 0x19a4c116b8d2d0c8))
  (global $k49 i64 (i64.const 0x1e376c085141ab53))
  (global $k50 i64 (i64.const 0x2748774cdf8eeb99))
  (global $k51 i64 (i64.const 0x34b0bcb5e19b48a8))
  (global $k52 i64 (i64.const 0x391c0cb3c5c95a63))
  (global $k53 i64 (i64.const 0x4ed8aa4ae3418acb))
  (global $k54 i64 (i64.const 0x5b9cca4f7763e373))
  (global $k55 i64 (i64.const 0x682e6ff3d6b2b8a3))
  (global $k56 i64 (i64.const 0x748f82ee5defb2fc))
  (global $k57 i64 (i64.const 0x78a5636f43172f60))
  (global $k58 i64 (i64.const 0x84c87814a1f0ab72))
  (global $k59 i64 (i64.const 0x8cc702081a6439ec))
  (global $k60 i64 (i64.const 0x90befffa23631e28))
  (global $k61 i64 (i64.const 0xa4506cebde82bde9))
  (global $k62 i64 (i64.const 0xbef9a3f7b2c67915))
  (global $k63 i64 (i64.const 0xc67178f2e372532b))
  (global $k64 i64 (i64.const 0xca273eceea26619c))
  (global $k65 i64 (i64.const 0xd186b8c721c0c207))
  (global $k66 i64 (i64.const 0xeada7dd6cde0eb1e))
  (global $k67 i64 (i64.const 0xf57d4f7fee6ed178))
  (global $k68 i64 (i64.const 0x06f067aa72176fba))
  (global $k69 i64 (i64.const 0x0a637dc5a2c898a6))
  (global $k70 i64 (i64.const 0x113f9804bef90dae))
  (global $k71 i64 (i64.const 0x1b710b35131c471b))
  (global $k72 i64 (i64.const 0x28db77f523047d84))
  (global $k73 i64 (i64.const 0x32caab7b40c72493))
  (global $k74 i64 (i64.const 0x3c9ebe0a15c9bebc))
  (global $k75 i64 (i64.const 0x431d67c49c100d4c))
  (global $k76 i64 (i64.const 0x4cc5d4becb3e42b6))
  (global $k77 i64 (i64.const 0x597f299cfc657e2a))
  (global $k78 i64 (i64.const 0x5fcb6fab3ad6faec))
  (global $k79 i64 (i64.const 0x6c44198c4a475817))

  (func $i64.bswap
    (param $b i64)
    (result i64)

    ;; 1 set, 4 get, 8 const, 10 bitwise
        
    (i64.or
      (get_local $b)
      (i64.const 0xFFFF0000FFFF0000)
      (i64.and)
      (i64.rotl (i64.const 16))

      (get_local $b)
      (i64.const 0x0000FFFF0000FFFF)
      (i64.and)
      (i64.rotr (i64.const 16)))

    (set_local $b)

    (i64.or
      (get_local $b)
      (i64.const 0x00FF00FF00FF00FF)
      (i64.and)
      (i64.rotl (i64.const 8))

      (get_local $b)
      (i64.const 0xFF00FF00FF00FF00)
      (i64.and)
      (i64.rotr (i64.const 8))))

  (func $load_reverse_endian (param $ptr i32) (param $w i64)
    (result i64)

    (i64.load (get_local $ptr))
    (call $i64.bswap)
    (get_local $w)
    (i64.or))

  (func $load_last_bytes (param $ptr i32) (param $remaining i64)
    (result i64)

    (local $word i64)

    (i64.load (get_local $ptr))
    (set_local $word)

    (i64.const 0xffffffffffffffff)
    (i64.mul (get_local $remaining) (i64.const 8))
    (i64.shr_u)
    (i64.const -1)
    (i64.xor)
    (get_local $ptr)
    (i64.load)
    (i64.and))

  (func $round (param $w i64) (param $k i64)
    ;; precomputed values
    (local $T1 i64)
    (local $T2 i64)

    (local $ch_res i64)
    (local $maj_res i64)
    (local $big_sig0_res i64)
    (local $big_sig1_res i64)

    ;; precompute intermediate values

    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K0 + W0
    ;; T2 = big_sig0(a) + Maj(a, b, c)

    (set_local $ch_res (i64.xor (i64.and (get_global $e) (get_global $f)) (i64.and (i64.xor (get_global $e) (i64.const -1)) (get_global $g))))
    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_global $a) (get_global $b)) (i64.and (get_global $a) (get_global $c))) (i64.and (get_global $b) (get_global $c))))

    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_global $a) (i64.const 28)) (i64.rotr (get_global $a) (i64.const 34))) (i64.rotr (get_global $a) (i64.const 39))))
    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_global $e) (i64.const 14)) (i64.rotr (get_global $e) (i64.const 18))) (i64.rotr (get_global $e) (i64.const 41))))

    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_global $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w)) (get_local $k)))
    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

    ;; update registers

    ;; h <- g
    (set_global $h (get_global $g))

    ;; g <- f
    (set_global $g (get_global $f))  

    ;; f <- e
    (set_global $f (get_global $e))  

    ;; e <- d + T1
    (set_global $e (i64.add (get_global $d) (get_local $T1)))

    ;; d <- c
    (set_global $d (get_global $c))  

    ;; c <- b
    (set_global $c (get_global $b))  

    ;; b <- a
    (set_global $b (get_global $a))  

    ;; a <- T1 + T2
    (set_global $a (i64.add (get_local $T1) (get_local $T2))))

  (func $expand
    (param $a i64) (param $b i64) (param $c i64) (param $d i64)
    (result i64)

    (i64.add
      (i64.add
        (i64.add 
          (i64.xor
            (i64.xor
              (i64.rotr (get_local $a) (i64.const 19))
              (i64.rotr (get_local $a) (i64.const 61)))
            (i64.shr_u (get_local $a) (i64.const 6)))
          (get_local $b))
          (i64.xor
            (i64.xor
              (i64.rotr (get_local $c) (i64.const 1))
              (i64.rotr (get_local $c) (i64.const 8)))
            (i64.shr_u (get_local $c) (i64.const 7)))
        (get_local $d))))

  (func $compress
    (param $ctx i32)

    (param $w0 i64) (param $w1 i64) (param $w2 i64) (param $w3 i64)
    (param $w4 i64) (param $w5 i64) (param $w6 i64) (param $w7 i64)
    (param $w8 i64) (param $w9 i64) (param $w10 i64) (param $w11 i64)
    (param $w12 i64) (param $w13 i64) (param $w14 i64) (param $w15 i64)
    
    ;; precomputed values
    (local $T1 i64)
    (local $T2 i64)

    (local $ch_res i64)
    (local $maj_res i64)
    (local $big_sig0_res i64)
    (local $big_sig1_res i64)

    ;; expanded message schedule
    (local $w16 i64) (local $w17 i64) (local $w18 i64) (local $w19 i64) 
    (local $w20 i64) (local $w21 i64) (local $w22 i64) (local $w23 i64)
    (local $w24 i64) (local $w25 i64) (local $w26 i64) (local $w27 i64) 
    (local $w28 i64) (local $w29 i64) (local $w30 i64) (local $w31 i64)
    (local $w32 i64) (local $w33 i64) (local $w34 i64) (local $w35 i64) 
    (local $w36 i64) (local $w37 i64) (local $w38 i64) (local $w39 i64)
    (local $w40 i64) (local $w41 i64) (local $w42 i64) (local $w43 i64) 
    (local $w44 i64) (local $w45 i64) (local $w46 i64) (local $w47 i64)
    (local $w48 i64) (local $w49 i64) (local $w50 i64) (local $w51 i64) 
    (local $w52 i64) (local $w53 i64) (local $w54 i64) (local $w55 i64)
    (local $w56 i64) (local $w57 i64) (local $w58 i64) (local $w59 i64) 
    (local $w60 i64) (local $w61 i64) (local $w62 i64) (local $w63 i64)
    (local $w64 i64) (local $w65 i64) (local $w66 i64) (local $w67 i64)
    (local $w68 i64) (local $w69 i64) (local $w70 i64) (local $w71 i64) 
    (local $w72 i64) (local $w73 i64) (local $w74 i64) (local $w75 i64) 
    (local $w76 i64) (local $w77 i64) (local $w78 i64) (local $w79 i64)

    (set_local $w16 (call $expand (get_local $w14) (get_local $w9)  (get_local $w1) (get_local $w0)))
    (set_local $w17 (call $expand (get_local $w15) (get_local $w10) (get_local $w2)  (get_local $w1)))
    (set_local $w18 (call $expand (get_local $w16) (get_local $w11) (get_local $w3)  (get_local $w2)))
    (set_local $w19 (call $expand (get_local $w17) (get_local $w12) (get_local $w4)  (get_local $w3)))
    (set_local $w20 (call $expand (get_local $w18) (get_local $w13) (get_local $w5)  (get_local $w4)))
    (set_local $w21 (call $expand (get_local $w19) (get_local $w14) (get_local $w6)  (get_local $w5)))
    (set_local $w22 (call $expand (get_local $w20) (get_local $w15) (get_local $w7)  (get_local $w6)))
    (set_local $w23 (call $expand (get_local $w21) (get_local $w16) (get_local $w8)  (get_local $w7)))
    (set_local $w24 (call $expand (get_local $w22) (get_local $w17) (get_local $w9)  (get_local $w8)))
    (set_local $w25 (call $expand (get_local $w23) (get_local $w18) (get_local $w10) (get_local $w9)))
    (set_local $w26 (call $expand (get_local $w24) (get_local $w19) (get_local $w11) (get_local $w10)))
    (set_local $w27 (call $expand (get_local $w25) (get_local $w20) (get_local $w12) (get_local $w11)))
    (set_local $w28 (call $expand (get_local $w26) (get_local $w21) (get_local $w13) (get_local $w12)))
    (set_local $w29 (call $expand (get_local $w27) (get_local $w22) (get_local $w14) (get_local $w13)))
    (set_local $w30 (call $expand (get_local $w28) (get_local $w23) (get_local $w15) (get_local $w14)))
    (set_local $w31 (call $expand (get_local $w29) (get_local $w24) (get_local $w16) (get_local $w15)))
    (set_local $w32 (call $expand (get_local $w30) (get_local $w25) (get_local $w17) (get_local $w16)))
    (set_local $w33 (call $expand (get_local $w31) (get_local $w26) (get_local $w18) (get_local $w17)))
    (set_local $w34 (call $expand (get_local $w32) (get_local $w27) (get_local $w19) (get_local $w18)))
    (set_local $w35 (call $expand (get_local $w33) (get_local $w28) (get_local $w20) (get_local $w19)))
    (set_local $w36 (call $expand (get_local $w34) (get_local $w29) (get_local $w21) (get_local $w20)))
    (set_local $w37 (call $expand (get_local $w35) (get_local $w30) (get_local $w22) (get_local $w21)))
    (set_local $w38 (call $expand (get_local $w36) (get_local $w31) (get_local $w23) (get_local $w22)))
    (set_local $w39 (call $expand (get_local $w37) (get_local $w32) (get_local $w24) (get_local $w23)))
    (set_local $w40 (call $expand (get_local $w38) (get_local $w33) (get_local $w25) (get_local $w24)))
    (set_local $w41 (call $expand (get_local $w39) (get_local $w34) (get_local $w26) (get_local $w25)))
    (set_local $w42 (call $expand (get_local $w40) (get_local $w35) (get_local $w27) (get_local $w26)))
    (set_local $w43 (call $expand (get_local $w41) (get_local $w36) (get_local $w28) (get_local $w27)))
    (set_local $w44 (call $expand (get_local $w42) (get_local $w37) (get_local $w29) (get_local $w28)))
    (set_local $w45 (call $expand (get_local $w43) (get_local $w38) (get_local $w30) (get_local $w29)))
    (set_local $w46 (call $expand (get_local $w44) (get_local $w39) (get_local $w31) (get_local $w30)))
    (set_local $w47 (call $expand (get_local $w45) (get_local $w40) (get_local $w32) (get_local $w31)))
    (set_local $w48 (call $expand (get_local $w46) (get_local $w41) (get_local $w33) (get_local $w32)))
    (set_local $w49 (call $expand (get_local $w47) (get_local $w42) (get_local $w34) (get_local $w33)))
    (set_local $w50 (call $expand (get_local $w48) (get_local $w43) (get_local $w35) (get_local $w34)))
    (set_local $w51 (call $expand (get_local $w49) (get_local $w44) (get_local $w36) (get_local $w35)))
    (set_local $w52 (call $expand (get_local $w50) (get_local $w45) (get_local $w37) (get_local $w36)))
    (set_local $w53 (call $expand (get_local $w51) (get_local $w46) (get_local $w38) (get_local $w37)))
    (set_local $w54 (call $expand (get_local $w52) (get_local $w47) (get_local $w39) (get_local $w38)))
    (set_local $w55 (call $expand (get_local $w53) (get_local $w48) (get_local $w40) (get_local $w39)))
    (set_local $w56 (call $expand (get_local $w54) (get_local $w49) (get_local $w41) (get_local $w40)))
    (set_local $w57 (call $expand (get_local $w55) (get_local $w50) (get_local $w42) (get_local $w41)))
    (set_local $w58 (call $expand (get_local $w56) (get_local $w51) (get_local $w43) (get_local $w42)))
    (set_local $w59 (call $expand (get_local $w57) (get_local $w52) (get_local $w44) (get_local $w43)))
    (set_local $w60 (call $expand (get_local $w58) (get_local $w53) (get_local $w45) (get_local $w44)))
    (set_local $w61 (call $expand (get_local $w59) (get_local $w54) (get_local $w46) (get_local $w45)))
    (set_local $w62 (call $expand (get_local $w60) (get_local $w55) (get_local $w47) (get_local $w46)))
    (set_local $w63 (call $expand (get_local $w61) (get_local $w56) (get_local $w48) (get_local $w47)))
    (set_local $w64 (call $expand (get_local $w62) (get_local $w57) (get_local $w49) (get_local $w48)))
    (set_local $w65 (call $expand (get_local $w63) (get_local $w58) (get_local $w50) (get_local $w49)))
    (set_local $w66 (call $expand (get_local $w64) (get_local $w59) (get_local $w51) (get_local $w50)))
    (set_local $w67 (call $expand (get_local $w65) (get_local $w60) (get_local $w52) (get_local $w51)))
    (set_local $w68 (call $expand (get_local $w66) (get_local $w61) (get_local $w53) (get_local $w52)))
    (set_local $w69 (call $expand (get_local $w67) (get_local $w62) (get_local $w54) (get_local $w53)))
    (set_local $w70 (call $expand (get_local $w68) (get_local $w63) (get_local $w55) (get_local $w54)))
    (set_local $w71 (call $expand (get_local $w69) (get_local $w64) (get_local $w56) (get_local $w55)))
    (set_local $w72 (call $expand (get_local $w70) (get_local $w65) (get_local $w57) (get_local $w56)))
    (set_local $w73 (call $expand (get_local $w71) (get_local $w66) (get_local $w58) (get_local $w57)))
    (set_local $w74 (call $expand (get_local $w72) (get_local $w67) (get_local $w59) (get_local $w58)))
    (set_local $w75 (call $expand (get_local $w73) (get_local $w68) (get_local $w60) (get_local $w59)))
    (set_local $w76 (call $expand (get_local $w74) (get_local $w69) (get_local $w61) (get_local $w60)))
    (set_local $w77 (call $expand (get_local $w75) (get_local $w70) (get_local $w62) (get_local $w61)))
    (set_local $w78 (call $expand (get_local $w76) (get_local $w71) (get_local $w63) (get_local $w62)))
    (set_local $w79 (call $expand (get_local $w77) (get_local $w72) (get_local $w64) (get_local $w63)))

                    ;;  store inital state
    (if (i64.le_u (i64.load offset=192 (get_local $ctx)) (i64.const 128))
        (then
            (i64.store offset=0  (get_local $ctx) (i64.const 0x6a09e667f3bcc908))
            (i64.store offset=8  (get_local $ctx) (i64.const 0xbb67ae8584caa73b))
            (i64.store offset=16 (get_local $ctx) (i64.const 0x3c6ef372fe94f82b))
            (i64.store offset=24 (get_local $ctx) (i64.const 0xa54ff53a5f1d36f1))
            (i64.store offset=32 (get_local $ctx) (i64.const 0x510e527fade682d1))
            (i64.store offset=40 (get_local $ctx) (i64.const 0x9b05688c2b3e6c1f))
            (i64.store offset=48 (get_local $ctx) (i64.const 0x1f83d9abfb41bd6b))
            (i64.store offset=56 (get_local $ctx) (i64.const 0x5be0cd19137e2179))))

    ;; load previous hash state into registers
    (set_global $a (i64.load offset=0 (get_local $ctx)))
    (set_global $b (i64.load offset=8 (get_local $ctx)))
    (set_global $c (i64.load offset=16 (get_local $ctx)))
    (set_global $d (i64.load offset=24 (get_local $ctx)))
    (set_global $e (i64.load offset=32 (get_local $ctx)))
    (set_global $f (i64.load offset=40 (get_local $ctx)))
    (set_global $g (i64.load offset=48 (get_local $ctx)))
    (set_global $h (i64.load offset=56 (get_local $ctx)))

    (call $round (get_local $w1) (get_global $k1))
    (call $round (get_local $w2) (get_global $k2))
    (call $round (get_local $w3) (get_global $k3))
    (call $round (get_local $w4) (get_global $k4))
    (call $round (get_local $w5) (get_global $k5))
    (call $round (get_local $w6) (get_global $k6))
    (call $round (get_local $w7) (get_global $k7))
    (call $round (get_local $w8) (get_global $k8))
    (call $round (get_local $w9) (get_global $k9))
    (call $round (get_local $w10) (get_global $k10))
    (call $round (get_local $w11) (get_global $k11))
    (call $round (get_local $w12) (get_global $k12))
    (call $round (get_local $w13) (get_global $k13))
    (call $round (get_local $w14) (get_global $k14))
    (call $round (get_local $w15) (get_global $k15))
    (call $round (get_local $w16) (get_global $k16))
    (call $round (get_local $w17) (get_global $k17))
    (call $round (get_local $w18) (get_global $k18))
    (call $round (get_local $w19) (get_global $k19))
    (call $round (get_local $w20) (get_global $k20))
    (call $round (get_local $w21) (get_global $k21))
    (call $round (get_local $w22) (get_global $k22))
    (call $round (get_local $w23) (get_global $k23))
    (call $round (get_local $w24) (get_global $k24))
    (call $round (get_local $w25) (get_global $k25))
    (call $round (get_local $w26) (get_global $k26))
    (call $round (get_local $w27) (get_global $k27))
    (call $round (get_local $w28) (get_global $k28))
    (call $round (get_local $w29) (get_global $k29))
    (call $round (get_local $w30) (get_global $k30))
    (call $round (get_local $w31) (get_global $k31))
    (call $round (get_local $w32) (get_global $k32))
    (call $round (get_local $w33) (get_global $k33))
    (call $round (get_local $w34) (get_global $k34))
    (call $round (get_local $w35) (get_global $k35))
    (call $round (get_local $w36) (get_global $k36))
    (call $round (get_local $w37) (get_global $k37))
    (call $round (get_local $w38) (get_global $k38))
    (call $round (get_local $w39) (get_global $k39))
    (call $round (get_local $w40) (get_global $k40))
    (call $round (get_local $w41) (get_global $k41))
    (call $round (get_local $w42) (get_global $k42))
    (call $round (get_local $w43) (get_global $k43))
    (call $round (get_local $w44) (get_global $k44))
    (call $round (get_local $w45) (get_global $k45))
    (call $round (get_local $w46) (get_global $k46))
    (call $round (get_local $w47) (get_global $k47))
    (call $round (get_local $w48) (get_global $k48))
    (call $round (get_local $w49) (get_global $k49))
    (call $round (get_local $w50) (get_global $k50))
    (call $round (get_local $w51) (get_global $k51))
    (call $round (get_local $w52) (get_global $k52))
    (call $round (get_local $w53) (get_global $k53))
    (call $round (get_local $w54) (get_global $k54))
    (call $round (get_local $w55) (get_global $k55))
    (call $round (get_local $w56) (get_global $k56))
    (call $round (get_local $w57) (get_global $k57))
    (call $round (get_local $w58) (get_global $k58))
    (call $round (get_local $w59) (get_global $k59))
    (call $round (get_local $w60) (get_global $k60))
    (call $round (get_local $w61) (get_global $k61))
    (call $round (get_local $w62) (get_global $k62))
    (call $round (get_local $w63) (get_global $k63))
    (call $round (get_local $w64) (get_global $k64))
    (call $round (get_local $w65) (get_global $k65))
    (call $round (get_local $w66) (get_global $k66))
    (call $round (get_local $w67) (get_global $k67))
    (call $round (get_local $w68) (get_global $k68))
    (call $round (get_local $w69) (get_global $k69))
    (call $round (get_local $w70) (get_global $k70))
    (call $round (get_local $w71) (get_global $k71))
    (call $round (get_local $w72) (get_global $k72))
    (call $round (get_local $w73) (get_global $k73))
    (call $round (get_local $w74) (get_global $k74))
    (call $round (get_local $w75) (get_global $k75))
    (call $round (get_local $w76) (get_global $k76))
    (call $round (get_local $w77) (get_global $k77))
    (call $round (get_local $w78) (get_global $k78))
    (call $round (get_local $w79) (get_global $k79))

    ;; store hash values
    (i64.store offset=0  (get_local $ctx) (i64.add (i64.load offset=0  (get_local $ctx)) (get_global $a)))
    (i64.store offset=8  (get_local $ctx) (i64.add (i64.load offset=8  (get_local $ctx)) (get_global $b)))
    (i64.store offset=16 (get_local $ctx) (i64.add (i64.load offset=16 (get_local $ctx)) (get_global $c)))
    (i64.store offset=24 (get_local $ctx) (i64.add (i64.load offset=24 (get_local $ctx)) (get_global $d)))
    (i64.store offset=32 (get_local $ctx) (i64.add (i64.load offset=32 (get_local $ctx)) (get_global $e)))
    (i64.store offset=40 (get_local $ctx) (i64.add (i64.load offset=40 (get_local $ctx)) (get_global $f)))
    (i64.store offset=48 (get_local $ctx) (i64.add (i64.load offset=48 (get_local $ctx)) (get_global $g)))
    (i64.store offset=56 (get_local $ctx) (i64.add (i64.load offset=56 (get_local $ctx)) (get_global $h)))

    ;; zero out words
    (set_local $w0  (i64.const 0))
    (set_local $w1  (i64.const 0))
    (set_local $w2  (i64.const 0))
    (set_local $w3  (i64.const 0))
    (set_local $w4  (i64.const 0))
    (set_local $w5  (i64.const 0))
    (set_local $w6  (i64.const 0))
    (set_local $w7  (i64.const 0))
    (set_local $w8  (i64.const 0))
    (set_local $w9  (i64.const 0))
    (set_local $w10 (i64.const 0))
    (set_local $w11 (i64.const 0))
    (set_local $w12 (i64.const 0))
    (set_local $w13 (i64.const 0))
    (set_local $w14 (i64.const 0))
    (set_local $w15 (i64.const 0)))

  (func $sha512 (export "sha512") (param $ctx i32) (param $roi i32) (param $length i32) (param $final i32)
    ;; (result i32)

    ;;    schema  208 bytes
    ;;     0..64  hash state
    ;;    64..80  number of bytes read across all updates (128bit)
    ;;   80..208  store words between updates

    (local $i i32)
    (local $ptr i32)
    (local $bytes_read i64)
    (local $bytes_read_overflow i64)
    (local $check_overflow i64)
    (local $last_word i64)
    (local $remaining i32)
    (local $tail i64)

    ;; expanded message schedule
    (local $w0 i64)  (local $w1 i64)  (local $w2 i64)  (local $w3 i64)  
    (local $w4 i64)  (local $w5 i64)  (local $w6 i64)  (local $w7 i64) 
    (local $w8 i64)  (local $w9 i64)  (local $w10 i64) (local $w11 i64) 
    (local $w12 i64) (local $w13 i64) (local $w14 i64) (local $w15 i64)
    (local $w16 i64) (local $w17 i64) (local $w18 i64) (local $w19 i64)

    ;; load current block position
    (set_local $bytes_read (i64.load offset=64 (get_local $ctx)))
    (set_local $bytes_read_overflow (i64.load offset=72 (get_local $ctx)))
    (set_local $ptr (i32.add (get_local $ctx) (i32.const 80)))
    (set_local $remaining (get_local $length))

    (block $finish
      (set_local $w0  (i64.load offset=0   (get_local $ptr)))
      (set_local $w1  (i64.load offset=8   (get_local $ptr)))
      (set_local $w2  (i64.load offset=16  (get_local $ptr)))
      (set_local $w3  (i64.load offset=24  (get_local $ptr)))
      (set_local $w4  (i64.load offset=32  (get_local $ptr)))
      (set_local $w5  (i64.load offset=40  (get_local $ptr)))
      (set_local $w6  (i64.load offset=48  (get_local $ptr)))
      (set_local $w7  (i64.load offset=56  (get_local $ptr)))
      (set_local $w8  (i64.load offset=64  (get_local $ptr)))
      (set_local $w9  (i64.load offset=72  (get_local $ptr)))
      (set_local $w10 (i64.load offset=80  (get_local $ptr)))
      (set_local $w11 (i64.load offset=88  (get_local $ptr)))
      (set_local $w12 (i64.load offset=96  (get_local $ptr)))
      (set_local $w13 (i64.load offset=104 (get_local $ptr)))
      (set_local $w14 (i64.load offset=112 (get_local $ptr)))
      (set_local $w15 (i64.load offset=120 (get_local $ptr)))

      (tee_local $remaining (i32.sub (get_local $remaining) (i32.const 64)))
      (i32.const 0)
      (i32.le_s)
      (br_if $finish)

      (get_local $ctx)
      (get_local $w0 )
      (get_local $w1 )
      (get_local $w2 )
      (get_local $w3 )
      (get_local $w4 )
      (get_local $w5 )
      (get_local $w6 )
      (get_local $w7 )
      (get_local $w8 )
      (get_local $w9 )
      (get_local $w10)
      (get_local $w11)
      (get_local $w12)
      (get_local $w13)
      (get_local $w14)
      (get_local $w15)
      (call $compress)

      (loop $rest_of_input
        (set_local $w0  (i64.load offset=0   (get_local $roi)))
        (set_local $w1  (i64.load offset=8   (get_local $roi)))
        (set_local $w2  (i64.load offset=16  (get_local $roi)))
        (set_local $w3  (i64.load offset=24  (get_local $roi)))
        (set_local $w4  (i64.load offset=32  (get_local $roi)))
        (set_local $w5  (i64.load offset=40  (get_local $roi)))
        (set_local $w6  (i64.load offset=48  (get_local $roi)))
        (set_local $w7  (i64.load offset=56  (get_local $roi)))
        (set_local $w8  (i64.load offset=64  (get_local $roi)))
        (set_local $w9  (i64.load offset=72  (get_local $roi)))
        (set_local $w10 (i64.load offset=80  (get_local $roi)))
        (set_local $w11 (i64.load offset=88  (get_local $roi)))
        (set_local $w12 (i64.load offset=96  (get_local $roi)))
        (set_local $w13 (i64.load offset=104 (get_local $roi)))
        (set_local $w14 (i64.load offset=112 (get_local $roi)))
        (set_local $w15 (i64.load offset=120 (get_local $roi)))

        (tee_local $remaining (i32.sub (get_local $remaining) (i32.const 64)))
        (i32.const 0)
        (i32.le_s)
        (if 
          (then
            (i64.store offset=80  (get_local $ctx) (get_local $w0))
            (i64.store offset=88  (get_local $ctx) (get_local $w1))
            (i64.store offset=96  (get_local $ctx) (get_local $w2))
            (i64.store offset=104 (get_local $ctx) (get_local $w3))
            (i64.store offset=112 (get_local $ctx) (get_local $w4))
            (i64.store offset=120 (get_local $ctx) (get_local $w5))
            (i64.store offset=128 (get_local $ctx) (get_local $w6))
            (i64.store offset=136 (get_local $ctx) (get_local $w7))
            (i64.store offset=144 (get_local $ctx) (get_local $w8))
            (i64.store offset=152 (get_local $ctx) (get_local $w9))
            (i64.store offset=160 (get_local $ctx) (get_local $w10))
            (i64.store offset=168 (get_local $ctx) (get_local $w11))
            (i64.store offset=176 (get_local $ctx) (get_local $w12))
            (i64.store offset=184 (get_local $ctx) (get_local $w13))
            (i64.store offset=192 (get_local $ctx) (get_local $w14))
            (i64.store offset=200 (get_local $ctx) (get_local $w15))
            (br $finish)))

        (get_local $ctx)
        (get_local $w0 )
        (get_local $w1 )
        (get_local $w2 )
        (get_local $w3 )
        (get_local $w4 )
        (get_local $w5 )
        (get_local $w6 )
        (get_local $w7 )
        (get_local $w8 )
        (get_local $w9 )
        (get_local $w10)
        (get_local $w11)
        (get_local $w12)
        (get_local $w13)
        (get_local $w14)
        (get_local $w15)
        (call $compress)
        (br $rest_of_input)))

    (set_local $check_overflow (get_local $bytes_read))
    (set_local $bytes_read (i64.add (get_local $bytes_read) (i64.extend_u/i32 (get_local $length))))

    ;; carry n > 64 bits for i128 length
    (if (i64.lt_u (get_local $bytes_read) (get_local $check_overflow))
        (then
            (set_local $check_overflow (get_local $bytes_read))
            (set_local $bytes_read_overflow (i64.add (get_local $bytes_read_overflow) (i64.const 1)))))

    (i64.store offset=64 (get_local $ctx) (get_local $bytes_read))
    (i64.store offset=72 (get_local $ctx) (get_local $bytes_read_overflow))

    (if (i32.eq (get_local $final) (i32.const 1))
      (then
        (set_local $tail (i64.and (get_local $bytes_read) (i64.const 0x3f)))
        (set_local $last_word (i64.shl (i64.const 0x80) (i64.shl (i64.and (get_local $tail) (i64.const 0x7)) (i64.const 3))))

        (block $pad_end
                (block $832
                    (block $768
                        (block $704
                            (block $640
                                (block $576
                                    (block $512
                                        (block $448
                                            (block $384
                                                (block $320
                                                    (block $256
                                                        (block $192
                                                            (block $128
                                                                (block $64
                                                                    (block $0
                                                                        (block $960
                                                                            (block $896
                                                                                (block $switch
                                                                                    (i32.wrap/i64 (get_local $tail))
                                                                                    (i32.const -16)
                                                                                    (i32.and)
                                                                                    (br_table $0 $64 $128 $192 $256 $320 $384 $448 $512 $576 $640 $704 $768 $832 $896 $960)))
                                                                                
                                                                                (get_local $last_word)
                                                                                (get_local $w14)
                                                                                (i64.or)
                                                                                (set_local $w14)
                                                                                (set_local $last_word (i64.const 0)))
                                                                            
                                                                            (get_local $last_word)
                                                                            (get_local $w15)
                                                                            (i64.or)
                                                                            (set_local $w15)
                                                                            (set_local $last_word (i64.const 0))

                                                                            ;; compress
                                                                            (get_local $ctx)
                                                                            (get_local $w0)
                                                                            (get_local $w1)
                                                                            (get_local $w2)
                                                                            (get_local $w3)
                                                                            (get_local $w4)
                                                                            (get_local $w5)
                                                                            (get_local $w6)
                                                                            (get_local $w7)
                                                                            (get_local $w8)
                                                                            (get_local $w9)
                                                                            (get_local $w10)
                                                                            (get_local $w11)
                                                                            (get_local $w12)
                                                                            (get_local $w13)
                                                                            (get_local $w14)
                                                                            (get_local $w15)
                                                                            (call $compress))
                                                                        
                                                                        (get_local $last_word)
                                                                        (get_local $w0)
                                                                        (i64.or)
                                                                        (set_local $w0)
                                                                        (set_local $last_word (i64.const 0)))
                                                                    
                                                                    (get_local $last_word)
                                                                    (get_local $w1)
                                                                    (i64.or)
                                                                    (set_local $w1)
                                                                    (set_local $last_word (i64.const 0)))
                                                                
                                                                (get_local $last_word)
                                                                (get_local $w2)
                                                                (i64.or)
                                                                (set_local $w2)
                                                                (set_local $last_word (i64.const 0)))
                                                            
                                                            (get_local $last_word)
                                                            (get_local $w3)
                                                            (i64.or)
                                                            (set_local $w3)
                                                            (set_local $last_word (i64.const 0)))
                                                        
                                                        (get_local $last_word)
                                                        (get_local $w4)
                                                        (i64.or)
                                                        (set_local $w4)
                                                        (set_local $last_word (i64.const 0)))
                                                    
                                                    (get_local $last_word)
                                                    (get_local $w5)
                                                    (i64.or)
                                                    (set_local $w5)
                                                    (set_local $last_word (i64.const 0)))
                                                
                                                (get_local $last_word)
                                                (get_local $w6)
                                                (i64.or)
                                                (set_local $w6)
                                                (set_local $last_word (i64.const 0)))
                                            
                                            (get_local $last_word)
                                            (get_local $w7)
                                            (i64.or)
                                            (set_local $w7)
                                            (set_local $last_word (i64.const 0)))
                                        
                                        (get_local $last_word)
                                        (get_local $w8)
                                        (i64.or)
                                        (set_local $w8)
                                        (set_local $last_word (i64.const 0)))
                                    
                                    (get_local $last_word)
                                    (get_local $w9)
                                    (i64.or)
                                    (set_local $w9)
                                    (set_local $last_word (i64.const 0)))
                                
                                (get_local $last_word)
                                (get_local $w10)
                                (i64.or)
                                (set_local $w10)
                                (set_local $last_word (i64.const 0)))
                            
                            (get_local $last_word)
                            (get_local $w11)
                            (i64.or)
                            (set_local $w11)
                            (set_local $last_word (i64.const 0)))
                        
                        (get_local $last_word)
                        (get_local $w12)
                        (i64.or)
                        (set_local $w12)
                        (set_local $last_word (i64.const 0)))
                    
                    (get_local $last_word)
                    (get_local $w13)
                    (i64.or)
                    (set_local $w13)
                    (set_local $last_word (i64.const 0)))
            
            ;; load upper limb of 128bit length
            (get_local $bytes_read)
            (i64.const 61)
            (i64.shr_u)
            (get_local $bytes_read_overflow)
            (i64.const 3)
            (i64.shr_u)
            (i64.add)
            (set_local $w14)

            (set_local $w15 (i64.mul (get_local $bytes_read) (i64.const 8)))

            (call $i64.log (i64.sub (get_local $w0) (i64.const 1)))
            (call $i64.log (get_local $w0))
            (call $i64.log (get_local $w1))
            (call $i64.log (get_local $w2))
            (call $i64.log (get_local $w3))
            (call $i64.log (get_local $w4))
            (call $i64.log (get_local $w5))
            (call $i64.log (get_local $w6))
            (call $i64.log (get_local $w7))
            (call $i64.log (get_local $w8))
            (call $i64.log (get_local $w9))
            (call $i64.log (get_local $w10))
            (call $i64.log (get_local $w11))
            (call $i64.log (get_local $w12))
            (call $i64.log (get_local $w13))
            (call $i64.log (get_local $w14))
            (call $i64.log (get_local $w15))
            (set_local $w15 (i64.const 0x1800000000000000))

            (get_local $ctx)
            (get_local $w0)
            (get_local $w1)
            (get_local $w2)
            (get_local $w3)
            (get_local $w4)
            (get_local $w5)
            (get_local $w6)
            (get_local $w7)
            (get_local $w8)
            (get_local $w9)
            (get_local $w10)
            (get_local $w11)
            (get_local $w12)
            (get_local $w13)
            (get_local $w14)
            (get_local $w15)
            (call $compress)

            (get_local $ctx)
            (call $i64.bswap (get_global $a))
            (i64.store offset=0)

            (get_local $ctx)
            (call $i64.bswap (get_global $b))
            (i64.store offset=8)

            (get_local $ctx)
            (call $i64.bswap (get_global $c))
            (i64.store offset=16)

            (get_local $ctx)
            (call $i64.bswap (get_global $d))
            (i64.store offset=24)

            (get_local $ctx)
            (call $i64.bswap (get_global $e))
            (i64.store offset=32)

            (get_local $ctx)
            (call $i64.bswap (get_global $f))
            (i64.store offset=40)

            (get_local $ctx)
            (call $i64.bswap (get_global $g))
            (i64.store offset=48)

            (get_local $ctx)
            (call $i64.bswap (get_global $h))
            (i64.store offset=56)))))

    ;; HASH COMPLETE FOR MESSAGE BLOCK
