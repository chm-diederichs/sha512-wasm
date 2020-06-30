(module
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
    ;; g <- f
    ;; f <- e
    ;; e <- d + T1
    ;; d <- c
    ;; c <- b
    ;; b <- a
    ;; a <- T1 + T2

    (set_global $h (get_global $g))
    (set_global $g (get_global $f))  
    (set_global $f (get_global $e))  
    (set_global $e (i64.add (get_global $d) (get_local $T1)))
    (set_global $d (get_global $c))  
    (set_global $c (get_global $b))  
    (set_global $b (get_global $a))  
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

    (local $tmp0 i64)
    (local $tmp1 i64)
    (local $tmp2 i64)
    (local $tmp3 i64)
    (local $tmp4 i64)
    (local $tmp5 i64)
    (local $tmp6 i64)
    (local $tmp7 i64)
    (local $tmp8 i64)
    (local $tmp9 i64)
    (local $tmp10 i64)
    (local $tmp11 i64)
    (local $tmp12 i64)
    (local $tmp13 i64)
    (local $tmp14 i64)
    (local $tmp15 i64)


    ;;  store inital state
    (if (i64.eq (i64.load offset=208 (get_local $ctx)) (i64.const 0))
        (then
            (i64.store offset=0  (get_local $ctx) (i64.const 0x6a09e667f3bcc908))
            (i64.store offset=8  (get_local $ctx) (i64.const 0xbb67ae8584caa73b))
            (i64.store offset=16 (get_local $ctx) (i64.const 0x3c6ef372fe94f82b))
            (i64.store offset=24 (get_local $ctx) (i64.const 0xa54ff53a5f1d36f1))
            (i64.store offset=32 (get_local $ctx) (i64.const 0x510e527fade682d1))
            (i64.store offset=40 (get_local $ctx) (i64.const 0x9b05688c2b3e6c1f))
            (i64.store offset=48 (get_local $ctx) (i64.const 0x1f83d9abfb41bd6b))
            (i64.store offset=56 (get_local $ctx) (i64.const 0x5be0cd19137e2179))
            (i64.store offset=208 (get_local $ctx) (i64.const 1))))

    ;; load previous hash state into registers
    (set_global $a (i64.load offset=0 (get_local $ctx)))
    (set_global $b (i64.load offset=8 (get_local $ctx)))
    (set_global $c (i64.load offset=16 (get_local $ctx)))
    (set_global $d (i64.load offset=24 (get_local $ctx)))
    (set_global $e (i64.load offset=32 (get_local $ctx)))
    (set_global $f (i64.load offset=40 (get_local $ctx)))
    (set_global $g (i64.load offset=48 (get_local $ctx)))
    (set_global $h (i64.load offset=56 (get_local $ctx)))

    (set_local $w0  (call $i64.bswap (get_local $w0 )))
    (set_local $w1  (call $i64.bswap (get_local $w1 )))
    (set_local $w2  (call $i64.bswap (get_local $w2 )))
    (set_local $w3  (call $i64.bswap (get_local $w3 )))
    (set_local $w4  (call $i64.bswap (get_local $w4 )))
    (set_local $w5  (call $i64.bswap (get_local $w5 )))
    (set_local $w6  (call $i64.bswap (get_local $w6 )))
    (set_local $w7  (call $i64.bswap (get_local $w7 )))
    (set_local $w8  (call $i64.bswap (get_local $w8 )))
    (set_local $w9  (call $i64.bswap (get_local $w9 )))
    (set_local $w10 (call $i64.bswap (get_local $w10)))
    (set_local $w11 (call $i64.bswap (get_local $w11)))
    (set_local $w12 (call $i64.bswap (get_local $w12)))
    (set_local $w13 (call $i64.bswap (get_local $w13)))
    (set_local $w14 (call $i64.bswap (get_local $w14)))
    (set_local $w15 (call $i64.bswap (get_local $w15)))

    (set_local $w16 (call $expand (get_local $w14) (get_local $w9)  (get_local $w1)  (get_local $w0)))
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
    
    (call $round (get_local $w0)(i64.const 0x428a2f98d728ae22))
    (call $round (get_local $w1)(i64.const 0x7137449123ef65cd))
    (call $round (get_local $w2)(i64.const 0xb5c0fbcfec4d3b2f))
    (call $round (get_local $w3)(i64.const 0xe9b5dba58189dbbc))
    (call $round (get_local $w4)(i64.const 0x3956c25bf348b538))
    (call $round (get_local $w5)(i64.const 0x59f111f1b605d019))
    (call $round (get_local $w6)(i64.const 0x923f82a4af194f9b))
    (call $round (get_local $w7)(i64.const 0xab1c5ed5da6d8118))
    (call $round (get_local $w8)(i64.const 0xd807aa98a3030242))
    (call $round (get_local $w9)(i64.const 0x12835b0145706fbe))
    (call $round (get_local $w10) (i64.const 0x243185be4ee4b28c))
    (call $round (get_local $w11) (i64.const 0x550c7dc3d5ffb4e2))
    (call $round (get_local $w12) (i64.const 0x72be5d74f27b896f))
    (call $round (get_local $w13) (i64.const 0x80deb1fe3b1696b1))
    (call $round (get_local $w14) (i64.const 0x9bdc06a725c71235))
    (call $round (get_local $w15) (i64.const 0xc19bf174cf692694))
    (call $round (get_local $w16) (i64.const 0xe49b69c19ef14ad2))
    (call $round (get_local $w17) (i64.const 0xefbe4786384f25e3))
    (call $round (get_local $w18) (i64.const 0x0fc19dc68b8cd5b5))
    (call $round (get_local $w19) (i64.const 0x240ca1cc77ac9c65))
    (call $round (get_local $w20) (i64.const 0x2de92c6f592b0275))
    (call $round (get_local $w21) (i64.const 0x4a7484aa6ea6e483))
    (call $round (get_local $w22) (i64.const 0x5cb0a9dcbd41fbd4))
    (call $round (get_local $w23) (i64.const 0x76f988da831153b5))
    (call $round (get_local $w24) (i64.const 0x983e5152ee66dfab))
    (call $round (get_local $w25) (i64.const 0xa831c66d2db43210))
    (call $round (get_local $w26) (i64.const 0xb00327c898fb213f))
    (call $round (get_local $w27) (i64.const 0xbf597fc7beef0ee4))
    (call $round (get_local $w28) (i64.const 0xc6e00bf33da88fc2))
    (call $round (get_local $w29) (i64.const 0xd5a79147930aa725))
    (call $round (get_local $w30) (i64.const 0x06ca6351e003826f))
    (call $round (get_local $w31) (i64.const 0x142929670a0e6e70))
    (call $round (get_local $w32) (i64.const 0x27b70a8546d22ffc))
    (call $round (get_local $w33) (i64.const 0x2e1b21385c26c926))
    (call $round (get_local $w34) (i64.const 0x4d2c6dfc5ac42aed))
    (call $round (get_local $w35) (i64.const 0x53380d139d95b3df))
    (call $round (get_local $w36) (i64.const 0x650a73548baf63de))
    (call $round (get_local $w37) (i64.const 0x766a0abb3c77b2a8))
    (call $round (get_local $w38) (i64.const 0x81c2c92e47edaee6))
    (call $round (get_local $w39) (i64.const 0x92722c851482353b))
    (call $round (get_local $w40) (i64.const 0xa2bfe8a14cf10364))
    (call $round (get_local $w41) (i64.const 0xa81a664bbc423001))
    (call $round (get_local $w42) (i64.const 0xc24b8b70d0f89791))
    (call $round (get_local $w43) (i64.const 0xc76c51a30654be30))
    (call $round (get_local $w44) (i64.const 0xd192e819d6ef5218))
    (call $round (get_local $w45) (i64.const 0xd69906245565a910))
    (call $round (get_local $w46) (i64.const 0xf40e35855771202a))
    (call $round (get_local $w47) (i64.const 0x106aa07032bbd1b8))
    (call $round (get_local $w48) (i64.const 0x19a4c116b8d2d0c8))
    (call $round (get_local $w49) (i64.const 0x1e376c085141ab53))
    (call $round (get_local $w50) (i64.const 0x2748774cdf8eeb99))
    (call $round (get_local $w51) (i64.const 0x34b0bcb5e19b48a8))
    (call $round (get_local $w52) (i64.const 0x391c0cb3c5c95a63))
    (call $round (get_local $w53) (i64.const 0x4ed8aa4ae3418acb))
    (call $round (get_local $w54) (i64.const 0x5b9cca4f7763e373))
    (call $round (get_local $w55) (i64.const 0x682e6ff3d6b2b8a3))
    (call $round (get_local $w56) (i64.const 0x748f82ee5defb2fc))
    (call $round (get_local $w57) (i64.const 0x78a5636f43172f60))
    (call $round (get_local $w58) (i64.const 0x84c87814a1f0ab72))
    (call $round (get_local $w59) (i64.const 0x8cc702081a6439ec))
    (call $round (get_local $w60) (i64.const 0x90befffa23631e28))
    (call $round (get_local $w61) (i64.const 0xa4506cebde82bde9))
    (call $round (get_local $w62) (i64.const 0xbef9a3f7b2c67915))
    (call $round (get_local $w63) (i64.const 0xc67178f2e372532b))
    (call $round (get_local $w64) (i64.const 0xca273eceea26619c))
    (call $round (get_local $w65) (i64.const 0xd186b8c721c0c207))
    (call $round (get_local $w66) (i64.const 0xeada7dd6cde0eb1e))
    (call $round (get_local $w67) (i64.const 0xf57d4f7fee6ed178))
    (call $round (get_local $w68) (i64.const 0x06f067aa72176fba))
    (call $round (get_local $w69) (i64.const 0x0a637dc5a2c898a6))
    (call $round (get_local $w70) (i64.const 0x113f9804bef90dae))
    (call $round (get_local $w71) (i64.const 0x1b710b35131c471b))
    (call $round (get_local $w72) (i64.const 0x28db77f523047d84))
    (call $round (get_local $w73) (i64.const 0x32caab7b40c72493))
    (call $round (get_local $w74) (i64.const 0x3c9ebe0a15c9bebc))
    (call $round (get_local $w75) (i64.const 0x431d67c49c100d4c))
    (call $round (get_local $w76) (i64.const 0x4cc5d4becb3e42b6))
    (call $round (get_local $w77) (i64.const 0x597f299cfc657e2a))
    (call $round (get_local $w78) (i64.const 0x5fcb6fab3ad6faec))
    (call $round (get_local $w79) (i64.const 0x6c44198c4a475817))

    ;; store hash values
    (i64.store offset=0  (get_local $ctx) (i64.add (i64.load offset=0  (get_local $ctx)) (get_global $a)))
    (i64.store offset=8  (get_local $ctx) (i64.add (i64.load offset=8  (get_local $ctx)) (get_global $b)))
    (i64.store offset=16 (get_local $ctx) (i64.add (i64.load offset=16 (get_local $ctx)) (get_global $c)))
    (i64.store offset=24 (get_local $ctx) (i64.add (i64.load offset=24 (get_local $ctx)) (get_global $d)))
    (i64.store offset=32 (get_local $ctx) (i64.add (i64.load offset=32 (get_local $ctx)) (get_global $e)))
    (i64.store offset=40 (get_local $ctx) (i64.add (i64.load offset=40 (get_local $ctx)) (get_global $f)))
    (i64.store offset=48 (get_local $ctx) (i64.add (i64.load offset=48 (get_local $ctx)) (get_global $g)))
    (i64.store offset=56 (get_local $ctx) (i64.add (i64.load offset=56 (get_local $ctx)) (get_global $h))))

  (func $sha512 (export "sha512") (param $ctx i32) (param $roi i32) (param $length i32) (param $final i32)
    ;; (result i32)

    ;;    schema  208 bytes
    ;;     0..64  hash state
    ;;    64..80  number of bytes read across all updates (128bit)
    ;;   80..208  store words between updates

    (local $bytes_read i64)
    (local $bytes_read_overflow i64)
    (local $check_overflow i64)
    (local $last_word i64)
    (local $tail i64)

    ;; expanded message schedule
    (local $w0 i64)  (local $w1 i64)  (local $w2 i64)  (local $w3 i64)  
    (local $w4 i64)  (local $w5 i64)  (local $w6 i64)  (local $w7 i64) 
    (local $w8 i64)  (local $w9 i64)  (local $w10 i64) (local $w11 i64) 
    (local $w12 i64) (local $w13 i64) (local $w14 i64) (local $w15 i64)

    (set_local $bytes_read (i64.load offset=64 (get_local $ctx)))
    (set_local $bytes_read_overflow (i64.load offset=72 (get_local $ctx)))
    (set_local $tail (i64.add (i64.and (get_local $bytes_read) (i64.const 0x7f)) (i64.extend_u/i32 (get_local $length))))

    ;; load current block position
    (set_local $check_overflow (get_local $bytes_read))
    (set_local $bytes_read (i64.add (get_local $bytes_read) (i64.extend_u/i32 (get_local $length))))
    (i64.store offset=64 (get_local $ctx) (get_local $bytes_read))

    ;; carry n > 64 bits for i128 length
    (if (i64.lt_u (get_local $bytes_read) (get_local $check_overflow))
        (then
            (set_local $bytes_read_overflow (i64.add (get_local $bytes_read_overflow) (i64.const 1)))
            (i64.store offset=72 (get_local $ctx) (get_local $bytes_read_overflow))))

    (block $finish
      (set_local $w0  (i64.load offset=80  (get_local $ctx)))
      (set_local $w1  (i64.load offset=88  (get_local $ctx)))
      (set_local $w2  (i64.load offset=96  (get_local $ctx)))
      (set_local $w3  (i64.load offset=104 (get_local $ctx)))
      (set_local $w4  (i64.load offset=112 (get_local $ctx)))
      (set_local $w5  (i64.load offset=120 (get_local $ctx)))
      (set_local $w6  (i64.load offset=128 (get_local $ctx)))
      (set_local $w7  (i64.load offset=136 (get_local $ctx)))
      (set_local $w8  (i64.load offset=144 (get_local $ctx)))
      (set_local $w9  (i64.load offset=152 (get_local $ctx)))
      (set_local $w10 (i64.load offset=160 (get_local $ctx)))
      (set_local $w11 (i64.load offset=168 (get_local $ctx)))
      (set_local $w12 (i64.load offset=176 (get_local $ctx)))
      (set_local $w13 (i64.load offset=184 (get_local $ctx)))
      (set_local $w14 (i64.load offset=192 (get_local $ctx)))
      (set_local $w15 (i64.load offset=200 (get_local $ctx)))

      (tee_local $tail (i64.sub (get_local $tail) (i64.const 128)))
      (i64.const 0)
      (i64.lt_s)
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

        (set_local $roi (i32.add (get_local $roi) (i32.const 128)))

        (tee_local $tail (i64.sub (get_local $tail) (i64.const 128)))
        (i64.const 0)
        (i64.lt_s)
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

    (if (i32.eq (get_local $final) (i32.const 1))
      (then
        (set_local $tail (i64.and (get_local $bytes_read) (i64.const 0x7f)))
        (set_local $last_word (i64.shl (i64.const 0x80) (i64.shl (i64.and (get_local $tail) (i64.const 0x7)) (i64.const 3))))

        (block $pad_end
                (block $13
                    (block $12
                        (block $11
                            (block $10
                                (block $9
                                    (block $8
                                        (block $7
                                            (block $6
                                                (block $5
                                                    (block $4
                                                        (block $3
                                                            (block $2
                                                                (block $1
                                                                    (block $0
                                                                        (block $15
                                                                            (block $14
                                                                                (block $switch
                                                                                    (i32.wrap/i64 (get_local $tail))
                                                                                    (i32.const 3)
                                                                                    (i32.shr_u)
                                                                                    (br_table $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15)))
                                                                                
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
                                                                            (call $compress)

                                                                            (i64.store offset=64 (get_local $ctx) (get_local $bytes_read))

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
            (call $i64.bswap)
            (set_local $w14)

            ;; load lower limb of 128bit length
            (set_local $w15 (call $i64.bswap (i64.mul (get_local $bytes_read) (i64.const 8))))

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
            (i64.load offset=0 (get_local $ctx))
            (call $i64.bswap)
            (i64.store offset=0)

            (get_local $ctx)
            (i64.load offset=8 (get_local $ctx))
            (call $i64.bswap)
            (i64.store offset=8)

            (get_local $ctx)
            (i64.load offset=16 (get_local $ctx))
            (call $i64.bswap)
            (i64.store offset=16)

            (get_local $ctx)
            (i64.load offset=24 (get_local $ctx))
            (call $i64.bswap)
            (i64.store offset=24)

            (get_local $ctx)
            (i64.load offset=32 (get_local $ctx))
            (call $i64.bswap)
            (i64.store offset=32)

            (get_local $ctx)
            (i64.load offset=40 (get_local $ctx))
            (call $i64.bswap)
            (i64.store offset=40)

            (get_local $ctx)
            (i64.load offset=48 (get_local $ctx))
            (call $i64.bswap)
            (i64.store offset=48)

            (get_local $ctx)
            (i64.load offset=56 (get_local $ctx))
            (call $i64.bswap)
            (i64.store offset=56)))))

    ;; HASH COMPLETE FOR MESSAGE BLOCK
