(module
  (func $i32.log (import "debug" "log") (param i32))
  (func $i32.log_tee (import "debug" "log_tee") (param i32) (result i32))
  ;; No i64 interop with JS yet - but maybe coming with WebAssembly BigInt
  ;; So we can instead fake this by splitting the i64 into two i32 limbs,
  ;; however these are WASM functions using i32x2.log:
  (func $i32x2.log (import "debug" "log") (param i32) (param i32))
  (func $f32.log (import "debug" "log") (param f32))
  (func $f32.log_tee (import "debug" "log_tee") (param f32) (result f32))
  (func $f64.log (import "debug" "log") (param f64))
  (func $f64.log_tee (import "debug" "log_tee") (param f64) (result f64))

  (memory (export "memory") 10 1000)

  ;; i64 logging by splitting into two i32 limbs
  (func $i64.log
      (param $0 i64)
      (call $i32x2.log
          ;; Upper limb
          (i32.wrap/i64
              (i64.shr_u (get_local $0)
                       (i64.const 32)))
          ;; Lower limb
          (i32.wrap/i64 (get_local $0))))

  (func $i64.log_tee
      (param $0 i64)
      (result i64)
      (call $i64.log (get_local $0))
      (return (get_local $0)))

  (func $sha512_monolith (export "sha512_monolith") (param $ctx i32) (param $input i32) (param $input_end i32) (param $final i32)
    (result i32)

    (local $i i32)
    (local $ptr i32)
    (local $bytes_read i32)
    (local $end_point i32)
    (local $block_position i32)
    (local $last_word i64)
    (local $leftover i32)

    ;; registers
    (local $a i64)
    (local $b i64)
    (local $c i64)
    (local $d i64)
    (local $e i64)
    (local $f i64)
    (local $g i64)
    (local $h i64)

    ;; precomputed values
    (local $T1 i64)
    (local $T2 i64)

    (local $ch_res i64)
    (local $maj_res i64)
    (local $big_sig0_res i64)
    (local $big_sig1_res i64)

    ;; expanded message schedule
    (local $w0 i64)  (local $w1 i64)  (local $w2 i64)  (local $w3 i64)  
    (local $w4 i64)  (local $w5 i64)  (local $w6 i64)  (local $w7 i64) 
    (local $w8 i64)  (local $w9 i64)  (local $w10 i64) (local $w11 i64) 
    (local $w12 i64) (local $w13 i64) (local $w14 i64) (local $w15 i64)
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

    (local $k0 i64)  (local $k1 i64)  (local $k2 i64)  (local $k3 i64)  
    (local $k4 i64)  (local $k5 i64)  (local $k6 i64)  (local $k7 i64) 
    (local $k8 i64)  (local $k9 i64)  (local $k10 i64) (local $k11 i64) 
    (local $k12 i64) (local $k13 i64) (local $k14 i64) (local $k15 i64)
    (local $k16 i64) (local $k17 i64) (local $k18 i64) (local $k19 i64) 
    (local $k20 i64) (local $k21 i64) (local $k22 i64) (local $k23 i64)
    (local $k24 i64) (local $k25 i64) (local $k26 i64) (local $k27 i64) 
    (local $k28 i64) (local $k29 i64) (local $k30 i64) (local $k31 i64)
    (local $k32 i64) (local $k33 i64) (local $k34 i64) (local $k35 i64) 
    (local $k36 i64) (local $k37 i64) (local $k38 i64) (local $k39 i64)
    (local $k40 i64) (local $k41 i64) (local $k42 i64) (local $k43 i64) 
    (local $k44 i64) (local $k45 i64) (local $k46 i64) (local $k47 i64)
    (local $k48 i64) (local $k49 i64) (local $k50 i64) (local $k51 i64) 
    (local $k52 i64) (local $k53 i64) (local $k54 i64) (local $k55 i64)
    (local $k56 i64) (local $k57 i64) (local $k58 i64) (local $k59 i64) 
    (local $k60 i64) (local $k61 i64) (local $k62 i64) (local $k63 i64)
    (local $k64 i64) (local $k65 i64) (local $k66 i64) (local $k67 i64)
    (local $k68 i64) (local $k69 i64) (local $k70 i64) (local $k71 i64) 
    (local $k72 i64) (local $k73 i64) (local $k74 i64) (local $k75 i64) 
    (local $k76 i64) (local $k77 i64) (local $k78 i64) (local $k79 i64)

    (set_local $a (i64.xor (i64.const 0x6a09e667f3bcc908) (i64.const 0)))
    (set_local $b (i64.xor (i64.const 0xbb67ae8584caa73b) (i64.const 0)))
    (set_local $c (i64.xor (i64.const 0x3c6ef372fe94f82b) (i64.const 0)))
    (set_local $d (i64.xor (i64.const 0xa54ff53a5f1d36f1) (i64.const 0)))
    (set_local $e (i64.xor (i64.const 0x510e527fade682d1) (i64.const 0)))
    (set_local $f (i64.xor (i64.const 0x9b05688c2b3e6c1f) (i64.const 0)))
    (set_local $g (i64.xor (i64.const 0x1f83d9abfb41bd6b) (i64.const 0)))
    (set_local $h (i64.xor (i64.const 0x5be0cd19137e2179) (i64.const 0)))

    (set_local $k0  (i64.xor (i64.const 0x428a2f98d728ae22) (i64.const 0)))
    (set_local $k1  (i64.xor (i64.const 0x7137449123ef65cd) (i64.const 0)))
    (set_local $k2  (i64.xor (i64.const 0xb5c0fbcfec4d3b2f) (i64.const 0)))
    (set_local $k3  (i64.xor (i64.const 0xe9b5dba58189dbbc) (i64.const 0)))
    (set_local $k4  (i64.xor (i64.const 0x3956c25bf348b538) (i64.const 0)))
    (set_local $k5  (i64.xor (i64.const 0x59f111f1b605d019) (i64.const 0)))
    (set_local $k6  (i64.xor (i64.const 0x923f82a4af194f9b) (i64.const 0)))
    (set_local $k7  (i64.xor (i64.const 0xab1c5ed5da6d8118) (i64.const 0)))
    (set_local $k8  (i64.xor (i64.const 0xd807aa98a3030242) (i64.const 0)))
    (set_local $k9  (i64.xor (i64.const 0x12835b0145706fbe) (i64.const 0)))
    (set_local $k10 (i64.xor (i64.const 0x243185be4ee4b28c) (i64.const 0)))
    (set_local $k11 (i64.xor (i64.const 0x550c7dc3d5ffb4e2) (i64.const 0)))
    (set_local $k12 (i64.xor (i64.const 0x72be5d74f27b896f) (i64.const 0)))
    (set_local $k13 (i64.xor (i64.const 0x80deb1fe3b1696b1) (i64.const 0)))
    (set_local $k14 (i64.xor (i64.const 0x9bdc06a725c71235) (i64.const 0)))
    (set_local $k15 (i64.xor (i64.const 0xc19bf174cf692694) (i64.const 0)))
    (set_local $k16 (i64.xor (i64.const 0xe49b69c19ef14ad2) (i64.const 0)))
    (set_local $k17 (i64.xor (i64.const 0xefbe4786384f25e3) (i64.const 0)))
    (set_local $k18 (i64.xor (i64.const 0x0fc19dc68b8cd5b5) (i64.const 0)))
    (set_local $k19 (i64.xor (i64.const 0x240ca1cc77ac9c65) (i64.const 0)))
    (set_local $k20 (i64.xor (i64.const 0x2de92c6f592b0275) (i64.const 0)))
    (set_local $k21 (i64.xor (i64.const 0x4a7484aa6ea6e483) (i64.const 0)))
    (set_local $k22 (i64.xor (i64.const 0x5cb0a9dcbd41fbd4) (i64.const 0)))
    (set_local $k23 (i64.xor (i64.const 0x76f988da831153b5) (i64.const 0)))
    (set_local $k24 (i64.xor (i64.const 0x983e5152ee66dfab) (i64.const 0)))
    (set_local $k25 (i64.xor (i64.const 0xa831c66d2db43210) (i64.const 0)))
    (set_local $k26 (i64.xor (i64.const 0xb00327c898fb213f) (i64.const 0)))
    (set_local $k27 (i64.xor (i64.const 0xbf597fc7beef0ee4) (i64.const 0)))
    (set_local $k28 (i64.xor (i64.const 0xc6e00bf33da88fc2) (i64.const 0)))
    (set_local $k29 (i64.xor (i64.const 0xd5a79147930aa725) (i64.const 0)))
    (set_local $k30 (i64.xor (i64.const 0x06ca6351e003826f) (i64.const 0)))
    (set_local $k31 (i64.xor (i64.const 0x142929670a0e6e70) (i64.const 0)))
    (set_local $k32 (i64.xor (i64.const 0x27b70a8546d22ffc) (i64.const 0)))
    (set_local $k33 (i64.xor (i64.const 0x2e1b21385c26c926) (i64.const 0)))
    (set_local $k34 (i64.xor (i64.const 0x4d2c6dfc5ac42aed) (i64.const 0)))
    (set_local $k35 (i64.xor (i64.const 0x53380d139d95b3df) (i64.const 0)))
    (set_local $k36 (i64.xor (i64.const 0x650a73548baf63de) (i64.const 0)))
    (set_local $k37 (i64.xor (i64.const 0x766a0abb3c77b2a8) (i64.const 0)))
    (set_local $k38 (i64.xor (i64.const 0x81c2c92e47edaee6) (i64.const 0)))
    (set_local $k39 (i64.xor (i64.const 0x92722c851482353b) (i64.const 0)))
    (set_local $k40 (i64.xor (i64.const 0xa2bfe8a14cf10364) (i64.const 0)))
    (set_local $k41 (i64.xor (i64.const 0xa81a664bbc423001) (i64.const 0)))
    (set_local $k42 (i64.xor (i64.const 0xc24b8b70d0f89791) (i64.const 0)))
    (set_local $k43 (i64.xor (i64.const 0xc76c51a30654be30) (i64.const 0)))
    (set_local $k44 (i64.xor (i64.const 0xd192e819d6ef5218) (i64.const 0)))
    (set_local $k45 (i64.xor (i64.const 0xd69906245565a910) (i64.const 0)))
    (set_local $k46 (i64.xor (i64.const 0xf40e35855771202a) (i64.const 0)))
    (set_local $k47 (i64.xor (i64.const 0x106aa07032bbd1b8) (i64.const 0)))
    (set_local $k48 (i64.xor (i64.const 0x19a4c116b8d2d0c8) (i64.const 0)))
    (set_local $k49 (i64.xor (i64.const 0x1e376c085141ab53) (i64.const 0)))
    (set_local $k50 (i64.xor (i64.const 0x2748774cdf8eeb99) (i64.const 0)))
    (set_local $k51 (i64.xor (i64.const 0x34b0bcb5e19b48a8) (i64.const 0)))
    (set_local $k52 (i64.xor (i64.const 0x391c0cb3c5c95a63) (i64.const 0)))
    (set_local $k53 (i64.xor (i64.const 0x4ed8aa4ae3418acb) (i64.const 0)))
    (set_local $k54 (i64.xor (i64.const 0x5b9cca4f7763e373) (i64.const 0)))
    (set_local $k55 (i64.xor (i64.const 0x682e6ff3d6b2b8a3) (i64.const 0)))
    (set_local $k56 (i64.xor (i64.const 0x748f82ee5defb2fc) (i64.const 0)))
    (set_local $k57 (i64.xor (i64.const 0x78a5636f43172f60) (i64.const 0)))
    (set_local $k58 (i64.xor (i64.const 0x84c87814a1f0ab72) (i64.const 0)))
    (set_local $k59 (i64.xor (i64.const 0x8cc702081a6439ec) (i64.const 0)))
    (set_local $k60 (i64.xor (i64.const 0x90befffa23631e28) (i64.const 0)))
    (set_local $k61 (i64.xor (i64.const 0xa4506cebde82bde9) (i64.const 0)))
    (set_local $k62 (i64.xor (i64.const 0xbef9a3f7b2c67915) (i64.const 0)))
    (set_local $k63 (i64.xor (i64.const 0xc67178f2e372532b) (i64.const 0)))
    (set_local $k64 (i64.xor (i64.const 0xca273eceea26619c) (i64.const 0)))
    (set_local $k65 (i64.xor (i64.const 0xd186b8c721c0c207) (i64.const 0)))
    (set_local $k66 (i64.xor (i64.const 0xeada7dd6cde0eb1e) (i64.const 0)))
    (set_local $k67 (i64.xor (i64.const 0xf57d4f7fee6ed178) (i64.const 0)))
    (set_local $k68 (i64.xor (i64.const 0x06f067aa72176fba) (i64.const 0)))
    (set_local $k69 (i64.xor (i64.const 0x0a637dc5a2c898a6) (i64.const 0)))
    (set_local $k70 (i64.xor (i64.const 0x113f9804bef90dae) (i64.const 0)))
    (set_local $k71 (i64.xor (i64.const 0x1b710b35131c471b) (i64.const 0)))
    (set_local $k72 (i64.xor (i64.const 0x28db77f523047d84) (i64.const 0)))
    (set_local $k73 (i64.xor (i64.const 0x32caab7b40c72493) (i64.const 0)))
    (set_local $k74 (i64.xor (i64.const 0x3c9ebe0a15c9bebc) (i64.const 0)))
    (set_local $k75 (i64.xor (i64.const 0x431d67c49c100d4c) (i64.const 0)))
    (set_local $k76 (i64.xor (i64.const 0x4cc5d4becb3e42b6) (i64.const 0)))
    (set_local $k77 (i64.xor (i64.const 0x597f299cfc657e2a) (i64.const 0)))
    (set_local $k78 (i64.xor (i64.const 0x5fcb6fab3ad6faec) (i64.const 0)))
    (set_local $k79 (i64.xor (i64.const 0x6c44198c4a475817) (i64.const 0)))

    ;; load current block position
    (set_local $bytes_read (i32.load offset=184 (get_local $ctx)))
    (set_local $block_position (i32.rem_u (get_local $bytes_read) (i32.const 128)))
    (set_local $leftover (i32.rem_u (get_local $input_end) (i32.const 8)))
    (set_local $end_point (i32.sub (get_local $input_end) (get_local $leftover)))

    (set_local $ptr (get_local $input))
    (block $break
        (block $0
            (block $1
                (block $2
                    (block $3
                        (block $4
                            (block $5
                                (block $6
                                    (block $7
                                        (block $8
                                            (block $9
                                                (block $10
                                                    (block $11
                                                        (block $12
                                                            (block $13
                                                                (block $14
                                                                    (block $15
                                                                        (block $switch
                                                                            (br_table $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15
                                                                                (i32.div_u (get_local $block_position) (i32.const 8)))))

                                                                        (set_local $w15 (get_local $last_word))
                                                                        (set_local $w14 (i64.load offset=176 (get_local $ctx)))
                                                                        (set_local $w13 (i64.load offset=168 (get_local $ctx)))
                                                                        (set_local $w12 (i64.load offset=160 (get_local $ctx)))
                                                                        (set_local $w11 (i64.load offset=152 (get_local $ctx)))
                                                                        (set_local $w10 (i64.load offset=144 (get_local $ctx)))
                                                                        (set_local $w9 (i64.load offset=136 (get_local $ctx)))
                                                                        (set_local $w8 (i64.load offset=128 (get_local $ctx)))
                                                                        (set_local $w7 (i64.load offset=120 (get_local $ctx)))
                                                                        (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                                                        (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                                                        (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                                                        (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                                                        (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                                                        (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                                                        (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                                                        (br $break))

                                                                    (set_local $w13 (i64.load offset=168 (get_local $ctx)))
                                                                    (set_local $w12 (i64.load offset=160 (get_local $ctx)))
                                                                    (set_local $w11 (i64.load offset=152 (get_local $ctx)))
                                                                    (set_local $w10 (i64.load offset=144 (get_local $ctx)))
                                                                    (set_local $w9 (i64.load offset=136 (get_local $ctx)))
                                                                    (set_local $w8 (i64.load offset=128 (get_local $ctx)))
                                                                    (set_local $w7 (i64.load offset=120 (get_local $ctx)))
                                                                    (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                                                    (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                                                    (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                                                    (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                                                    (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                                                    (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                                                    (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                                                    (br $break))

                                                                (set_local $w12 (i64.load offset=160 (get_local $ctx)))
                                                                (set_local $w11 (i64.load offset=152 (get_local $ctx)))
                                                                (set_local $w10 (i64.load offset=144 (get_local $ctx)))
                                                                (set_local $w9 (i64.load offset=136 (get_local $ctx)))
                                                                (set_local $w8 (i64.load offset=128 (get_local $ctx)))
                                                                (set_local $w7 (i64.load offset=120 (get_local $ctx)))
                                                                (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                                                (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                                                (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                                                (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                                                (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                                                (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                                                (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                                                (br $break))

                                                            (set_local $w11 (i64.load offset=152 (get_local $ctx)))
                                                            (set_local $w10 (i64.load offset=144 (get_local $ctx)))
                                                            (set_local $w9 (i64.load offset=136 (get_local $ctx)))
                                                            (set_local $w8 (i64.load offset=128 (get_local $ctx)))
                                                            (set_local $w7 (i64.load offset=120 (get_local $ctx)))
                                                            (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                                            (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                                            (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                                            (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                                            (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                                            (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                                            (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                                            (br $break))

                                                        (set_local $w10 (i64.load offset=144 (get_local $ctx)))
                                                        (set_local $w9 (i64.load offset=136 (get_local $ctx)))
                                                        (set_local $w8 (i64.load offset=128 (get_local $ctx)))
                                                        (set_local $w7 (i64.load offset=120 (get_local $ctx)))
                                                        (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                                        (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                                        (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                                        (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                                        (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                                        (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                                        (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                                        (br $break))

                                                    (set_local $w9 (i64.load offset=136 (get_local $ctx)))
                                                    (set_local $w8 (i64.load offset=128 (get_local $ctx)))
                                                    (set_local $w7 (i64.load offset=120 (get_local $ctx)))
                                                    (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                                    (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                                    (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                                    (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                                    (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                                    (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                                    (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                                    (br $break))

                                                (set_local $w8 (i64.load offset=128 (get_local $ctx)))
                                                (set_local $w7 (i64.load offset=120 (get_local $ctx)))
                                                (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                                (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                                (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                                (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                                (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                                (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                                (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                                (br $break))

                                            (set_local $w7 (i64.load offset=120 (get_local $ctx)))
                                            (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                            (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                            (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                            (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                            (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                            (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                            (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                            (br $break))

                                        (set_local $w6 (i64.load offset=112 (get_local $ctx)))
                                        (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                        (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                        (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                        (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                        (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                        (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                        (br $break))

                                    (set_local $w5 (i64.load offset=104 (get_local $ctx)))
                                    (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                    (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                    (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                    (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                    (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                    (br $break))

                                (set_local $w4 (i64.load offset=96 (get_local $ctx)))
                                (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                                (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                                (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                                (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                                (br $break))

                            (set_local $w3 (i64.load offset=88 (get_local $ctx)))
                            (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                            (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                            (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                            (br $break))

                        (set_local $w2 (i64.load offset=80 (get_local $ctx)))
                        (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                        (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                        (br $break))

                    (set_local $w1 (i64.load offset=72 (get_local $ctx)))
                    (set_local $w0 (i64.load offset=64 (get_local $ctx)))                
                    (br $break))
                
                (set_local $w0 (i64.load offset=64 (get_local $ctx)))
                (br $break))

            (br $break))

    ;; update bytes read
    ;; (set_local $bytes_read (i32.add (get_local $bytes_read) (i32.sub (get_local $input_end) (get_local $input))))

    ;; need to keep track of where we are in block, idea is that we fill block to 128bytes then hash.

    (block $end
        (loop $start
            (call $i32.log (get_local $ptr))
            (call $i32.log (get_local $bytes_read))
            (call $i32.log (i32.const 0xbeefdead))
            (call $i64.log (get_local $w1))
            (call $i32.log (get_local $end_point))

            (br_if $end (i32.eq (get_local $ptr) (get_local $end_point)))
            (if (i32.eq (get_local $block_position) (i32.const 128))
                (then
                    ;; (set_local $i (i32.const 7))
                    (set_local $block_position (i32.const 0))
                    (set_local $bytes_read (i32.add (get_local $bytes_read) (i32.const 128)))

                    ;; compress
                    (set_local $w16 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w14) (i64.const 19)) (i64.rotr (get_local $w14) (i64.const 61))) (i64.shr_u (get_local $w14) (i64.const 6))) (get_local $w9)) (i64.xor (i64.xor (i64.rotr (get_local $w1) (i64.const 1)) (i64.rotr (get_local $w1) (i64.const 8))) (i64.shr_u (get_local $w1) (i64.const 7))) (get_local $w0))))
                    (set_local $w17 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w15) (i64.const 19)) (i64.rotr (get_local $w15) (i64.const 61))) (i64.shr_u (get_local $w15) (i64.const 6))) (get_local $w10)) (i64.xor (i64.xor (i64.rotr (get_local $w2) (i64.const 1)) (i64.rotr (get_local $w2) (i64.const 8))) (i64.shr_u (get_local $w2) (i64.const 7))) (get_local $w1))))
                    (set_local $w18 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w16) (i64.const 19)) (i64.rotr (get_local $w16) (i64.const 61))) (i64.shr_u (get_local $w16) (i64.const 6))) (get_local $w11)) (i64.xor (i64.xor (i64.rotr (get_local $w3) (i64.const 1)) (i64.rotr (get_local $w3) (i64.const 8))) (i64.shr_u (get_local $w3) (i64.const 7))) (get_local $w2))))
                    (set_local $w19 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w17) (i64.const 19)) (i64.rotr (get_local $w17) (i64.const 61))) (i64.shr_u (get_local $w17) (i64.const 6))) (get_local $w12)) (i64.xor (i64.xor (i64.rotr (get_local $w4) (i64.const 1)) (i64.rotr (get_local $w4) (i64.const 8))) (i64.shr_u (get_local $w4) (i64.const 7))) (get_local $w3))))
                    (set_local $w20 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w18) (i64.const 19)) (i64.rotr (get_local $w18) (i64.const 61))) (i64.shr_u (get_local $w18) (i64.const 6))) (get_local $w13)) (i64.xor (i64.xor (i64.rotr (get_local $w5) (i64.const 1)) (i64.rotr (get_local $w5) (i64.const 8))) (i64.shr_u (get_local $w5) (i64.const 7))) (get_local $w4))))
                    (set_local $w21 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w19) (i64.const 19)) (i64.rotr (get_local $w19) (i64.const 61))) (i64.shr_u (get_local $w19) (i64.const 6))) (get_local $w14)) (i64.xor (i64.xor (i64.rotr (get_local $w6) (i64.const 1)) (i64.rotr (get_local $w6) (i64.const 8))) (i64.shr_u (get_local $w6) (i64.const 7))) (get_local $w5))))
                    (set_local $w22 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w20) (i64.const 19)) (i64.rotr (get_local $w20) (i64.const 61))) (i64.shr_u (get_local $w20) (i64.const 6))) (get_local $w15)) (i64.xor (i64.xor (i64.rotr (get_local $w7) (i64.const 1)) (i64.rotr (get_local $w7) (i64.const 8))) (i64.shr_u (get_local $w7) (i64.const 7))) (get_local $w6))))
                    (set_local $w23 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w21) (i64.const 19)) (i64.rotr (get_local $w21) (i64.const 61))) (i64.shr_u (get_local $w21) (i64.const 6))) (get_local $w16)) (i64.xor (i64.xor (i64.rotr (get_local $w8) (i64.const 1)) (i64.rotr (get_local $w8) (i64.const 8))) (i64.shr_u (get_local $w8) (i64.const 7))) (get_local $w7))))
                    (set_local $w24 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w22) (i64.const 19)) (i64.rotr (get_local $w22) (i64.const 61))) (i64.shr_u (get_local $w22) (i64.const 6))) (get_local $w17)) (i64.xor (i64.xor (i64.rotr (get_local $w9) (i64.const 1)) (i64.rotr (get_local $w9) (i64.const 8))) (i64.shr_u (get_local $w9) (i64.const 7))) (get_local $w8))))
                    (set_local $w25 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w23) (i64.const 19)) (i64.rotr (get_local $w23) (i64.const 61))) (i64.shr_u (get_local $w23) (i64.const 6))) (get_local $w18)) (i64.xor (i64.xor (i64.rotr (get_local $w10) (i64.const 1)) (i64.rotr (get_local $w10) (i64.const 8))) (i64.shr_u (get_local $w10) (i64.const 7))) (get_local $w9))))
                    (set_local $w26 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w24) (i64.const 19)) (i64.rotr (get_local $w24) (i64.const 61))) (i64.shr_u (get_local $w24) (i64.const 6))) (get_local $w19)) (i64.xor (i64.xor (i64.rotr (get_local $w11) (i64.const 1)) (i64.rotr (get_local $w11) (i64.const 8))) (i64.shr_u (get_local $w11) (i64.const 7))) (get_local $w10))))
                    (set_local $w27 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w25) (i64.const 19)) (i64.rotr (get_local $w25) (i64.const 61))) (i64.shr_u (get_local $w25) (i64.const 6))) (get_local $w20)) (i64.xor (i64.xor (i64.rotr (get_local $w12) (i64.const 1)) (i64.rotr (get_local $w12) (i64.const 8))) (i64.shr_u (get_local $w12) (i64.const 7))) (get_local $w11))))
                    (set_local $w28 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w26) (i64.const 19)) (i64.rotr (get_local $w26) (i64.const 61))) (i64.shr_u (get_local $w26) (i64.const 6))) (get_local $w21)) (i64.xor (i64.xor (i64.rotr (get_local $w13) (i64.const 1)) (i64.rotr (get_local $w13) (i64.const 8))) (i64.shr_u (get_local $w13) (i64.const 7))) (get_local $w12))))
                    (set_local $w29 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w27) (i64.const 19)) (i64.rotr (get_local $w27) (i64.const 61))) (i64.shr_u (get_local $w27) (i64.const 6))) (get_local $w22)) (i64.xor (i64.xor (i64.rotr (get_local $w14) (i64.const 1)) (i64.rotr (get_local $w14) (i64.const 8))) (i64.shr_u (get_local $w14) (i64.const 7))) (get_local $w13))))
                    (set_local $w30 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w28) (i64.const 19)) (i64.rotr (get_local $w28) (i64.const 61))) (i64.shr_u (get_local $w28) (i64.const 6))) (get_local $w23)) (i64.xor (i64.xor (i64.rotr (get_local $w15) (i64.const 1)) (i64.rotr (get_local $w15) (i64.const 8))) (i64.shr_u (get_local $w15) (i64.const 7))) (get_local $w14))))
                    (set_local $w31 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w29) (i64.const 19)) (i64.rotr (get_local $w29) (i64.const 61))) (i64.shr_u (get_local $w29) (i64.const 6))) (get_local $w24)) (i64.xor (i64.xor (i64.rotr (get_local $w16) (i64.const 1)) (i64.rotr (get_local $w16) (i64.const 8))) (i64.shr_u (get_local $w16) (i64.const 7))) (get_local $w15))))
                    (set_local $w32 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w30) (i64.const 19)) (i64.rotr (get_local $w30) (i64.const 61))) (i64.shr_u (get_local $w30) (i64.const 6))) (get_local $w25)) (i64.xor (i64.xor (i64.rotr (get_local $w17) (i64.const 1)) (i64.rotr (get_local $w17) (i64.const 8))) (i64.shr_u (get_local $w17) (i64.const 7))) (get_local $w16))))
                    (set_local $w33 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w31) (i64.const 19)) (i64.rotr (get_local $w31) (i64.const 61))) (i64.shr_u (get_local $w31) (i64.const 6))) (get_local $w26)) (i64.xor (i64.xor (i64.rotr (get_local $w18) (i64.const 1)) (i64.rotr (get_local $w18) (i64.const 8))) (i64.shr_u (get_local $w18) (i64.const 7))) (get_local $w17))))
                    (set_local $w34 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w32) (i64.const 19)) (i64.rotr (get_local $w32) (i64.const 61))) (i64.shr_u (get_local $w32) (i64.const 6))) (get_local $w27)) (i64.xor (i64.xor (i64.rotr (get_local $w19) (i64.const 1)) (i64.rotr (get_local $w19) (i64.const 8))) (i64.shr_u (get_local $w19) (i64.const 7))) (get_local $w18))))
                    (set_local $w35 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w33) (i64.const 19)) (i64.rotr (get_local $w33) (i64.const 61))) (i64.shr_u (get_local $w33) (i64.const 6))) (get_local $w28)) (i64.xor (i64.xor (i64.rotr (get_local $w20) (i64.const 1)) (i64.rotr (get_local $w20) (i64.const 8))) (i64.shr_u (get_local $w20) (i64.const 7))) (get_local $w19))))
                    (set_local $w36 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w34) (i64.const 19)) (i64.rotr (get_local $w34) (i64.const 61))) (i64.shr_u (get_local $w34) (i64.const 6))) (get_local $w29)) (i64.xor (i64.xor (i64.rotr (get_local $w21) (i64.const 1)) (i64.rotr (get_local $w21) (i64.const 8))) (i64.shr_u (get_local $w21) (i64.const 7))) (get_local $w20))))
                    (set_local $w37 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w35) (i64.const 19)) (i64.rotr (get_local $w35) (i64.const 61))) (i64.shr_u (get_local $w35) (i64.const 6))) (get_local $w30)) (i64.xor (i64.xor (i64.rotr (get_local $w22) (i64.const 1)) (i64.rotr (get_local $w22) (i64.const 8))) (i64.shr_u (get_local $w22) (i64.const 7))) (get_local $w21))))
                    (set_local $w38 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w36) (i64.const 19)) (i64.rotr (get_local $w36) (i64.const 61))) (i64.shr_u (get_local $w36) (i64.const 6))) (get_local $w31)) (i64.xor (i64.xor (i64.rotr (get_local $w23) (i64.const 1)) (i64.rotr (get_local $w23) (i64.const 8))) (i64.shr_u (get_local $w23) (i64.const 7))) (get_local $w22))))
                    (set_local $w39 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w37) (i64.const 19)) (i64.rotr (get_local $w37) (i64.const 61))) (i64.shr_u (get_local $w37) (i64.const 6))) (get_local $w32)) (i64.xor (i64.xor (i64.rotr (get_local $w24) (i64.const 1)) (i64.rotr (get_local $w24) (i64.const 8))) (i64.shr_u (get_local $w24) (i64.const 7))) (get_local $w23))))
                    (set_local $w40 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w38) (i64.const 19)) (i64.rotr (get_local $w38) (i64.const 61))) (i64.shr_u (get_local $w38) (i64.const 6))) (get_local $w33)) (i64.xor (i64.xor (i64.rotr (get_local $w25) (i64.const 1)) (i64.rotr (get_local $w25) (i64.const 8))) (i64.shr_u (get_local $w25) (i64.const 7))) (get_local $w24))))
                    (set_local $w41 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w39) (i64.const 19)) (i64.rotr (get_local $w39) (i64.const 61))) (i64.shr_u (get_local $w39) (i64.const 6))) (get_local $w34)) (i64.xor (i64.xor (i64.rotr (get_local $w26) (i64.const 1)) (i64.rotr (get_local $w26) (i64.const 8))) (i64.shr_u (get_local $w26) (i64.const 7))) (get_local $w25))))
                    (set_local $w42 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w40) (i64.const 19)) (i64.rotr (get_local $w40) (i64.const 61))) (i64.shr_u (get_local $w40) (i64.const 6))) (get_local $w35)) (i64.xor (i64.xor (i64.rotr (get_local $w27) (i64.const 1)) (i64.rotr (get_local $w27) (i64.const 8))) (i64.shr_u (get_local $w27) (i64.const 7))) (get_local $w26))))
                    (set_local $w43 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w41) (i64.const 19)) (i64.rotr (get_local $w41) (i64.const 61))) (i64.shr_u (get_local $w41) (i64.const 6))) (get_local $w36)) (i64.xor (i64.xor (i64.rotr (get_local $w28) (i64.const 1)) (i64.rotr (get_local $w28) (i64.const 8))) (i64.shr_u (get_local $w28) (i64.const 7))) (get_local $w27))))
                    (set_local $w44 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w42) (i64.const 19)) (i64.rotr (get_local $w42) (i64.const 61))) (i64.shr_u (get_local $w42) (i64.const 6))) (get_local $w37)) (i64.xor (i64.xor (i64.rotr (get_local $w29) (i64.const 1)) (i64.rotr (get_local $w29) (i64.const 8))) (i64.shr_u (get_local $w29) (i64.const 7))) (get_local $w28))))
                    (set_local $w45 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w43) (i64.const 19)) (i64.rotr (get_local $w43) (i64.const 61))) (i64.shr_u (get_local $w43) (i64.const 6))) (get_local $w38)) (i64.xor (i64.xor (i64.rotr (get_local $w30) (i64.const 1)) (i64.rotr (get_local $w30) (i64.const 8))) (i64.shr_u (get_local $w30) (i64.const 7))) (get_local $w29))))
                    (set_local $w46 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w44) (i64.const 19)) (i64.rotr (get_local $w44) (i64.const 61))) (i64.shr_u (get_local $w44) (i64.const 6))) (get_local $w39)) (i64.xor (i64.xor (i64.rotr (get_local $w31) (i64.const 1)) (i64.rotr (get_local $w31) (i64.const 8))) (i64.shr_u (get_local $w31) (i64.const 7))) (get_local $w30))))
                    (set_local $w47 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w45) (i64.const 19)) (i64.rotr (get_local $w45) (i64.const 61))) (i64.shr_u (get_local $w45) (i64.const 6))) (get_local $w40)) (i64.xor (i64.xor (i64.rotr (get_local $w32) (i64.const 1)) (i64.rotr (get_local $w32) (i64.const 8))) (i64.shr_u (get_local $w32) (i64.const 7))) (get_local $w31))))
                    (set_local $w48 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w46) (i64.const 19)) (i64.rotr (get_local $w46) (i64.const 61))) (i64.shr_u (get_local $w46) (i64.const 6))) (get_local $w41)) (i64.xor (i64.xor (i64.rotr (get_local $w33) (i64.const 1)) (i64.rotr (get_local $w33) (i64.const 8))) (i64.shr_u (get_local $w33) (i64.const 7))) (get_local $w32))))
                    (set_local $w49 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w47) (i64.const 19)) (i64.rotr (get_local $w47) (i64.const 61))) (i64.shr_u (get_local $w47) (i64.const 6))) (get_local $w42)) (i64.xor (i64.xor (i64.rotr (get_local $w34) (i64.const 1)) (i64.rotr (get_local $w34) (i64.const 8))) (i64.shr_u (get_local $w34) (i64.const 7))) (get_local $w33))))
                    (set_local $w50 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w48) (i64.const 19)) (i64.rotr (get_local $w48) (i64.const 61))) (i64.shr_u (get_local $w48) (i64.const 6))) (get_local $w43)) (i64.xor (i64.xor (i64.rotr (get_local $w35) (i64.const 1)) (i64.rotr (get_local $w35) (i64.const 8))) (i64.shr_u (get_local $w35) (i64.const 7))) (get_local $w34))))
                    (set_local $w51 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w49) (i64.const 19)) (i64.rotr (get_local $w49) (i64.const 61))) (i64.shr_u (get_local $w49) (i64.const 6))) (get_local $w44)) (i64.xor (i64.xor (i64.rotr (get_local $w36) (i64.const 1)) (i64.rotr (get_local $w36) (i64.const 8))) (i64.shr_u (get_local $w36) (i64.const 7))) (get_local $w35))))
                    (set_local $w52 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w50) (i64.const 19)) (i64.rotr (get_local $w50) (i64.const 61))) (i64.shr_u (get_local $w50) (i64.const 6))) (get_local $w45)) (i64.xor (i64.xor (i64.rotr (get_local $w37) (i64.const 1)) (i64.rotr (get_local $w37) (i64.const 8))) (i64.shr_u (get_local $w37) (i64.const 7))) (get_local $w36))))
                    (set_local $w53 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w51) (i64.const 19)) (i64.rotr (get_local $w51) (i64.const 61))) (i64.shr_u (get_local $w51) (i64.const 6))) (get_local $w46)) (i64.xor (i64.xor (i64.rotr (get_local $w38) (i64.const 1)) (i64.rotr (get_local $w38) (i64.const 8))) (i64.shr_u (get_local $w38) (i64.const 7))) (get_local $w37))))
                    (set_local $w54 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w52) (i64.const 19)) (i64.rotr (get_local $w52) (i64.const 61))) (i64.shr_u (get_local $w52) (i64.const 6))) (get_local $w47)) (i64.xor (i64.xor (i64.rotr (get_local $w39) (i64.const 1)) (i64.rotr (get_local $w39) (i64.const 8))) (i64.shr_u (get_local $w39) (i64.const 7))) (get_local $w38))))
                    (set_local $w55 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w53) (i64.const 19)) (i64.rotr (get_local $w53) (i64.const 61))) (i64.shr_u (get_local $w53) (i64.const 6))) (get_local $w48)) (i64.xor (i64.xor (i64.rotr (get_local $w40) (i64.const 1)) (i64.rotr (get_local $w40) (i64.const 8))) (i64.shr_u (get_local $w40) (i64.const 7))) (get_local $w39))))
                    (set_local $w56 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w54) (i64.const 19)) (i64.rotr (get_local $w54) (i64.const 61))) (i64.shr_u (get_local $w54) (i64.const 6))) (get_local $w49)) (i64.xor (i64.xor (i64.rotr (get_local $w41) (i64.const 1)) (i64.rotr (get_local $w41) (i64.const 8))) (i64.shr_u (get_local $w41) (i64.const 7))) (get_local $w40))))
                    (set_local $w57 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w55) (i64.const 19)) (i64.rotr (get_local $w55) (i64.const 61))) (i64.shr_u (get_local $w55) (i64.const 6))) (get_local $w50)) (i64.xor (i64.xor (i64.rotr (get_local $w42) (i64.const 1)) (i64.rotr (get_local $w42) (i64.const 8))) (i64.shr_u (get_local $w42) (i64.const 7))) (get_local $w41))))
                    (set_local $w58 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w56) (i64.const 19)) (i64.rotr (get_local $w56) (i64.const 61))) (i64.shr_u (get_local $w56) (i64.const 6))) (get_local $w51)) (i64.xor (i64.xor (i64.rotr (get_local $w43) (i64.const 1)) (i64.rotr (get_local $w43) (i64.const 8))) (i64.shr_u (get_local $w43) (i64.const 7))) (get_local $w42))))
                    (set_local $w59 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w57) (i64.const 19)) (i64.rotr (get_local $w57) (i64.const 61))) (i64.shr_u (get_local $w57) (i64.const 6))) (get_local $w52)) (i64.xor (i64.xor (i64.rotr (get_local $w44) (i64.const 1)) (i64.rotr (get_local $w44) (i64.const 8))) (i64.shr_u (get_local $w44) (i64.const 7))) (get_local $w43))))
                    (set_local $w60 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w58) (i64.const 19)) (i64.rotr (get_local $w58) (i64.const 61))) (i64.shr_u (get_local $w58) (i64.const 6))) (get_local $w53)) (i64.xor (i64.xor (i64.rotr (get_local $w45) (i64.const 1)) (i64.rotr (get_local $w45) (i64.const 8))) (i64.shr_u (get_local $w45) (i64.const 7))) (get_local $w44))))
                    (set_local $w61 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w59) (i64.const 19)) (i64.rotr (get_local $w59) (i64.const 61))) (i64.shr_u (get_local $w59) (i64.const 6))) (get_local $w54)) (i64.xor (i64.xor (i64.rotr (get_local $w46) (i64.const 1)) (i64.rotr (get_local $w46) (i64.const 8))) (i64.shr_u (get_local $w46) (i64.const 7))) (get_local $w45))))
                    (set_local $w62 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w60) (i64.const 19)) (i64.rotr (get_local $w60) (i64.const 61))) (i64.shr_u (get_local $w60) (i64.const 6))) (get_local $w55)) (i64.xor (i64.xor (i64.rotr (get_local $w47) (i64.const 1)) (i64.rotr (get_local $w47) (i64.const 8))) (i64.shr_u (get_local $w47) (i64.const 7))) (get_local $w46))))
                    (set_local $w63 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w61) (i64.const 19)) (i64.rotr (get_local $w61) (i64.const 61))) (i64.shr_u (get_local $w61) (i64.const 6))) (get_local $w56)) (i64.xor (i64.xor (i64.rotr (get_local $w48) (i64.const 1)) (i64.rotr (get_local $w48) (i64.const 8))) (i64.shr_u (get_local $w48) (i64.const 7))) (get_local $w47))))
                    (set_local $w64 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w62) (i64.const 19)) (i64.rotr (get_local $w62) (i64.const 61))) (i64.shr_u (get_local $w62) (i64.const 6))) (get_local $w57)) (i64.xor (i64.xor (i64.rotr (get_local $w49) (i64.const 1)) (i64.rotr (get_local $w49) (i64.const 8))) (i64.shr_u (get_local $w49) (i64.const 7))) (get_local $w48))))
                    (set_local $w65 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w63) (i64.const 19)) (i64.rotr (get_local $w63) (i64.const 61))) (i64.shr_u (get_local $w63) (i64.const 6))) (get_local $w58)) (i64.xor (i64.xor (i64.rotr (get_local $w50) (i64.const 1)) (i64.rotr (get_local $w50) (i64.const 8))) (i64.shr_u (get_local $w50) (i64.const 7))) (get_local $w49))))
                    (set_local $w66 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w64) (i64.const 19)) (i64.rotr (get_local $w64) (i64.const 61))) (i64.shr_u (get_local $w64) (i64.const 6))) (get_local $w59)) (i64.xor (i64.xor (i64.rotr (get_local $w51) (i64.const 1)) (i64.rotr (get_local $w51) (i64.const 8))) (i64.shr_u (get_local $w51) (i64.const 7))) (get_local $w50))))
                    (set_local $w67 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w65) (i64.const 19)) (i64.rotr (get_local $w65) (i64.const 61))) (i64.shr_u (get_local $w65) (i64.const 6))) (get_local $w60)) (i64.xor (i64.xor (i64.rotr (get_local $w52) (i64.const 1)) (i64.rotr (get_local $w52) (i64.const 8))) (i64.shr_u (get_local $w52) (i64.const 7))) (get_local $w51))))
                    (set_local $w68 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w66) (i64.const 19)) (i64.rotr (get_local $w66) (i64.const 61))) (i64.shr_u (get_local $w66) (i64.const 6))) (get_local $w61)) (i64.xor (i64.xor (i64.rotr (get_local $w53) (i64.const 1)) (i64.rotr (get_local $w53) (i64.const 8))) (i64.shr_u (get_local $w53) (i64.const 7))) (get_local $w52))))
                    (set_local $w69 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w67) (i64.const 19)) (i64.rotr (get_local $w67) (i64.const 61))) (i64.shr_u (get_local $w67) (i64.const 6))) (get_local $w62)) (i64.xor (i64.xor (i64.rotr (get_local $w54) (i64.const 1)) (i64.rotr (get_local $w54) (i64.const 8))) (i64.shr_u (get_local $w54) (i64.const 7))) (get_local $w53))))
                    (set_local $w70 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w68) (i64.const 19)) (i64.rotr (get_local $w68) (i64.const 61))) (i64.shr_u (get_local $w68) (i64.const 6))) (get_local $w63)) (i64.xor (i64.xor (i64.rotr (get_local $w55) (i64.const 1)) (i64.rotr (get_local $w55) (i64.const 8))) (i64.shr_u (get_local $w55) (i64.const 7))) (get_local $w54))))
                    (set_local $w71 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w69) (i64.const 19)) (i64.rotr (get_local $w69) (i64.const 61))) (i64.shr_u (get_local $w69) (i64.const 6))) (get_local $w64)) (i64.xor (i64.xor (i64.rotr (get_local $w56) (i64.const 1)) (i64.rotr (get_local $w56) (i64.const 8))) (i64.shr_u (get_local $w56) (i64.const 7))) (get_local $w55))))
                    (set_local $w72 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w70) (i64.const 19)) (i64.rotr (get_local $w70) (i64.const 61))) (i64.shr_u (get_local $w70) (i64.const 6))) (get_local $w65)) (i64.xor (i64.xor (i64.rotr (get_local $w57) (i64.const 1)) (i64.rotr (get_local $w57) (i64.const 8))) (i64.shr_u (get_local $w57) (i64.const 7))) (get_local $w56))))
                    (set_local $w73 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w71) (i64.const 19)) (i64.rotr (get_local $w71) (i64.const 61))) (i64.shr_u (get_local $w71) (i64.const 6))) (get_local $w66)) (i64.xor (i64.xor (i64.rotr (get_local $w58) (i64.const 1)) (i64.rotr (get_local $w58) (i64.const 8))) (i64.shr_u (get_local $w58) (i64.const 7))) (get_local $w57))))
                    (set_local $w74 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w72) (i64.const 19)) (i64.rotr (get_local $w72) (i64.const 61))) (i64.shr_u (get_local $w72) (i64.const 6))) (get_local $w67)) (i64.xor (i64.xor (i64.rotr (get_local $w59) (i64.const 1)) (i64.rotr (get_local $w59) (i64.const 8))) (i64.shr_u (get_local $w59) (i64.const 7))) (get_local $w58))))
                    (set_local $w75 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w73) (i64.const 19)) (i64.rotr (get_local $w73) (i64.const 61))) (i64.shr_u (get_local $w73) (i64.const 6))) (get_local $w68)) (i64.xor (i64.xor (i64.rotr (get_local $w60) (i64.const 1)) (i64.rotr (get_local $w60) (i64.const 8))) (i64.shr_u (get_local $w60) (i64.const 7))) (get_local $w59))))
                    (set_local $w76 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w74) (i64.const 19)) (i64.rotr (get_local $w74) (i64.const 61))) (i64.shr_u (get_local $w74) (i64.const 6))) (get_local $w69)) (i64.xor (i64.xor (i64.rotr (get_local $w61) (i64.const 1)) (i64.rotr (get_local $w61) (i64.const 8))) (i64.shr_u (get_local $w61) (i64.const 7))) (get_local $w60))))
                    (set_local $w77 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w75) (i64.const 19)) (i64.rotr (get_local $w75) (i64.const 61))) (i64.shr_u (get_local $w75) (i64.const 6))) (get_local $w70)) (i64.xor (i64.xor (i64.rotr (get_local $w62) (i64.const 1)) (i64.rotr (get_local $w62) (i64.const 8))) (i64.shr_u (get_local $w62) (i64.const 7))) (get_local $w61))))
                    (set_local $w78 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w76) (i64.const 19)) (i64.rotr (get_local $w76) (i64.const 61))) (i64.shr_u (get_local $w76) (i64.const 6))) (get_local $w71)) (i64.xor (i64.xor (i64.rotr (get_local $w63) (i64.const 1)) (i64.rotr (get_local $w63) (i64.const 8))) (i64.shr_u (get_local $w63) (i64.const 7))) (get_local $w62))))
                    (set_local $w79 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w77) (i64.const 19)) (i64.rotr (get_local $w77) (i64.const 61))) (i64.shr_u (get_local $w77) (i64.const 6))) (get_local $w72)) (i64.xor (i64.xor (i64.rotr (get_local $w64) (i64.const 1)) (i64.rotr (get_local $w64) (i64.const 8))) (i64.shr_u (get_local $w64) (i64.const 7))) (get_local $w63))))


                    ;; load previous hash state into registers
                    (set_local $a (i64.load offset=0 (i32.const 0)))
                    (set_local $b (i64.load offset=8 (i32.const 0)))
                    (set_local $c (i64.load offset=16 (i32.const 0)))
                    (set_local $d (i64.load offset=24 (i32.const 0)))
                    (set_local $e (i64.load offset=32 (i32.const 0)))
                    (set_local $f (i64.load offset=40 (i32.const 0)))
                    (set_local $g (i64.load offset=48 (i32.const 0)))
                    (set_local $h (i64.load offset=56 (i32.const 0)))

                    ;; ROUND 0

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K0 + W0
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w0)) (get_local $k0 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 1

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K1 + W1
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w1)) (get_local $k1 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 2

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K2 + W2
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w2)) (get_local $k2 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 3

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K3 + W3
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w3)) (get_local $k3 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 4

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K4 + W4
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w4)) (get_local $k4 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 5

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K5 + W5
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w5)) (get_local $k5 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 6

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K6 + W6
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w6)) (get_local $k6 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 7

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K7 + W7
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w7)) (get_local $k7 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 8

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K8 + W8
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w8)) (get_local $k8 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 9

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K9 + W9
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w9)) (get_local $k9 )))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 10

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K10 + W10
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w10)) (get_local $k10)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 11

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K11 + W11
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w11)) (get_local $k11)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 12

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K12 + W12
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w12)) (get_local $k12)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 13

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K13 + W13
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w13)) (get_local $k13)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 14

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K14 + W14
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w14)) (get_local $k14)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 15

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K15 + W15
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w15)) (get_local $k15)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 16

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K16 + W16
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w16)) (get_local $k16)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 17

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K17 + W17
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w17)) (get_local $k17)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 18

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K18 + W18
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w18)) (get_local $k18)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 19

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K19 + W19
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w19)) (get_local $k19)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 20

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K20 + W20
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w20)) (get_local $k20)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 21

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K21 + W21
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w21)) (get_local $k21)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 22

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K22 + W22
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w22)) (get_local $k22)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 23

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K23 + W23
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w23)) (get_local $k23)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 24

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K24 + W24
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w24)) (get_local $k24)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 25

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K25 + W25
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w25)) (get_local $k25)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 26

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K26 + W26
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w26)) (get_local $k26)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 27

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K27 + W27
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w27)) (get_local $k27)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 28

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K28 + W28
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w28)) (get_local $k28)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 29

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K29 + W29
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w29)) (get_local $k29)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 30

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K30 + W30
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w30)) (get_local $k30)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 31

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K31 + W31
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w31)) (get_local $k31)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 32

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K32 + W32
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w32)) (get_local $k32)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 33

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K33 + W33
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w33)) (get_local $k33)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 34

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K34 + W34
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w34)) (get_local $k34)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 35

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K35 + W35
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w35)) (get_local $k35)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 36

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K36 + W36
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w36)) (get_local $k36)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 37

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K37 + W37
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w37)) (get_local $k37)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 38

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K38 + W38
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w38)) (get_local $k38)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 39

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K39 + W39
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w39)) (get_local $k39)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 40

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K40 + W40
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w40)) (get_local $k40)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 41

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K41 + W41
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w41)) (get_local $k41)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 42

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K42 + W42
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w42)) (get_local $k42)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 43

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K43 + W43
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w43)) (get_local $k43)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 44

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K44 + W44
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w44)) (get_local $k44)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 45

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K45 + W45
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w45)) (get_local $k45)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 46

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K46 + W46
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w46)) (get_local $k46)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 47

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K47 + W47
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w47)) (get_local $k47)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 48

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K48 + W48
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w48)) (get_local $k48)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 49

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K49 + W49
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w49)) (get_local $k49)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 50

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K50 + W50
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w50)) (get_local $k50)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 51

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K51 + W51
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w51)) (get_local $k51)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 52

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K52 + W52
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w52)) (get_local $k52)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 53

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K53 + W53
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w53)) (get_local $k53)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 54

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K54 + W54
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w54)) (get_local $k54)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 55

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K55 + W55
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w55)) (get_local $k55)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 56

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K56 + W56
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w56)) (get_local $k56)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 57

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K57 + W57
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w57)) (get_local $k57)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 58

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K58 + W58
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w58)) (get_local $k58)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 59

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K59 + W59
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w59)) (get_local $k59)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 60

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K60 + W60
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w60)) (get_local $k60)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 61

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K61 + W61
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w61)) (get_local $k61)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 62

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K62 + W62
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w62)) (get_local $k62)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 63

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K63 + W63
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w63)) (get_local $k63)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 64

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K64 + W64
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w64)) (get_local $k64)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 65

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K65 + W65
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w65)) (get_local $k65)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 66

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K66 + W66
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w66)) (get_local $k66)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 67

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K67 + W67
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w67)) (get_local $k67)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 68

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K68 + W68
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w68)) (get_local $k68)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 69

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K69 + W69
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w69)) (get_local $k69)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 70

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K70 + W70
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w70)) (get_local $k70)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 71

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K71 + W71
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w71)) (get_local $k71)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 72

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K72 + W72
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w72)) (get_local $k72)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 73

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K73 + W73
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w73)) (get_local $k73)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 74

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K74 + W74
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w74)) (get_local $k74)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 75

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K75 + W75
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w75)) (get_local $k75)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 76

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K76 + W76
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w76)) (get_local $k76)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 77

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K77 + W77
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w77)) (get_local $k77)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 78

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K78 + W78
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w78)) (get_local $k78)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                    ;; ROUND 79

                    ;; precompute intermediate values

                    ;; T1 = h + big_sig1(e) + ch(e, f, g) + K79 + W79
                    ;; T2 = big_sig0(a) + Maj(a, b, c)

                    (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                    (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                    (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                    (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                    (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w79)) (get_local $k79)))
                    (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                    ;; update registers

                    ;; h <- g
                    (set_local $h (get_local $g))

                    ;; g <- f
                    (set_local $g (get_local $f))  

                    ;; f <- e
                    (set_local $f (get_local $e))  

                    ;; e <- d + T1
                    (set_local $e (i64.add (get_local $d) (get_local $T1)))

                    ;; d <- c
                    (set_local $d (get_local $c))  

                    ;; c <- b
                    (set_local $c (get_local $b))  

                    ;; b <- a
                    (set_local $b (get_local $a))  

                    ;; a <- T1 + T2
                    (set_local $a (i64.add (get_local $T1) (get_local $T2)))))
            
            (block $break
                (block $0
                    (block $1
                        (block $2
                            (block $3
                                (block $4
                                    (block $5
                                        (block $6
                                            (block $7
                                                (block $8
                                                    (block $9
                                                        (block $10
                                                            (block $11
                                                                (block $12
                                                                    (block $13
                                                                        (block $14
                                                                            (block $15
                                                                                (block $switch
                                                                                    (br_table $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15
                                                                                        (i32.div_u (get_local $block_position) (i32.const 8)))))

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
                                                                                (set_local $w15)

                                                                                (br $break))

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
                                                                            (set_local $w14)
                                                                            (i64.store offset=176 (get_local $ctx) (get_local $w14))

                                                                            (br $break))

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
                                                                        (set_local $w13)
                                                                        (i64.store offset=168 (get_local $ctx) (get_local $w13))

                                                                        (br $break))

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
                                                                    (set_local $w12)
                                                                    (i64.store offset=160 (get_local $ctx) (get_local $w12))

                                                                    (br $break))

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
                                                                (set_local $w11)
                                                                (i64.store offset=152 (get_local $ctx) (get_local $w11))

                                                                (br $break))

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
                                                            (set_local $w10)
                                                            (i64.store offset=144 (get_local $ctx) (get_local $w10))

                                                            (br $break))

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
                                                        (set_local $w9)
                                                        (i64.store offset=136 (get_local $ctx) (get_local $w9))

                                                        (br $break))

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
                                                    (set_local $w8)
                                                    (i64.store offset=128 (get_local $ctx) (get_local $w8))

                                                    (br $break))

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
                                                (set_local $w7)
                                                (i64.store offset=120 (get_local $ctx) (get_local $w7))

                                                (br $break))

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
                                            (set_local $w6)
                                            (i64.store offset=112 (get_local $ctx) (get_local $w6))

                                            (br $break))

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
                                        (set_local $w5)
                                        (i64.store offset=104 (get_local $ctx) (get_local $w5))

                                        (br $break))

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
                                    (set_local $w4)
                                    (i64.store offset=96 (get_local $ctx) (get_local $w4))

                                    (br $break))

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
                                (set_local $w3)
                                (i64.store offset=88 (get_local $ctx) (get_local $w3))

                                (br $break))

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
                            (set_local $w2)
                            (i64.store offset=80 (get_local $ctx) (get_local $w2))

                            (br $break))

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
                        (set_local $w1)
                        (i64.store offset=72 (get_local $ctx) (get_local $w1))

                        (br $break))

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
                    (set_local $w0)
                    (i64.store offset=64 (get_local $ctx) (get_local $w0))

                    (br $break))
            
            (set_local $ptr (i32.add (get_local $ptr) (i32.const 8)))
            (set_local $block_position (i32.add (get_local $block_position) (i32.const 8)))
            (br $start)))

    ;; TODO: store last word and leftover bytes
    (if (i32.ne (get_local $end_point) (get_local $input_end))
            (then
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
                (set_local $last_word)))

    ;;  store block position
    ;; (set_local $block_position (i32.add (get_local $block_position) (get_local $leftover)))
    (i32.store offset=184 (get_local $ctx) (get_local $block_position))

    ;;  store leftover bytes and return number of bytes modulo 8
    (i64.store (get_local $input) (i64.load (get_local $ptr)))

    (if (i32.eq (get_local $final) (i32.const 1))
        (then
            (i64.shl (i64.const 0x80) (i64.extend_u/i32 (i32.mul (i32.sub (i32.const 7) (get_local $leftover)) (i32.const 8))))
            (get_local $last_word)
            (i64.or)
            (set_local $last_word)
            ;; (call $i32.log (get_local $block_position))

            (block $break
                (block $0
                    (block $1
                        (block $2
                            (block $3
                                (block $4
                                    (block $5
                                        (block $6
                                            (block $7
                                                (block $8
                                                    (block $9
                                                        (block $10
                                                            (block $11
                                                                (block $12
                                                                    (block $13
                                                                        (block $14
                                                                            (block $15
                                                                                (block $switch
                                                                                    (br_table $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15
                                                                                        (i32.div_u (get_local $block_position) (i32.const 8)))))

                                                                                (set_local $w15 (get_local $last_word))
                                                                                (br $break))

                                                                            (set_local $w14 (get_local $last_word))
                                                                            (br $break))

                                                                        (set_local $w13 (get_local $last_word))
                                                                        (br $break))

                                                                    (set_local $w12 (get_local $last_word))
                                                                    (br $break))

                                                                (set_local $w11 (get_local $last_word))
                                                                (br $break))

                                                            (set_local $w10 (get_local $last_word))
                                                            (br $break))

                                                        (set_local $w9 (get_local $last_word))
                                                        (br $break))

                                                    (set_local $w8 (get_local $last_word))
                                                    (br $break))

                                                (set_local $w7 (get_local $last_word))
                                                (br $break))

                                            (set_local $w6 (get_local $last_word))
                                            (br $break))

                                        (set_local $w5 (get_local $last_word))
                                        (br $break))

                                    (set_local $w4 (get_local $last_word))
                                    (br $break))

                                (set_local $w3 (get_local $last_word))
                                (br $break))

                            (set_local $w2 (get_local $last_word))
                            (br $break))

                        (set_local $w1 (get_local $last_word))
                        (br $break))

                    (set_local $w0 (get_local $last_word))
                    (br $break))

            (set_local $block_position (i32.add (get_local $block_position) (i32.const 8)))
            (set_local $bytes_read (i32.add (get_local $bytes_read) (get_local $leftover)))

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
                                                                                    (br_table $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15
                                                                                        (i32.div_u (get_local $block_position) (i32.const 8)))))

                                                                                (set_local $w14 (i64.const 0)))
                                                                            (set_local $w15 (i64.const 0))

                                                                            ;; compress
                                                                            (set_local $w16 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w14) (i64.const 19)) (i64.rotr (get_local $w14) (i64.const 61))) (i64.shr_u (get_local $w14) (i64.const 6))) (get_local $w9)) (i64.xor (i64.xor (i64.rotr (get_local $w1) (i64.const 1)) (i64.rotr (get_local $w1) (i64.const 8))) (i64.shr_u (get_local $w1) (i64.const 7))) (get_local $w0))))
                                                                            (set_local $w17 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w15) (i64.const 19)) (i64.rotr (get_local $w15) (i64.const 61))) (i64.shr_u (get_local $w15) (i64.const 6))) (get_local $w10)) (i64.xor (i64.xor (i64.rotr (get_local $w2) (i64.const 1)) (i64.rotr (get_local $w2) (i64.const 8))) (i64.shr_u (get_local $w2) (i64.const 7))) (get_local $w1))))
                                                                            (set_local $w18 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w16) (i64.const 19)) (i64.rotr (get_local $w16) (i64.const 61))) (i64.shr_u (get_local $w16) (i64.const 6))) (get_local $w11)) (i64.xor (i64.xor (i64.rotr (get_local $w3) (i64.const 1)) (i64.rotr (get_local $w3) (i64.const 8))) (i64.shr_u (get_local $w3) (i64.const 7))) (get_local $w2))))
                                                                            (set_local $w19 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w17) (i64.const 19)) (i64.rotr (get_local $w17) (i64.const 61))) (i64.shr_u (get_local $w17) (i64.const 6))) (get_local $w12)) (i64.xor (i64.xor (i64.rotr (get_local $w4) (i64.const 1)) (i64.rotr (get_local $w4) (i64.const 8))) (i64.shr_u (get_local $w4) (i64.const 7))) (get_local $w3))))
                                                                            (set_local $w20 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w18) (i64.const 19)) (i64.rotr (get_local $w18) (i64.const 61))) (i64.shr_u (get_local $w18) (i64.const 6))) (get_local $w13)) (i64.xor (i64.xor (i64.rotr (get_local $w5) (i64.const 1)) (i64.rotr (get_local $w5) (i64.const 8))) (i64.shr_u (get_local $w5) (i64.const 7))) (get_local $w4))))
                                                                            (set_local $w21 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w19) (i64.const 19)) (i64.rotr (get_local $w19) (i64.const 61))) (i64.shr_u (get_local $w19) (i64.const 6))) (get_local $w14)) (i64.xor (i64.xor (i64.rotr (get_local $w6) (i64.const 1)) (i64.rotr (get_local $w6) (i64.const 8))) (i64.shr_u (get_local $w6) (i64.const 7))) (get_local $w5))))
                                                                            (set_local $w22 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w20) (i64.const 19)) (i64.rotr (get_local $w20) (i64.const 61))) (i64.shr_u (get_local $w20) (i64.const 6))) (get_local $w15)) (i64.xor (i64.xor (i64.rotr (get_local $w7) (i64.const 1)) (i64.rotr (get_local $w7) (i64.const 8))) (i64.shr_u (get_local $w7) (i64.const 7))) (get_local $w6))))
                                                                            (set_local $w23 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w21) (i64.const 19)) (i64.rotr (get_local $w21) (i64.const 61))) (i64.shr_u (get_local $w21) (i64.const 6))) (get_local $w16)) (i64.xor (i64.xor (i64.rotr (get_local $w8) (i64.const 1)) (i64.rotr (get_local $w8) (i64.const 8))) (i64.shr_u (get_local $w8) (i64.const 7))) (get_local $w7))))
                                                                            (set_local $w24 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w22) (i64.const 19)) (i64.rotr (get_local $w22) (i64.const 61))) (i64.shr_u (get_local $w22) (i64.const 6))) (get_local $w17)) (i64.xor (i64.xor (i64.rotr (get_local $w9) (i64.const 1)) (i64.rotr (get_local $w9) (i64.const 8))) (i64.shr_u (get_local $w9) (i64.const 7))) (get_local $w8))))
                                                                            (set_local $w25 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w23) (i64.const 19)) (i64.rotr (get_local $w23) (i64.const 61))) (i64.shr_u (get_local $w23) (i64.const 6))) (get_local $w18)) (i64.xor (i64.xor (i64.rotr (get_local $w10) (i64.const 1)) (i64.rotr (get_local $w10) (i64.const 8))) (i64.shr_u (get_local $w10) (i64.const 7))) (get_local $w9))))
                                                                            (set_local $w26 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w24) (i64.const 19)) (i64.rotr (get_local $w24) (i64.const 61))) (i64.shr_u (get_local $w24) (i64.const 6))) (get_local $w19)) (i64.xor (i64.xor (i64.rotr (get_local $w11) (i64.const 1)) (i64.rotr (get_local $w11) (i64.const 8))) (i64.shr_u (get_local $w11) (i64.const 7))) (get_local $w10))))
                                                                            (set_local $w27 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w25) (i64.const 19)) (i64.rotr (get_local $w25) (i64.const 61))) (i64.shr_u (get_local $w25) (i64.const 6))) (get_local $w20)) (i64.xor (i64.xor (i64.rotr (get_local $w12) (i64.const 1)) (i64.rotr (get_local $w12) (i64.const 8))) (i64.shr_u (get_local $w12) (i64.const 7))) (get_local $w11))))
                                                                            (set_local $w28 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w26) (i64.const 19)) (i64.rotr (get_local $w26) (i64.const 61))) (i64.shr_u (get_local $w26) (i64.const 6))) (get_local $w21)) (i64.xor (i64.xor (i64.rotr (get_local $w13) (i64.const 1)) (i64.rotr (get_local $w13) (i64.const 8))) (i64.shr_u (get_local $w13) (i64.const 7))) (get_local $w12))))
                                                                            (set_local $w29 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w27) (i64.const 19)) (i64.rotr (get_local $w27) (i64.const 61))) (i64.shr_u (get_local $w27) (i64.const 6))) (get_local $w22)) (i64.xor (i64.xor (i64.rotr (get_local $w14) (i64.const 1)) (i64.rotr (get_local $w14) (i64.const 8))) (i64.shr_u (get_local $w14) (i64.const 7))) (get_local $w13))))
                                                                            (set_local $w30 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w28) (i64.const 19)) (i64.rotr (get_local $w28) (i64.const 61))) (i64.shr_u (get_local $w28) (i64.const 6))) (get_local $w23)) (i64.xor (i64.xor (i64.rotr (get_local $w15) (i64.const 1)) (i64.rotr (get_local $w15) (i64.const 8))) (i64.shr_u (get_local $w15) (i64.const 7))) (get_local $w14))))
                                                                            (set_local $w31 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w29) (i64.const 19)) (i64.rotr (get_local $w29) (i64.const 61))) (i64.shr_u (get_local $w29) (i64.const 6))) (get_local $w24)) (i64.xor (i64.xor (i64.rotr (get_local $w16) (i64.const 1)) (i64.rotr (get_local $w16) (i64.const 8))) (i64.shr_u (get_local $w16) (i64.const 7))) (get_local $w15))))
                                                                            (set_local $w32 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w30) (i64.const 19)) (i64.rotr (get_local $w30) (i64.const 61))) (i64.shr_u (get_local $w30) (i64.const 6))) (get_local $w25)) (i64.xor (i64.xor (i64.rotr (get_local $w17) (i64.const 1)) (i64.rotr (get_local $w17) (i64.const 8))) (i64.shr_u (get_local $w17) (i64.const 7))) (get_local $w16))))
                                                                            (set_local $w33 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w31) (i64.const 19)) (i64.rotr (get_local $w31) (i64.const 61))) (i64.shr_u (get_local $w31) (i64.const 6))) (get_local $w26)) (i64.xor (i64.xor (i64.rotr (get_local $w18) (i64.const 1)) (i64.rotr (get_local $w18) (i64.const 8))) (i64.shr_u (get_local $w18) (i64.const 7))) (get_local $w17))))
                                                                            (set_local $w34 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w32) (i64.const 19)) (i64.rotr (get_local $w32) (i64.const 61))) (i64.shr_u (get_local $w32) (i64.const 6))) (get_local $w27)) (i64.xor (i64.xor (i64.rotr (get_local $w19) (i64.const 1)) (i64.rotr (get_local $w19) (i64.const 8))) (i64.shr_u (get_local $w19) (i64.const 7))) (get_local $w18))))
                                                                            (set_local $w35 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w33) (i64.const 19)) (i64.rotr (get_local $w33) (i64.const 61))) (i64.shr_u (get_local $w33) (i64.const 6))) (get_local $w28)) (i64.xor (i64.xor (i64.rotr (get_local $w20) (i64.const 1)) (i64.rotr (get_local $w20) (i64.const 8))) (i64.shr_u (get_local $w20) (i64.const 7))) (get_local $w19))))
                                                                            (set_local $w36 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w34) (i64.const 19)) (i64.rotr (get_local $w34) (i64.const 61))) (i64.shr_u (get_local $w34) (i64.const 6))) (get_local $w29)) (i64.xor (i64.xor (i64.rotr (get_local $w21) (i64.const 1)) (i64.rotr (get_local $w21) (i64.const 8))) (i64.shr_u (get_local $w21) (i64.const 7))) (get_local $w20))))
                                                                            (set_local $w37 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w35) (i64.const 19)) (i64.rotr (get_local $w35) (i64.const 61))) (i64.shr_u (get_local $w35) (i64.const 6))) (get_local $w30)) (i64.xor (i64.xor (i64.rotr (get_local $w22) (i64.const 1)) (i64.rotr (get_local $w22) (i64.const 8))) (i64.shr_u (get_local $w22) (i64.const 7))) (get_local $w21))))
                                                                            (set_local $w38 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w36) (i64.const 19)) (i64.rotr (get_local $w36) (i64.const 61))) (i64.shr_u (get_local $w36) (i64.const 6))) (get_local $w31)) (i64.xor (i64.xor (i64.rotr (get_local $w23) (i64.const 1)) (i64.rotr (get_local $w23) (i64.const 8))) (i64.shr_u (get_local $w23) (i64.const 7))) (get_local $w22))))
                                                                            (set_local $w39 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w37) (i64.const 19)) (i64.rotr (get_local $w37) (i64.const 61))) (i64.shr_u (get_local $w37) (i64.const 6))) (get_local $w32)) (i64.xor (i64.xor (i64.rotr (get_local $w24) (i64.const 1)) (i64.rotr (get_local $w24) (i64.const 8))) (i64.shr_u (get_local $w24) (i64.const 7))) (get_local $w23))))
                                                                            (set_local $w40 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w38) (i64.const 19)) (i64.rotr (get_local $w38) (i64.const 61))) (i64.shr_u (get_local $w38) (i64.const 6))) (get_local $w33)) (i64.xor (i64.xor (i64.rotr (get_local $w25) (i64.const 1)) (i64.rotr (get_local $w25) (i64.const 8))) (i64.shr_u (get_local $w25) (i64.const 7))) (get_local $w24))))
                                                                            (set_local $w41 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w39) (i64.const 19)) (i64.rotr (get_local $w39) (i64.const 61))) (i64.shr_u (get_local $w39) (i64.const 6))) (get_local $w34)) (i64.xor (i64.xor (i64.rotr (get_local $w26) (i64.const 1)) (i64.rotr (get_local $w26) (i64.const 8))) (i64.shr_u (get_local $w26) (i64.const 7))) (get_local $w25))))
                                                                            (set_local $w42 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w40) (i64.const 19)) (i64.rotr (get_local $w40) (i64.const 61))) (i64.shr_u (get_local $w40) (i64.const 6))) (get_local $w35)) (i64.xor (i64.xor (i64.rotr (get_local $w27) (i64.const 1)) (i64.rotr (get_local $w27) (i64.const 8))) (i64.shr_u (get_local $w27) (i64.const 7))) (get_local $w26))))
                                                                            (set_local $w43 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w41) (i64.const 19)) (i64.rotr (get_local $w41) (i64.const 61))) (i64.shr_u (get_local $w41) (i64.const 6))) (get_local $w36)) (i64.xor (i64.xor (i64.rotr (get_local $w28) (i64.const 1)) (i64.rotr (get_local $w28) (i64.const 8))) (i64.shr_u (get_local $w28) (i64.const 7))) (get_local $w27))))
                                                                            (set_local $w44 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w42) (i64.const 19)) (i64.rotr (get_local $w42) (i64.const 61))) (i64.shr_u (get_local $w42) (i64.const 6))) (get_local $w37)) (i64.xor (i64.xor (i64.rotr (get_local $w29) (i64.const 1)) (i64.rotr (get_local $w29) (i64.const 8))) (i64.shr_u (get_local $w29) (i64.const 7))) (get_local $w28))))
                                                                            (set_local $w45 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w43) (i64.const 19)) (i64.rotr (get_local $w43) (i64.const 61))) (i64.shr_u (get_local $w43) (i64.const 6))) (get_local $w38)) (i64.xor (i64.xor (i64.rotr (get_local $w30) (i64.const 1)) (i64.rotr (get_local $w30) (i64.const 8))) (i64.shr_u (get_local $w30) (i64.const 7))) (get_local $w29))))
                                                                            (set_local $w46 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w44) (i64.const 19)) (i64.rotr (get_local $w44) (i64.const 61))) (i64.shr_u (get_local $w44) (i64.const 6))) (get_local $w39)) (i64.xor (i64.xor (i64.rotr (get_local $w31) (i64.const 1)) (i64.rotr (get_local $w31) (i64.const 8))) (i64.shr_u (get_local $w31) (i64.const 7))) (get_local $w30))))
                                                                            (set_local $w47 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w45) (i64.const 19)) (i64.rotr (get_local $w45) (i64.const 61))) (i64.shr_u (get_local $w45) (i64.const 6))) (get_local $w40)) (i64.xor (i64.xor (i64.rotr (get_local $w32) (i64.const 1)) (i64.rotr (get_local $w32) (i64.const 8))) (i64.shr_u (get_local $w32) (i64.const 7))) (get_local $w31))))
                                                                            (set_local $w48 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w46) (i64.const 19)) (i64.rotr (get_local $w46) (i64.const 61))) (i64.shr_u (get_local $w46) (i64.const 6))) (get_local $w41)) (i64.xor (i64.xor (i64.rotr (get_local $w33) (i64.const 1)) (i64.rotr (get_local $w33) (i64.const 8))) (i64.shr_u (get_local $w33) (i64.const 7))) (get_local $w32))))
                                                                            (set_local $w49 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w47) (i64.const 19)) (i64.rotr (get_local $w47) (i64.const 61))) (i64.shr_u (get_local $w47) (i64.const 6))) (get_local $w42)) (i64.xor (i64.xor (i64.rotr (get_local $w34) (i64.const 1)) (i64.rotr (get_local $w34) (i64.const 8))) (i64.shr_u (get_local $w34) (i64.const 7))) (get_local $w33))))
                                                                            (set_local $w50 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w48) (i64.const 19)) (i64.rotr (get_local $w48) (i64.const 61))) (i64.shr_u (get_local $w48) (i64.const 6))) (get_local $w43)) (i64.xor (i64.xor (i64.rotr (get_local $w35) (i64.const 1)) (i64.rotr (get_local $w35) (i64.const 8))) (i64.shr_u (get_local $w35) (i64.const 7))) (get_local $w34))))
                                                                            (set_local $w51 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w49) (i64.const 19)) (i64.rotr (get_local $w49) (i64.const 61))) (i64.shr_u (get_local $w49) (i64.const 6))) (get_local $w44)) (i64.xor (i64.xor (i64.rotr (get_local $w36) (i64.const 1)) (i64.rotr (get_local $w36) (i64.const 8))) (i64.shr_u (get_local $w36) (i64.const 7))) (get_local $w35))))
                                                                            (set_local $w52 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w50) (i64.const 19)) (i64.rotr (get_local $w50) (i64.const 61))) (i64.shr_u (get_local $w50) (i64.const 6))) (get_local $w45)) (i64.xor (i64.xor (i64.rotr (get_local $w37) (i64.const 1)) (i64.rotr (get_local $w37) (i64.const 8))) (i64.shr_u (get_local $w37) (i64.const 7))) (get_local $w36))))
                                                                            (set_local $w53 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w51) (i64.const 19)) (i64.rotr (get_local $w51) (i64.const 61))) (i64.shr_u (get_local $w51) (i64.const 6))) (get_local $w46)) (i64.xor (i64.xor (i64.rotr (get_local $w38) (i64.const 1)) (i64.rotr (get_local $w38) (i64.const 8))) (i64.shr_u (get_local $w38) (i64.const 7))) (get_local $w37))))
                                                                            (set_local $w54 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w52) (i64.const 19)) (i64.rotr (get_local $w52) (i64.const 61))) (i64.shr_u (get_local $w52) (i64.const 6))) (get_local $w47)) (i64.xor (i64.xor (i64.rotr (get_local $w39) (i64.const 1)) (i64.rotr (get_local $w39) (i64.const 8))) (i64.shr_u (get_local $w39) (i64.const 7))) (get_local $w38))))
                                                                            (set_local $w55 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w53) (i64.const 19)) (i64.rotr (get_local $w53) (i64.const 61))) (i64.shr_u (get_local $w53) (i64.const 6))) (get_local $w48)) (i64.xor (i64.xor (i64.rotr (get_local $w40) (i64.const 1)) (i64.rotr (get_local $w40) (i64.const 8))) (i64.shr_u (get_local $w40) (i64.const 7))) (get_local $w39))))
                                                                            (set_local $w56 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w54) (i64.const 19)) (i64.rotr (get_local $w54) (i64.const 61))) (i64.shr_u (get_local $w54) (i64.const 6))) (get_local $w49)) (i64.xor (i64.xor (i64.rotr (get_local $w41) (i64.const 1)) (i64.rotr (get_local $w41) (i64.const 8))) (i64.shr_u (get_local $w41) (i64.const 7))) (get_local $w40))))
                                                                            (set_local $w57 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w55) (i64.const 19)) (i64.rotr (get_local $w55) (i64.const 61))) (i64.shr_u (get_local $w55) (i64.const 6))) (get_local $w50)) (i64.xor (i64.xor (i64.rotr (get_local $w42) (i64.const 1)) (i64.rotr (get_local $w42) (i64.const 8))) (i64.shr_u (get_local $w42) (i64.const 7))) (get_local $w41))))
                                                                            (set_local $w58 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w56) (i64.const 19)) (i64.rotr (get_local $w56) (i64.const 61))) (i64.shr_u (get_local $w56) (i64.const 6))) (get_local $w51)) (i64.xor (i64.xor (i64.rotr (get_local $w43) (i64.const 1)) (i64.rotr (get_local $w43) (i64.const 8))) (i64.shr_u (get_local $w43) (i64.const 7))) (get_local $w42))))
                                                                            (set_local $w59 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w57) (i64.const 19)) (i64.rotr (get_local $w57) (i64.const 61))) (i64.shr_u (get_local $w57) (i64.const 6))) (get_local $w52)) (i64.xor (i64.xor (i64.rotr (get_local $w44) (i64.const 1)) (i64.rotr (get_local $w44) (i64.const 8))) (i64.shr_u (get_local $w44) (i64.const 7))) (get_local $w43))))
                                                                            (set_local $w60 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w58) (i64.const 19)) (i64.rotr (get_local $w58) (i64.const 61))) (i64.shr_u (get_local $w58) (i64.const 6))) (get_local $w53)) (i64.xor (i64.xor (i64.rotr (get_local $w45) (i64.const 1)) (i64.rotr (get_local $w45) (i64.const 8))) (i64.shr_u (get_local $w45) (i64.const 7))) (get_local $w44))))
                                                                            (set_local $w61 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w59) (i64.const 19)) (i64.rotr (get_local $w59) (i64.const 61))) (i64.shr_u (get_local $w59) (i64.const 6))) (get_local $w54)) (i64.xor (i64.xor (i64.rotr (get_local $w46) (i64.const 1)) (i64.rotr (get_local $w46) (i64.const 8))) (i64.shr_u (get_local $w46) (i64.const 7))) (get_local $w45))))
                                                                            (set_local $w62 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w60) (i64.const 19)) (i64.rotr (get_local $w60) (i64.const 61))) (i64.shr_u (get_local $w60) (i64.const 6))) (get_local $w55)) (i64.xor (i64.xor (i64.rotr (get_local $w47) (i64.const 1)) (i64.rotr (get_local $w47) (i64.const 8))) (i64.shr_u (get_local $w47) (i64.const 7))) (get_local $w46))))
                                                                            (set_local $w63 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w61) (i64.const 19)) (i64.rotr (get_local $w61) (i64.const 61))) (i64.shr_u (get_local $w61) (i64.const 6))) (get_local $w56)) (i64.xor (i64.xor (i64.rotr (get_local $w48) (i64.const 1)) (i64.rotr (get_local $w48) (i64.const 8))) (i64.shr_u (get_local $w48) (i64.const 7))) (get_local $w47))))
                                                                            (set_local $w64 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w62) (i64.const 19)) (i64.rotr (get_local $w62) (i64.const 61))) (i64.shr_u (get_local $w62) (i64.const 6))) (get_local $w57)) (i64.xor (i64.xor (i64.rotr (get_local $w49) (i64.const 1)) (i64.rotr (get_local $w49) (i64.const 8))) (i64.shr_u (get_local $w49) (i64.const 7))) (get_local $w48))))
                                                                            (set_local $w65 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w63) (i64.const 19)) (i64.rotr (get_local $w63) (i64.const 61))) (i64.shr_u (get_local $w63) (i64.const 6))) (get_local $w58)) (i64.xor (i64.xor (i64.rotr (get_local $w50) (i64.const 1)) (i64.rotr (get_local $w50) (i64.const 8))) (i64.shr_u (get_local $w50) (i64.const 7))) (get_local $w49))))
                                                                            (set_local $w66 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w64) (i64.const 19)) (i64.rotr (get_local $w64) (i64.const 61))) (i64.shr_u (get_local $w64) (i64.const 6))) (get_local $w59)) (i64.xor (i64.xor (i64.rotr (get_local $w51) (i64.const 1)) (i64.rotr (get_local $w51) (i64.const 8))) (i64.shr_u (get_local $w51) (i64.const 7))) (get_local $w50))))
                                                                            (set_local $w67 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w65) (i64.const 19)) (i64.rotr (get_local $w65) (i64.const 61))) (i64.shr_u (get_local $w65) (i64.const 6))) (get_local $w60)) (i64.xor (i64.xor (i64.rotr (get_local $w52) (i64.const 1)) (i64.rotr (get_local $w52) (i64.const 8))) (i64.shr_u (get_local $w52) (i64.const 7))) (get_local $w51))))
                                                                            (set_local $w68 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w66) (i64.const 19)) (i64.rotr (get_local $w66) (i64.const 61))) (i64.shr_u (get_local $w66) (i64.const 6))) (get_local $w61)) (i64.xor (i64.xor (i64.rotr (get_local $w53) (i64.const 1)) (i64.rotr (get_local $w53) (i64.const 8))) (i64.shr_u (get_local $w53) (i64.const 7))) (get_local $w52))))
                                                                            (set_local $w69 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w67) (i64.const 19)) (i64.rotr (get_local $w67) (i64.const 61))) (i64.shr_u (get_local $w67) (i64.const 6))) (get_local $w62)) (i64.xor (i64.xor (i64.rotr (get_local $w54) (i64.const 1)) (i64.rotr (get_local $w54) (i64.const 8))) (i64.shr_u (get_local $w54) (i64.const 7))) (get_local $w53))))
                                                                            (set_local $w70 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w68) (i64.const 19)) (i64.rotr (get_local $w68) (i64.const 61))) (i64.shr_u (get_local $w68) (i64.const 6))) (get_local $w63)) (i64.xor (i64.xor (i64.rotr (get_local $w55) (i64.const 1)) (i64.rotr (get_local $w55) (i64.const 8))) (i64.shr_u (get_local $w55) (i64.const 7))) (get_local $w54))))
                                                                            (set_local $w71 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w69) (i64.const 19)) (i64.rotr (get_local $w69) (i64.const 61))) (i64.shr_u (get_local $w69) (i64.const 6))) (get_local $w64)) (i64.xor (i64.xor (i64.rotr (get_local $w56) (i64.const 1)) (i64.rotr (get_local $w56) (i64.const 8))) (i64.shr_u (get_local $w56) (i64.const 7))) (get_local $w55))))
                                                                            (set_local $w72 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w70) (i64.const 19)) (i64.rotr (get_local $w70) (i64.const 61))) (i64.shr_u (get_local $w70) (i64.const 6))) (get_local $w65)) (i64.xor (i64.xor (i64.rotr (get_local $w57) (i64.const 1)) (i64.rotr (get_local $w57) (i64.const 8))) (i64.shr_u (get_local $w57) (i64.const 7))) (get_local $w56))))
                                                                            (set_local $w73 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w71) (i64.const 19)) (i64.rotr (get_local $w71) (i64.const 61))) (i64.shr_u (get_local $w71) (i64.const 6))) (get_local $w66)) (i64.xor (i64.xor (i64.rotr (get_local $w58) (i64.const 1)) (i64.rotr (get_local $w58) (i64.const 8))) (i64.shr_u (get_local $w58) (i64.const 7))) (get_local $w57))))
                                                                            (set_local $w74 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w72) (i64.const 19)) (i64.rotr (get_local $w72) (i64.const 61))) (i64.shr_u (get_local $w72) (i64.const 6))) (get_local $w67)) (i64.xor (i64.xor (i64.rotr (get_local $w59) (i64.const 1)) (i64.rotr (get_local $w59) (i64.const 8))) (i64.shr_u (get_local $w59) (i64.const 7))) (get_local $w58))))
                                                                            (set_local $w75 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w73) (i64.const 19)) (i64.rotr (get_local $w73) (i64.const 61))) (i64.shr_u (get_local $w73) (i64.const 6))) (get_local $w68)) (i64.xor (i64.xor (i64.rotr (get_local $w60) (i64.const 1)) (i64.rotr (get_local $w60) (i64.const 8))) (i64.shr_u (get_local $w60) (i64.const 7))) (get_local $w59))))
                                                                            (set_local $w76 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w74) (i64.const 19)) (i64.rotr (get_local $w74) (i64.const 61))) (i64.shr_u (get_local $w74) (i64.const 6))) (get_local $w69)) (i64.xor (i64.xor (i64.rotr (get_local $w61) (i64.const 1)) (i64.rotr (get_local $w61) (i64.const 8))) (i64.shr_u (get_local $w61) (i64.const 7))) (get_local $w60))))
                                                                            (set_local $w77 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w75) (i64.const 19)) (i64.rotr (get_local $w75) (i64.const 61))) (i64.shr_u (get_local $w75) (i64.const 6))) (get_local $w70)) (i64.xor (i64.xor (i64.rotr (get_local $w62) (i64.const 1)) (i64.rotr (get_local $w62) (i64.const 8))) (i64.shr_u (get_local $w62) (i64.const 7))) (get_local $w61))))
                                                                            (set_local $w78 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w76) (i64.const 19)) (i64.rotr (get_local $w76) (i64.const 61))) (i64.shr_u (get_local $w76) (i64.const 6))) (get_local $w71)) (i64.xor (i64.xor (i64.rotr (get_local $w63) (i64.const 1)) (i64.rotr (get_local $w63) (i64.const 8))) (i64.shr_u (get_local $w63) (i64.const 7))) (get_local $w62))))
                                                                            (set_local $w79 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w77) (i64.const 19)) (i64.rotr (get_local $w77) (i64.const 61))) (i64.shr_u (get_local $w77) (i64.const 6))) (get_local $w72)) (i64.xor (i64.xor (i64.rotr (get_local $w64) (i64.const 1)) (i64.rotr (get_local $w64) (i64.const 8))) (i64.shr_u (get_local $w64) (i64.const 7))) (get_local $w63))))


                                                                            ;; load previous hash state into registers
                                                                            (set_local $a (i64.load offset=0 (i32.const 0)))
                                                                            (set_local $b (i64.load offset=8 (i32.const 0)))
                                                                            (set_local $c (i64.load offset=16 (i32.const 0)))
                                                                            (set_local $d (i64.load offset=24 (i32.const 0)))
                                                                            (set_local $e (i64.load offset=32 (i32.const 0)))
                                                                            (set_local $f (i64.load offset=40 (i32.const 0)))
                                                                            (set_local $g (i64.load offset=48 (i32.const 0)))
                                                                            (set_local $h (i64.load offset=56 (i32.const 0)))

                                                                            ;; ROUND 0

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K0 + W0
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w0)) (get_local $k0 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 1

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K1 + W1
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w1)) (get_local $k1 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 2

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K2 + W2
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w2)) (get_local $k2 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 3

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K3 + W3
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w3)) (get_local $k3 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 4

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K4 + W4
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w4)) (get_local $k4 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 5

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K5 + W5
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w5)) (get_local $k5 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 6

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K6 + W6
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w6)) (get_local $k6 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 7

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K7 + W7
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w7)) (get_local $k7 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 8

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K8 + W8
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w8)) (get_local $k8 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 9

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K9 + W9
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w9)) (get_local $k9 )))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 10

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K10 + W10
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w10)) (get_local $k10)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 11

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K11 + W11
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w11)) (get_local $k11)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 12

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K12 + W12
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w12)) (get_local $k12)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 13

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K13 + W13
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w13)) (get_local $k13)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 14

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K14 + W14
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w14)) (get_local $k14)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 15

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K15 + W15
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w15)) (get_local $k15)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 16

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K16 + W16
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w16)) (get_local $k16)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 17

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K17 + W17
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w17)) (get_local $k17)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 18

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K18 + W18
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w18)) (get_local $k18)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 19

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K19 + W19
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w19)) (get_local $k19)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 20

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K20 + W20
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w20)) (get_local $k20)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 21

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K21 + W21
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w21)) (get_local $k21)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 22

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K22 + W22
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w22)) (get_local $k22)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 23

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K23 + W23
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w23)) (get_local $k23)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 24

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K24 + W24
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w24)) (get_local $k24)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 25

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K25 + W25
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w25)) (get_local $k25)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 26

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K26 + W26
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w26)) (get_local $k26)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 27

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K27 + W27
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w27)) (get_local $k27)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 28

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K28 + W28
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w28)) (get_local $k28)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 29

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K29 + W29
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w29)) (get_local $k29)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 30

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K30 + W30
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w30)) (get_local $k30)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 31

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K31 + W31
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w31)) (get_local $k31)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 32

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K32 + W32
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w32)) (get_local $k32)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 33

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K33 + W33
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w33)) (get_local $k33)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 34

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K34 + W34
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w34)) (get_local $k34)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 35

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K35 + W35
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w35)) (get_local $k35)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 36

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K36 + W36
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w36)) (get_local $k36)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 37

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K37 + W37
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w37)) (get_local $k37)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 38

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K38 + W38
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w38)) (get_local $k38)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 39

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K39 + W39
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w39)) (get_local $k39)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 40

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K40 + W40
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w40)) (get_local $k40)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 41

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K41 + W41
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w41)) (get_local $k41)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 42

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K42 + W42
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w42)) (get_local $k42)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 43

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K43 + W43
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w43)) (get_local $k43)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 44

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K44 + W44
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w44)) (get_local $k44)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 45

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K45 + W45
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w45)) (get_local $k45)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 46

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K46 + W46
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w46)) (get_local $k46)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 47

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K47 + W47
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w47)) (get_local $k47)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 48

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K48 + W48
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w48)) (get_local $k48)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 49

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K49 + W49
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w49)) (get_local $k49)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 50

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K50 + W50
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w50)) (get_local $k50)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 51

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K51 + W51
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w51)) (get_local $k51)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 52

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K52 + W52
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w52)) (get_local $k52)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 53

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K53 + W53
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w53)) (get_local $k53)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 54

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K54 + W54
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w54)) (get_local $k54)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 55

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K55 + W55
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w55)) (get_local $k55)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 56

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K56 + W56
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w56)) (get_local $k56)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 57

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K57 + W57
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w57)) (get_local $k57)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 58

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K58 + W58
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w58)) (get_local $k58)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 59

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K59 + W59
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w59)) (get_local $k59)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 60

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K60 + W60
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w60)) (get_local $k60)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 61

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K61 + W61
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w61)) (get_local $k61)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 62

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K62 + W62
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w62)) (get_local $k62)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 63

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K63 + W63
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w63)) (get_local $k63)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 64

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K64 + W64
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w64)) (get_local $k64)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 65

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K65 + W65
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w65)) (get_local $k65)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 66

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K66 + W66
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w66)) (get_local $k66)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 67

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K67 + W67
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w67)) (get_local $k67)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 68

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K68 + W68
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w68)) (get_local $k68)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 69

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K69 + W69
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w69)) (get_local $k69)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 70

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K70 + W70
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w70)) (get_local $k70)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 71

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K71 + W71
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w71)) (get_local $k71)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 72

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K72 + W72
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w72)) (get_local $k72)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 73

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K73 + W73
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w73)) (get_local $k73)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 74

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K74 + W74
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w74)) (get_local $k74)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 75

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K75 + W75
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w75)) (get_local $k75)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 76

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K76 + W76
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w76)) (get_local $k76)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 77

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K77 + W77
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w77)) (get_local $k77)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 78

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K78 + W78
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w78)) (get_local $k78)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

                                                                            ;; ROUND 79

                                                                            ;; precompute intermediate values

                                                                            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K79 + W79
                                                                            ;; T2 = big_sig0(a) + Maj(a, b, c)

                                                                            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
                                                                            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

                                                                            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
                                                                            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

                                                                            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w79)) (get_local $k79)))
                                                                            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

                                                                            ;; update registers

                                                                            ;; h <- g
                                                                            (set_local $h (get_local $g))

                                                                            ;; g <- f
                                                                            (set_local $g (get_local $f))  

                                                                            ;; f <- e
                                                                            (set_local $f (get_local $e))  

                                                                            ;; e <- d + T1
                                                                            (set_local $e (i64.add (get_local $d) (get_local $T1)))

                                                                            ;; d <- c
                                                                            (set_local $d (get_local $c))  

                                                                            ;; c <- b
                                                                            (set_local $c (get_local $b))  

                                                                            ;; b <- a
                                                                            (set_local $b (get_local $a))  

                                                                            ;; a <- T1 + T2
                                                                            (set_local $a (i64.add (get_local $T1) (get_local $T2))))

                                                                        (set_local $w0 (i64.const 0)))
                                                                    (set_local $w1 (i64.const 0)))
                                                                (set_local $w2 (i64.const 0)))
                                                            (set_local $w3 (i64.const 0)))
                                                        (set_local $w4 (i64.const 0)))
                                                    (set_local $w5 (i64.const 0)))
                                                (set_local $w6 (i64.const 0)))
                                            (set_local $w7 (i64.const 0)))
                                        (set_local $w8 (i64.const 0)))
                                    (set_local $w9 (i64.const 0)))
                                (set_local $w10 (i64.const 0)))
                            (set_local $w11 (i64.const 0)))
                        (set_local $w12 (i64.const 0)))
                    (set_local $w13 (i64.const 0)))

            (set_local $w14 (i64.const 0))
            (set_local $w15 (i64.extend_u/i32 (i32.mul (get_local $bytes_read) (i32.const 8))))

            (set_local $w16 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w14) (i64.const 19)) (i64.rotr (get_local $w14) (i64.const 61))) (i64.shr_u (get_local $w14) (i64.const 6))) (get_local $w9)) (i64.xor (i64.xor (i64.rotr (get_local $w1) (i64.const 1)) (i64.rotr (get_local $w1) (i64.const 8))) (i64.shr_u (get_local $w1) (i64.const 7))) (get_local $w0))))
            (set_local $w17 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w15) (i64.const 19)) (i64.rotr (get_local $w15) (i64.const 61))) (i64.shr_u (get_local $w15) (i64.const 6))) (get_local $w10)) (i64.xor (i64.xor (i64.rotr (get_local $w2) (i64.const 1)) (i64.rotr (get_local $w2) (i64.const 8))) (i64.shr_u (get_local $w2) (i64.const 7))) (get_local $w1))))
            (set_local $w18 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w16) (i64.const 19)) (i64.rotr (get_local $w16) (i64.const 61))) (i64.shr_u (get_local $w16) (i64.const 6))) (get_local $w11)) (i64.xor (i64.xor (i64.rotr (get_local $w3) (i64.const 1)) (i64.rotr (get_local $w3) (i64.const 8))) (i64.shr_u (get_local $w3) (i64.const 7))) (get_local $w2))))
            (set_local $w19 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w17) (i64.const 19)) (i64.rotr (get_local $w17) (i64.const 61))) (i64.shr_u (get_local $w17) (i64.const 6))) (get_local $w12)) (i64.xor (i64.xor (i64.rotr (get_local $w4) (i64.const 1)) (i64.rotr (get_local $w4) (i64.const 8))) (i64.shr_u (get_local $w4) (i64.const 7))) (get_local $w3))))
            (set_local $w20 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w18) (i64.const 19)) (i64.rotr (get_local $w18) (i64.const 61))) (i64.shr_u (get_local $w18) (i64.const 6))) (get_local $w13)) (i64.xor (i64.xor (i64.rotr (get_local $w5) (i64.const 1)) (i64.rotr (get_local $w5) (i64.const 8))) (i64.shr_u (get_local $w5) (i64.const 7))) (get_local $w4))))
            (set_local $w21 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w19) (i64.const 19)) (i64.rotr (get_local $w19) (i64.const 61))) (i64.shr_u (get_local $w19) (i64.const 6))) (get_local $w14)) (i64.xor (i64.xor (i64.rotr (get_local $w6) (i64.const 1)) (i64.rotr (get_local $w6) (i64.const 8))) (i64.shr_u (get_local $w6) (i64.const 7))) (get_local $w5))))
            (set_local $w22 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w20) (i64.const 19)) (i64.rotr (get_local $w20) (i64.const 61))) (i64.shr_u (get_local $w20) (i64.const 6))) (get_local $w15)) (i64.xor (i64.xor (i64.rotr (get_local $w7) (i64.const 1)) (i64.rotr (get_local $w7) (i64.const 8))) (i64.shr_u (get_local $w7) (i64.const 7))) (get_local $w6))))
            (set_local $w23 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w21) (i64.const 19)) (i64.rotr (get_local $w21) (i64.const 61))) (i64.shr_u (get_local $w21) (i64.const 6))) (get_local $w16)) (i64.xor (i64.xor (i64.rotr (get_local $w8) (i64.const 1)) (i64.rotr (get_local $w8) (i64.const 8))) (i64.shr_u (get_local $w8) (i64.const 7))) (get_local $w7))))
            (set_local $w24 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w22) (i64.const 19)) (i64.rotr (get_local $w22) (i64.const 61))) (i64.shr_u (get_local $w22) (i64.const 6))) (get_local $w17)) (i64.xor (i64.xor (i64.rotr (get_local $w9) (i64.const 1)) (i64.rotr (get_local $w9) (i64.const 8))) (i64.shr_u (get_local $w9) (i64.const 7))) (get_local $w8))))
            (set_local $w25 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w23) (i64.const 19)) (i64.rotr (get_local $w23) (i64.const 61))) (i64.shr_u (get_local $w23) (i64.const 6))) (get_local $w18)) (i64.xor (i64.xor (i64.rotr (get_local $w10) (i64.const 1)) (i64.rotr (get_local $w10) (i64.const 8))) (i64.shr_u (get_local $w10) (i64.const 7))) (get_local $w9))))
            (set_local $w26 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w24) (i64.const 19)) (i64.rotr (get_local $w24) (i64.const 61))) (i64.shr_u (get_local $w24) (i64.const 6))) (get_local $w19)) (i64.xor (i64.xor (i64.rotr (get_local $w11) (i64.const 1)) (i64.rotr (get_local $w11) (i64.const 8))) (i64.shr_u (get_local $w11) (i64.const 7))) (get_local $w10))))
            (set_local $w27 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w25) (i64.const 19)) (i64.rotr (get_local $w25) (i64.const 61))) (i64.shr_u (get_local $w25) (i64.const 6))) (get_local $w20)) (i64.xor (i64.xor (i64.rotr (get_local $w12) (i64.const 1)) (i64.rotr (get_local $w12) (i64.const 8))) (i64.shr_u (get_local $w12) (i64.const 7))) (get_local $w11))))
            (set_local $w28 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w26) (i64.const 19)) (i64.rotr (get_local $w26) (i64.const 61))) (i64.shr_u (get_local $w26) (i64.const 6))) (get_local $w21)) (i64.xor (i64.xor (i64.rotr (get_local $w13) (i64.const 1)) (i64.rotr (get_local $w13) (i64.const 8))) (i64.shr_u (get_local $w13) (i64.const 7))) (get_local $w12))))
            (set_local $w29 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w27) (i64.const 19)) (i64.rotr (get_local $w27) (i64.const 61))) (i64.shr_u (get_local $w27) (i64.const 6))) (get_local $w22)) (i64.xor (i64.xor (i64.rotr (get_local $w14) (i64.const 1)) (i64.rotr (get_local $w14) (i64.const 8))) (i64.shr_u (get_local $w14) (i64.const 7))) (get_local $w13))))
            (set_local $w30 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w28) (i64.const 19)) (i64.rotr (get_local $w28) (i64.const 61))) (i64.shr_u (get_local $w28) (i64.const 6))) (get_local $w23)) (i64.xor (i64.xor (i64.rotr (get_local $w15) (i64.const 1)) (i64.rotr (get_local $w15) (i64.const 8))) (i64.shr_u (get_local $w15) (i64.const 7))) (get_local $w14))))
            (set_local $w31 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w29) (i64.const 19)) (i64.rotr (get_local $w29) (i64.const 61))) (i64.shr_u (get_local $w29) (i64.const 6))) (get_local $w24)) (i64.xor (i64.xor (i64.rotr (get_local $w16) (i64.const 1)) (i64.rotr (get_local $w16) (i64.const 8))) (i64.shr_u (get_local $w16) (i64.const 7))) (get_local $w15))))
            (set_local $w32 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w30) (i64.const 19)) (i64.rotr (get_local $w30) (i64.const 61))) (i64.shr_u (get_local $w30) (i64.const 6))) (get_local $w25)) (i64.xor (i64.xor (i64.rotr (get_local $w17) (i64.const 1)) (i64.rotr (get_local $w17) (i64.const 8))) (i64.shr_u (get_local $w17) (i64.const 7))) (get_local $w16))))
            (set_local $w33 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w31) (i64.const 19)) (i64.rotr (get_local $w31) (i64.const 61))) (i64.shr_u (get_local $w31) (i64.const 6))) (get_local $w26)) (i64.xor (i64.xor (i64.rotr (get_local $w18) (i64.const 1)) (i64.rotr (get_local $w18) (i64.const 8))) (i64.shr_u (get_local $w18) (i64.const 7))) (get_local $w17))))
            (set_local $w34 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w32) (i64.const 19)) (i64.rotr (get_local $w32) (i64.const 61))) (i64.shr_u (get_local $w32) (i64.const 6))) (get_local $w27)) (i64.xor (i64.xor (i64.rotr (get_local $w19) (i64.const 1)) (i64.rotr (get_local $w19) (i64.const 8))) (i64.shr_u (get_local $w19) (i64.const 7))) (get_local $w18))))
            (set_local $w35 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w33) (i64.const 19)) (i64.rotr (get_local $w33) (i64.const 61))) (i64.shr_u (get_local $w33) (i64.const 6))) (get_local $w28)) (i64.xor (i64.xor (i64.rotr (get_local $w20) (i64.const 1)) (i64.rotr (get_local $w20) (i64.const 8))) (i64.shr_u (get_local $w20) (i64.const 7))) (get_local $w19))))
            (set_local $w36 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w34) (i64.const 19)) (i64.rotr (get_local $w34) (i64.const 61))) (i64.shr_u (get_local $w34) (i64.const 6))) (get_local $w29)) (i64.xor (i64.xor (i64.rotr (get_local $w21) (i64.const 1)) (i64.rotr (get_local $w21) (i64.const 8))) (i64.shr_u (get_local $w21) (i64.const 7))) (get_local $w20))))
            (set_local $w37 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w35) (i64.const 19)) (i64.rotr (get_local $w35) (i64.const 61))) (i64.shr_u (get_local $w35) (i64.const 6))) (get_local $w30)) (i64.xor (i64.xor (i64.rotr (get_local $w22) (i64.const 1)) (i64.rotr (get_local $w22) (i64.const 8))) (i64.shr_u (get_local $w22) (i64.const 7))) (get_local $w21))))
            (set_local $w38 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w36) (i64.const 19)) (i64.rotr (get_local $w36) (i64.const 61))) (i64.shr_u (get_local $w36) (i64.const 6))) (get_local $w31)) (i64.xor (i64.xor (i64.rotr (get_local $w23) (i64.const 1)) (i64.rotr (get_local $w23) (i64.const 8))) (i64.shr_u (get_local $w23) (i64.const 7))) (get_local $w22))))
            (set_local $w39 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w37) (i64.const 19)) (i64.rotr (get_local $w37) (i64.const 61))) (i64.shr_u (get_local $w37) (i64.const 6))) (get_local $w32)) (i64.xor (i64.xor (i64.rotr (get_local $w24) (i64.const 1)) (i64.rotr (get_local $w24) (i64.const 8))) (i64.shr_u (get_local $w24) (i64.const 7))) (get_local $w23))))
            (set_local $w40 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w38) (i64.const 19)) (i64.rotr (get_local $w38) (i64.const 61))) (i64.shr_u (get_local $w38) (i64.const 6))) (get_local $w33)) (i64.xor (i64.xor (i64.rotr (get_local $w25) (i64.const 1)) (i64.rotr (get_local $w25) (i64.const 8))) (i64.shr_u (get_local $w25) (i64.const 7))) (get_local $w24))))
            (set_local $w41 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w39) (i64.const 19)) (i64.rotr (get_local $w39) (i64.const 61))) (i64.shr_u (get_local $w39) (i64.const 6))) (get_local $w34)) (i64.xor (i64.xor (i64.rotr (get_local $w26) (i64.const 1)) (i64.rotr (get_local $w26) (i64.const 8))) (i64.shr_u (get_local $w26) (i64.const 7))) (get_local $w25))))
            (set_local $w42 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w40) (i64.const 19)) (i64.rotr (get_local $w40) (i64.const 61))) (i64.shr_u (get_local $w40) (i64.const 6))) (get_local $w35)) (i64.xor (i64.xor (i64.rotr (get_local $w27) (i64.const 1)) (i64.rotr (get_local $w27) (i64.const 8))) (i64.shr_u (get_local $w27) (i64.const 7))) (get_local $w26))))
            (set_local $w43 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w41) (i64.const 19)) (i64.rotr (get_local $w41) (i64.const 61))) (i64.shr_u (get_local $w41) (i64.const 6))) (get_local $w36)) (i64.xor (i64.xor (i64.rotr (get_local $w28) (i64.const 1)) (i64.rotr (get_local $w28) (i64.const 8))) (i64.shr_u (get_local $w28) (i64.const 7))) (get_local $w27))))
            (set_local $w44 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w42) (i64.const 19)) (i64.rotr (get_local $w42) (i64.const 61))) (i64.shr_u (get_local $w42) (i64.const 6))) (get_local $w37)) (i64.xor (i64.xor (i64.rotr (get_local $w29) (i64.const 1)) (i64.rotr (get_local $w29) (i64.const 8))) (i64.shr_u (get_local $w29) (i64.const 7))) (get_local $w28))))
            (set_local $w45 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w43) (i64.const 19)) (i64.rotr (get_local $w43) (i64.const 61))) (i64.shr_u (get_local $w43) (i64.const 6))) (get_local $w38)) (i64.xor (i64.xor (i64.rotr (get_local $w30) (i64.const 1)) (i64.rotr (get_local $w30) (i64.const 8))) (i64.shr_u (get_local $w30) (i64.const 7))) (get_local $w29))))
            (set_local $w46 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w44) (i64.const 19)) (i64.rotr (get_local $w44) (i64.const 61))) (i64.shr_u (get_local $w44) (i64.const 6))) (get_local $w39)) (i64.xor (i64.xor (i64.rotr (get_local $w31) (i64.const 1)) (i64.rotr (get_local $w31) (i64.const 8))) (i64.shr_u (get_local $w31) (i64.const 7))) (get_local $w30))))
            (set_local $w47 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w45) (i64.const 19)) (i64.rotr (get_local $w45) (i64.const 61))) (i64.shr_u (get_local $w45) (i64.const 6))) (get_local $w40)) (i64.xor (i64.xor (i64.rotr (get_local $w32) (i64.const 1)) (i64.rotr (get_local $w32) (i64.const 8))) (i64.shr_u (get_local $w32) (i64.const 7))) (get_local $w31))))
            (set_local $w48 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w46) (i64.const 19)) (i64.rotr (get_local $w46) (i64.const 61))) (i64.shr_u (get_local $w46) (i64.const 6))) (get_local $w41)) (i64.xor (i64.xor (i64.rotr (get_local $w33) (i64.const 1)) (i64.rotr (get_local $w33) (i64.const 8))) (i64.shr_u (get_local $w33) (i64.const 7))) (get_local $w32))))
            (set_local $w49 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w47) (i64.const 19)) (i64.rotr (get_local $w47) (i64.const 61))) (i64.shr_u (get_local $w47) (i64.const 6))) (get_local $w42)) (i64.xor (i64.xor (i64.rotr (get_local $w34) (i64.const 1)) (i64.rotr (get_local $w34) (i64.const 8))) (i64.shr_u (get_local $w34) (i64.const 7))) (get_local $w33))))
            (set_local $w50 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w48) (i64.const 19)) (i64.rotr (get_local $w48) (i64.const 61))) (i64.shr_u (get_local $w48) (i64.const 6))) (get_local $w43)) (i64.xor (i64.xor (i64.rotr (get_local $w35) (i64.const 1)) (i64.rotr (get_local $w35) (i64.const 8))) (i64.shr_u (get_local $w35) (i64.const 7))) (get_local $w34))))
            (set_local $w51 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w49) (i64.const 19)) (i64.rotr (get_local $w49) (i64.const 61))) (i64.shr_u (get_local $w49) (i64.const 6))) (get_local $w44)) (i64.xor (i64.xor (i64.rotr (get_local $w36) (i64.const 1)) (i64.rotr (get_local $w36) (i64.const 8))) (i64.shr_u (get_local $w36) (i64.const 7))) (get_local $w35))))
            (set_local $w52 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w50) (i64.const 19)) (i64.rotr (get_local $w50) (i64.const 61))) (i64.shr_u (get_local $w50) (i64.const 6))) (get_local $w45)) (i64.xor (i64.xor (i64.rotr (get_local $w37) (i64.const 1)) (i64.rotr (get_local $w37) (i64.const 8))) (i64.shr_u (get_local $w37) (i64.const 7))) (get_local $w36))))
            (set_local $w53 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w51) (i64.const 19)) (i64.rotr (get_local $w51) (i64.const 61))) (i64.shr_u (get_local $w51) (i64.const 6))) (get_local $w46)) (i64.xor (i64.xor (i64.rotr (get_local $w38) (i64.const 1)) (i64.rotr (get_local $w38) (i64.const 8))) (i64.shr_u (get_local $w38) (i64.const 7))) (get_local $w37))))
            (set_local $w54 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w52) (i64.const 19)) (i64.rotr (get_local $w52) (i64.const 61))) (i64.shr_u (get_local $w52) (i64.const 6))) (get_local $w47)) (i64.xor (i64.xor (i64.rotr (get_local $w39) (i64.const 1)) (i64.rotr (get_local $w39) (i64.const 8))) (i64.shr_u (get_local $w39) (i64.const 7))) (get_local $w38))))
            (set_local $w55 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w53) (i64.const 19)) (i64.rotr (get_local $w53) (i64.const 61))) (i64.shr_u (get_local $w53) (i64.const 6))) (get_local $w48)) (i64.xor (i64.xor (i64.rotr (get_local $w40) (i64.const 1)) (i64.rotr (get_local $w40) (i64.const 8))) (i64.shr_u (get_local $w40) (i64.const 7))) (get_local $w39))))
            (set_local $w56 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w54) (i64.const 19)) (i64.rotr (get_local $w54) (i64.const 61))) (i64.shr_u (get_local $w54) (i64.const 6))) (get_local $w49)) (i64.xor (i64.xor (i64.rotr (get_local $w41) (i64.const 1)) (i64.rotr (get_local $w41) (i64.const 8))) (i64.shr_u (get_local $w41) (i64.const 7))) (get_local $w40))))
            (set_local $w57 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w55) (i64.const 19)) (i64.rotr (get_local $w55) (i64.const 61))) (i64.shr_u (get_local $w55) (i64.const 6))) (get_local $w50)) (i64.xor (i64.xor (i64.rotr (get_local $w42) (i64.const 1)) (i64.rotr (get_local $w42) (i64.const 8))) (i64.shr_u (get_local $w42) (i64.const 7))) (get_local $w41))))
            (set_local $w58 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w56) (i64.const 19)) (i64.rotr (get_local $w56) (i64.const 61))) (i64.shr_u (get_local $w56) (i64.const 6))) (get_local $w51)) (i64.xor (i64.xor (i64.rotr (get_local $w43) (i64.const 1)) (i64.rotr (get_local $w43) (i64.const 8))) (i64.shr_u (get_local $w43) (i64.const 7))) (get_local $w42))))
            (set_local $w59 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w57) (i64.const 19)) (i64.rotr (get_local $w57) (i64.const 61))) (i64.shr_u (get_local $w57) (i64.const 6))) (get_local $w52)) (i64.xor (i64.xor (i64.rotr (get_local $w44) (i64.const 1)) (i64.rotr (get_local $w44) (i64.const 8))) (i64.shr_u (get_local $w44) (i64.const 7))) (get_local $w43))))
            (set_local $w60 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w58) (i64.const 19)) (i64.rotr (get_local $w58) (i64.const 61))) (i64.shr_u (get_local $w58) (i64.const 6))) (get_local $w53)) (i64.xor (i64.xor (i64.rotr (get_local $w45) (i64.const 1)) (i64.rotr (get_local $w45) (i64.const 8))) (i64.shr_u (get_local $w45) (i64.const 7))) (get_local $w44))))
            (set_local $w61 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w59) (i64.const 19)) (i64.rotr (get_local $w59) (i64.const 61))) (i64.shr_u (get_local $w59) (i64.const 6))) (get_local $w54)) (i64.xor (i64.xor (i64.rotr (get_local $w46) (i64.const 1)) (i64.rotr (get_local $w46) (i64.const 8))) (i64.shr_u (get_local $w46) (i64.const 7))) (get_local $w45))))
            (set_local $w62 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w60) (i64.const 19)) (i64.rotr (get_local $w60) (i64.const 61))) (i64.shr_u (get_local $w60) (i64.const 6))) (get_local $w55)) (i64.xor (i64.xor (i64.rotr (get_local $w47) (i64.const 1)) (i64.rotr (get_local $w47) (i64.const 8))) (i64.shr_u (get_local $w47) (i64.const 7))) (get_local $w46))))
            (set_local $w63 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w61) (i64.const 19)) (i64.rotr (get_local $w61) (i64.const 61))) (i64.shr_u (get_local $w61) (i64.const 6))) (get_local $w56)) (i64.xor (i64.xor (i64.rotr (get_local $w48) (i64.const 1)) (i64.rotr (get_local $w48) (i64.const 8))) (i64.shr_u (get_local $w48) (i64.const 7))) (get_local $w47))))
            (set_local $w64 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w62) (i64.const 19)) (i64.rotr (get_local $w62) (i64.const 61))) (i64.shr_u (get_local $w62) (i64.const 6))) (get_local $w57)) (i64.xor (i64.xor (i64.rotr (get_local $w49) (i64.const 1)) (i64.rotr (get_local $w49) (i64.const 8))) (i64.shr_u (get_local $w49) (i64.const 7))) (get_local $w48))))
            (set_local $w65 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w63) (i64.const 19)) (i64.rotr (get_local $w63) (i64.const 61))) (i64.shr_u (get_local $w63) (i64.const 6))) (get_local $w58)) (i64.xor (i64.xor (i64.rotr (get_local $w50) (i64.const 1)) (i64.rotr (get_local $w50) (i64.const 8))) (i64.shr_u (get_local $w50) (i64.const 7))) (get_local $w49))))
            (set_local $w66 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w64) (i64.const 19)) (i64.rotr (get_local $w64) (i64.const 61))) (i64.shr_u (get_local $w64) (i64.const 6))) (get_local $w59)) (i64.xor (i64.xor (i64.rotr (get_local $w51) (i64.const 1)) (i64.rotr (get_local $w51) (i64.const 8))) (i64.shr_u (get_local $w51) (i64.const 7))) (get_local $w50))))
            (set_local $w67 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w65) (i64.const 19)) (i64.rotr (get_local $w65) (i64.const 61))) (i64.shr_u (get_local $w65) (i64.const 6))) (get_local $w60)) (i64.xor (i64.xor (i64.rotr (get_local $w52) (i64.const 1)) (i64.rotr (get_local $w52) (i64.const 8))) (i64.shr_u (get_local $w52) (i64.const 7))) (get_local $w51))))
            (set_local $w68 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w66) (i64.const 19)) (i64.rotr (get_local $w66) (i64.const 61))) (i64.shr_u (get_local $w66) (i64.const 6))) (get_local $w61)) (i64.xor (i64.xor (i64.rotr (get_local $w53) (i64.const 1)) (i64.rotr (get_local $w53) (i64.const 8))) (i64.shr_u (get_local $w53) (i64.const 7))) (get_local $w52))))
            (set_local $w69 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w67) (i64.const 19)) (i64.rotr (get_local $w67) (i64.const 61))) (i64.shr_u (get_local $w67) (i64.const 6))) (get_local $w62)) (i64.xor (i64.xor (i64.rotr (get_local $w54) (i64.const 1)) (i64.rotr (get_local $w54) (i64.const 8))) (i64.shr_u (get_local $w54) (i64.const 7))) (get_local $w53))))
            (set_local $w70 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w68) (i64.const 19)) (i64.rotr (get_local $w68) (i64.const 61))) (i64.shr_u (get_local $w68) (i64.const 6))) (get_local $w63)) (i64.xor (i64.xor (i64.rotr (get_local $w55) (i64.const 1)) (i64.rotr (get_local $w55) (i64.const 8))) (i64.shr_u (get_local $w55) (i64.const 7))) (get_local $w54))))
            (set_local $w71 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w69) (i64.const 19)) (i64.rotr (get_local $w69) (i64.const 61))) (i64.shr_u (get_local $w69) (i64.const 6))) (get_local $w64)) (i64.xor (i64.xor (i64.rotr (get_local $w56) (i64.const 1)) (i64.rotr (get_local $w56) (i64.const 8))) (i64.shr_u (get_local $w56) (i64.const 7))) (get_local $w55))))
            (set_local $w72 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w70) (i64.const 19)) (i64.rotr (get_local $w70) (i64.const 61))) (i64.shr_u (get_local $w70) (i64.const 6))) (get_local $w65)) (i64.xor (i64.xor (i64.rotr (get_local $w57) (i64.const 1)) (i64.rotr (get_local $w57) (i64.const 8))) (i64.shr_u (get_local $w57) (i64.const 7))) (get_local $w56))))
            (set_local $w73 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w71) (i64.const 19)) (i64.rotr (get_local $w71) (i64.const 61))) (i64.shr_u (get_local $w71) (i64.const 6))) (get_local $w66)) (i64.xor (i64.xor (i64.rotr (get_local $w58) (i64.const 1)) (i64.rotr (get_local $w58) (i64.const 8))) (i64.shr_u (get_local $w58) (i64.const 7))) (get_local $w57))))
            (set_local $w74 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w72) (i64.const 19)) (i64.rotr (get_local $w72) (i64.const 61))) (i64.shr_u (get_local $w72) (i64.const 6))) (get_local $w67)) (i64.xor (i64.xor (i64.rotr (get_local $w59) (i64.const 1)) (i64.rotr (get_local $w59) (i64.const 8))) (i64.shr_u (get_local $w59) (i64.const 7))) (get_local $w58))))
            (set_local $w75 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w73) (i64.const 19)) (i64.rotr (get_local $w73) (i64.const 61))) (i64.shr_u (get_local $w73) (i64.const 6))) (get_local $w68)) (i64.xor (i64.xor (i64.rotr (get_local $w60) (i64.const 1)) (i64.rotr (get_local $w60) (i64.const 8))) (i64.shr_u (get_local $w60) (i64.const 7))) (get_local $w59))))
            (set_local $w76 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w74) (i64.const 19)) (i64.rotr (get_local $w74) (i64.const 61))) (i64.shr_u (get_local $w74) (i64.const 6))) (get_local $w69)) (i64.xor (i64.xor (i64.rotr (get_local $w61) (i64.const 1)) (i64.rotr (get_local $w61) (i64.const 8))) (i64.shr_u (get_local $w61) (i64.const 7))) (get_local $w60))))
            (set_local $w77 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w75) (i64.const 19)) (i64.rotr (get_local $w75) (i64.const 61))) (i64.shr_u (get_local $w75) (i64.const 6))) (get_local $w70)) (i64.xor (i64.xor (i64.rotr (get_local $w62) (i64.const 1)) (i64.rotr (get_local $w62) (i64.const 8))) (i64.shr_u (get_local $w62) (i64.const 7))) (get_local $w61))))
            (set_local $w78 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w76) (i64.const 19)) (i64.rotr (get_local $w76) (i64.const 61))) (i64.shr_u (get_local $w76) (i64.const 6))) (get_local $w71)) (i64.xor (i64.xor (i64.rotr (get_local $w63) (i64.const 1)) (i64.rotr (get_local $w63) (i64.const 8))) (i64.shr_u (get_local $w63) (i64.const 7))) (get_local $w62))))
            (set_local $w79 (i64.add (i64.add (i64.add (i64.xor (i64.xor (i64.rotr (get_local $w77) (i64.const 19)) (i64.rotr (get_local $w77) (i64.const 61))) (i64.shr_u (get_local $w77) (i64.const 6))) (get_local $w72)) (i64.xor (i64.xor (i64.rotr (get_local $w64) (i64.const 1)) (i64.rotr (get_local $w64) (i64.const 8))) (i64.shr_u (get_local $w64) (i64.const 7))) (get_local $w63))))


            (call $i64.log (get_local $w0 ))
            (call $i64.log (get_local $w1 ))
            (call $i64.log (get_local $w2 ))
            (call $i64.log (get_local $w3 ))
            (call $i64.log (get_local $w4 ))
            (call $i64.log (get_local $w5 ))
            (call $i64.log (get_local $w6 ))
            (call $i64.log (get_local $w7 ))
            (call $i64.log (get_local $w8 ))
            (call $i64.log (get_local $w9 ))
            (call $i64.log (get_local $w10))
            (call $i64.log (get_local $w11))
            (call $i64.log (get_local $w12))
            (call $i64.log (get_local $w13))
            (call $i64.log (get_local $w14))
            (call $i64.log (get_local $w15))

            ;; load previous hash state into registers
            (set_local $a (i64.load offset=0 (i32.const 0)))
            (set_local $b (i64.load offset=8 (i32.const 0)))
            (set_local $c (i64.load offset=16 (i32.const 0)))
            (set_local $d (i64.load offset=24 (i32.const 0)))
            (set_local $e (i64.load offset=32 (i32.const 0)))
            (set_local $f (i64.load offset=40 (i32.const 0)))
            (set_local $g (i64.load offset=48 (i32.const 0)))
            (set_local $h (i64.load offset=56 (i32.const 0)))

            ;; ROUND 0

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K0 + W0
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w0)) (get_local $k0 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 1

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K1 + W1
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w1)) (get_local $k1 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 2

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K2 + W2
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w2)) (get_local $k2 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 3

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K3 + W3
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w3)) (get_local $k3 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 4

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K4 + W4
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w4)) (get_local $k4 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 5

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K5 + W5
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w5)) (get_local $k5 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 6

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K6 + W6
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w6)) (get_local $k6 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 7

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K7 + W7
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w7)) (get_local $k7 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 8

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K8 + W8
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w8)) (get_local $k8 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 9

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K9 + W9
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w9)) (get_local $k9 )))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 10

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K10 + W10
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w10)) (get_local $k10)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 11

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K11 + W11
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w11)) (get_local $k11)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 12

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K12 + W12
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w12)) (get_local $k12)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 13

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K13 + W13
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w13)) (get_local $k13)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 14

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K14 + W14
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w14)) (get_local $k14)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 15

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K15 + W15
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w15)) (get_local $k15)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 16

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K16 + W16
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w16)) (get_local $k16)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 17

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K17 + W17
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w17)) (get_local $k17)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 18

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K18 + W18
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w18)) (get_local $k18)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 19

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K19 + W19
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w19)) (get_local $k19)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 20

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K20 + W20
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w20)) (get_local $k20)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 21

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K21 + W21
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w21)) (get_local $k21)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 22

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K22 + W22
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w22)) (get_local $k22)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 23

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K23 + W23
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w23)) (get_local $k23)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 24

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K24 + W24
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w24)) (get_local $k24)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 25

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K25 + W25
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w25)) (get_local $k25)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 26

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K26 + W26
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w26)) (get_local $k26)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 27

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K27 + W27
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w27)) (get_local $k27)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 28

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K28 + W28
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w28)) (get_local $k28)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 29

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K29 + W29
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w29)) (get_local $k29)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 30

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K30 + W30
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w30)) (get_local $k30)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 31

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K31 + W31
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w31)) (get_local $k31)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 32

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K32 + W32
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w32)) (get_local $k32)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 33

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K33 + W33
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w33)) (get_local $k33)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 34

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K34 + W34
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w34)) (get_local $k34)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 35

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K35 + W35
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w35)) (get_local $k35)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 36

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K36 + W36
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w36)) (get_local $k36)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 37

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K37 + W37
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w37)) (get_local $k37)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 38

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K38 + W38
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w38)) (get_local $k38)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 39

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K39 + W39
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w39)) (get_local $k39)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 40

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K40 + W40
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w40)) (get_local $k40)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 41

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K41 + W41
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w41)) (get_local $k41)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 42

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K42 + W42
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w42)) (get_local $k42)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 43

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K43 + W43
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w43)) (get_local $k43)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 44

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K44 + W44
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w44)) (get_local $k44)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 45

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K45 + W45
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w45)) (get_local $k45)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 46

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K46 + W46
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w46)) (get_local $k46)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 47

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K47 + W47
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w47)) (get_local $k47)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 48

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K48 + W48
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w48)) (get_local $k48)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 49

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K49 + W49
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w49)) (get_local $k49)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 50

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K50 + W50
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w50)) (get_local $k50)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 51

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K51 + W51
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w51)) (get_local $k51)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 52

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K52 + W52
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w52)) (get_local $k52)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 53

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K53 + W53
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w53)) (get_local $k53)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 54

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K54 + W54
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w54)) (get_local $k54)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 55

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K55 + W55
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w55)) (get_local $k55)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 56

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K56 + W56
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w56)) (get_local $k56)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 57

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K57 + W57
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w57)) (get_local $k57)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 58

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K58 + W58
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w58)) (get_local $k58)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 59

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K59 + W59
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w59)) (get_local $k59)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 60

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K60 + W60
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w60)) (get_local $k60)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 61

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K61 + W61
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w61)) (get_local $k61)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 62

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K62 + W62
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w62)) (get_local $k62)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 63

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K63 + W63
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w63)) (get_local $k63)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 64

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K64 + W64
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w64)) (get_local $k64)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 65

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K65 + W65
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w65)) (get_local $k65)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 66

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K66 + W66
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w66)) (get_local $k66)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 67

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K67 + W67
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w67)) (get_local $k67)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 68

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K68 + W68
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w68)) (get_local $k68)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 69

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K69 + W69
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w69)) (get_local $k69)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 70

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K70 + W70
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w70)) (get_local $k70)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 71

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K71 + W71
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w71)) (get_local $k71)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 72

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K72 + W72
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w72)) (get_local $k72)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 73

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K73 + W73
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w73)) (get_local $k73)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 74

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K74 + W74
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w74)) (get_local $k74)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 75

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K75 + W75
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w75)) (get_local $k75)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 76

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K76 + W76
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w76)) (get_local $k76)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 77

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K77 + W77
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w77)) (get_local $k77)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 78

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K78 + W78
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w78)) (get_local $k78)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))

            ;; ROUND 79

            ;; precompute intermediate values

            ;; T1 = h + big_sig1(e) + ch(e, f, g) + K79 + W79
            ;; T2 = big_sig0(a) + Maj(a, b, c)

            (set_local $ch_res (i64.xor (i64.and (get_local $e) (get_local $f)) (i64.and (i64.xor (get_local $e) (i64.const -1)) (get_local $g))))
            (set_local $maj_res (i64.xor (i64.xor (i64.and (get_local $a) (get_local $b)) (i64.and (get_local $a) (get_local $c))) (i64.and (get_local $b) (get_local $c))))

            (set_local $big_sig0_res (i64.xor (i64.xor (i64.rotr (get_local $a) (i64.const 28)) (i64.rotr (get_local $a) (i64.const 34))) (i64.rotr (get_local $a) (i64.const 39))))
            (set_local $big_sig1_res (i64.xor (i64.xor (i64.rotr (get_local $e) (i64.const 14)) (i64.rotr (get_local $e) (i64.const 18))) (i64.rotr (get_local $e) (i64.const 41))))

            (set_local $T1 (i64.add (i64.add (i64.add (i64.add (get_local $h) (get_local $ch_res)) (get_local $big_sig1_res)) (get_local $w79)) (get_local $k79)))
            (set_local $T2 (i64.add (get_local $big_sig0_res) (get_local $maj_res)))

            ;; update registers

            ;; h <- g
            (set_local $h (get_local $g))

            ;; g <- f
            (set_local $g (get_local $f))  

            ;; f <- e
            (set_local $f (get_local $e))  

            ;; e <- d + T1
            (set_local $e (i64.add (get_local $d) (get_local $T1)))

            ;; d <- c
            (set_local $d (get_local $c))  

            ;; c <- b
            (set_local $c (get_local $b))  

            ;; b <- a
            (set_local $b (get_local $a))  

            ;; a <- T1 + T2
            (set_local $a (i64.add (get_local $T1) (get_local $T2)))))

            

    ;; HASH COMPLETE FOR MESSAGE BLOCK
    ;; store hash values
    (i64.store offset=0  (i32.const 0) (i64.add (i64.load offset=0  (i32.const 0)) (get_local $a)))
    (i64.store offset=8  (i32.const 0) (i64.add (i64.load offset=8  (i32.const 0)) (get_local $b)))
    (i64.store offset=16 (i32.const 0) (i64.add (i64.load offset=16 (i32.const 0)) (get_local $c)))    
    (i64.store offset=24 (i32.const 0) (i64.add (i64.load offset=24 (i32.const 0)) (get_local $d)))
    (i64.store offset=32 (i32.const 0) (i64.add (i64.load offset=32 (i32.const 0)) (get_local $e)))
    (i64.store offset=40 (i32.const 0) (i64.add (i64.load offset=40 (i32.const 0)) (get_local $f)))
    (i64.store offset=48 (i32.const 0) (i64.add (i64.load offset=48 (i32.const 0)) (get_local $g)))
    (i64.store offset=56 (i32.const 0) (i64.add (i64.load offset=56 (i32.const 0)) (get_local $h)))

    ;; correctly store last word in the input for next update and return number of leftover bytes
    (i64.store8 (get_local $input) (i64.shr_u (get_local $last_word) (i64.const 56)))
    (i64.store8 offset=1 (get_local $input) (i64.shr_u (get_local $last_word) (i64.const 48)))
    (i64.store8 offset=2 (get_local $input) (i64.shr_u (get_local $last_word) (i64.const 40)))
    (i64.store8 offset=3 (get_local $input) (i64.shr_u (get_local $last_word) (i64.const 32)))
    (i64.store8 offset=4 (get_local $input) (i64.shr_u (get_local $last_word) (i64.const 24)))
    (i64.store8 offset=5 (get_local $input) (i64.shr_u (get_local $last_word) (i64.const 16)))
    (i64.store8 offset=6 (get_local $input) (i64.shr_u (get_local $last_word) (i64.const 8)))
    (i64.store8 offset=7 (get_local $input) (get_local $last_word))
    (get_local $leftover)))
    
