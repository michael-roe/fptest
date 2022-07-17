	la sp, _stack
	li a1, 1
	la t0, _test
	jalr 0(t0)
	j _fail
_test:

	# Test 1
	# b32?f =0 i -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L0
	ret
L0:


	# Test 2
	# b32?f =0 i -1.7FFFFFP127 -> 0x1 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L1
	ret
L1:


	# Test 3
	# b32?f =0 i -1.0E0F8BP33 -> 0x1 


	addi a1, a1, 1
	li t0, 0xd00e0f8b	 #-0x1.1c1f1600p+33
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L2
	ret
L2:


	# Test 4
	# b32?f =0 i -1.000000P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L3
	ret
L3:


	# Test 5
	# b32?f =0 i -0.7FFFFFP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L4
	ret
L4:


	# Test 6
	# b32?f =0 i -0.0F95D6P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x800f95d6	 #-0x1.f2bac000p-130
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L5
	ret
L5:


	# Test 7
	# b32?f =0 i -0.000001P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L6
	ret
L6:


	# Test 8
	# b32?f =0 i -1.000000P0 -> 0x1 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L7
	ret
L7:


	# Test 9
	# b32?f =0 i -Zero -> 0x1 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L8
	ret
L8:


	# Test 10
	# b32?f =0 i +Zero -> 0x1 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L9
	ret
L9:


	# Test 11
	# b32?f =0 i +1.000000P0 -> 0x1 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L10
	ret
L10:


	# Test 12
	# b32?f =0 i +0.000001P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L11
	ret
L11:


	# Test 13
	# b32?f =0 i +0.1FA6DAP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x1fa6da	 #0x1.fa6da000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L12
	ret
L12:


	# Test 14
	# b32?f =0 i +0.7FFFFFP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L13
	ret
L13:


	# Test 15
	# b32?f =0 i +1.000000P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L14
	ret
L14:


	# Test 16
	# b32?f =0 i +1.4BD50CP34 -> 0x1 


	addi a1, a1, 1
	li t0, 0x50cbd50c	 #0x1.97aa1800p+34
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L15
	ret
L15:


	# Test 17
	# b32?f =0 i +1.7FFFFFP127 -> 0x1 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L16
	ret
L16:


	# Test 18
	# b32?f =0 i +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L17
	ret
L17:


	# Test 19
	# b32?f =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L18
	ret
L18:


	# Test 20
	# b32?f =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L19
	ret
L19:


	# Test 21
	# b32?f =0 -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L20
	ret
L20:


	# Test 22
	# b32?f =0 -1.7FFFFFP127 -> 0x1 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L21
	ret
L21:


	# Test 23
	# b32?f =0 -1.2613B4P84 -> 0x1 


	addi a1, a1, 1
	li t0, 0xe9a613b4	 #-0x1.4c276800p+84
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L22
	ret
L22:


	# Test 24
	# b32?f =0 -1.000000P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L23
	ret
L23:


	# Test 25
	# b32?f =0 -0.7FFFFFP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L24
	ret
L24:


	# Test 26
	# b32?f =0 -0.5241E5P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x805241e5	 #-0x1.49079400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L25
	ret
L25:


	# Test 27
	# b32?f =0 -0.000001P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L26
	ret
L26:


	# Test 28
	# b32?f =0 -1.000000P0 -> 0x1 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L27
	ret
L27:


	# Test 29
	# b32?f =0 -Zero -> 0x1 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L28
	ret
L28:


	# Test 30
	# b32?f =0 +Zero -> 0x1 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L29
	ret
L29:


	# Test 31
	# b32?f =0 +1.000000P0 -> 0x1 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L30
	ret
L30:


	# Test 32
	# b32?f =0 +0.000001P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L31
	ret
L31:


	# Test 33
	# b32?f =0 +0.17EB02P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x17eb02	 #0x1.7eb02000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L32
	ret
L32:


	# Test 34
	# b32?f =0 +0.7FFFFFP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L33
	ret
L33:


	# Test 35
	# b32?f =0 +1.000000P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L34
	ret
L34:


	# Test 36
	# b32?f =0 +1.641934P-11 -> 0x1 


	addi a1, a1, 1
	li t0, 0x3a641934	 #0x1.c8326800p-11
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L35
	ret
L35:


	# Test 37
	# b32?f =0 +1.7FFFFFP127 -> 0x1 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L36
	ret
L36:


	# Test 38
	# b32?f =0 +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L37
	ret
L37:


	# Test 39
	# b32?f =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L38
	ret
L38:


	# Test 40
	# b32?f =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x7e
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L39
	ret
L39:


	# Test 41
	# b32?i =0 i -Inf -> 0x1 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L40
	ret
L40:


	# Test 42
	# b32?i =0 i -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L41
	ret
L41:


	# Test 43
	# b32?i =0 i -1.4D01DAP106 -> 0x0 


	addi a1, a1, 1
	li t0, 0xf4cd01da	 #-0x1.9a03b400p+106
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L42
	ret
L42:


	# Test 44
	# b32?i =0 i -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L43
	ret
L43:


	# Test 45
	# b32?i =0 i -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L44
	ret
L44:


	# Test 46
	# b32?i =0 i -0.2E0824P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x802e0824	 #-0x1.70412000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L45
	ret
L45:


	# Test 47
	# b32?i =0 i -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L46
	ret
L46:


	# Test 48
	# b32?i =0 i -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L47
	ret
L47:


	# Test 49
	# b32?i =0 i -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L48
	ret
L48:


	# Test 50
	# b32?i =0 i +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L49
	ret
L49:


	# Test 51
	# b32?i =0 i +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L50
	ret
L50:


	# Test 52
	# b32?i =0 i +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L51
	ret
L51:


	# Test 53
	# b32?i =0 i +0.194110P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x194110	 #0x1.94110000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L52
	ret
L52:


	# Test 54
	# b32?i =0 i +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L53
	ret
L53:


	# Test 55
	# b32?i =0 i +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L54
	ret
L54:


	# Test 56
	# b32?i =0 i +1.0AC75AP92 -> 0x0 


	addi a1, a1, 1
	li t0, 0x6d8ac75a	 #0x1.158eb400p+92
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L55
	ret
L55:


	# Test 57
	# b32?i =0 i +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L56
	ret
L56:


	# Test 58
	# b32?i =0 i +Inf -> 0x1 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L57
	ret
L57:


	# Test 59
	# b32?i =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L58
	ret
L58:


	# Test 60
	# b32?i =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L59
	ret
L59:


	# Test 61
	# b32?i =0 -Inf -> 0x1 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L60
	ret
L60:


	# Test 62
	# b32?i =0 -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L61
	ret
L61:


	# Test 63
	# b32?i =0 -1.654602P109 -> 0x0 


	addi a1, a1, 1
	li t0, 0xf6654602	 #-0x1.ca8c0400p+109
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L62
	ret
L62:


	# Test 64
	# b32?i =0 -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L63
	ret
L63:


	# Test 65
	# b32?i =0 -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L64
	ret
L64:


	# Test 66
	# b32?i =0 -0.26CC4CP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x8026cc4c	 #-0x1.36626000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L65
	ret
L65:


	# Test 67
	# b32?i =0 -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L66
	ret
L66:


	# Test 68
	# b32?i =0 -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L67
	ret
L67:


	# Test 69
	# b32?i =0 -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L68
	ret
L68:


	# Test 70
	# b32?i =0 +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L69
	ret
L69:


	# Test 71
	# b32?i =0 +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L70
	ret
L70:


	# Test 72
	# b32?i =0 +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L71
	ret
L71:


	# Test 73
	# b32?i =0 +0.62E584P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x62e584	 #0x1.8b961000p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L72
	ret
L72:


	# Test 74
	# b32?i =0 +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L73
	ret
L73:


	# Test 75
	# b32?i =0 +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L74
	ret
L74:


	# Test 76
	# b32?i =0 +1.22CB82P15 -> 0x0 


	addi a1, a1, 1
	li t0, 0x4722cb82	 #0x1.45970400p+15
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L75
	ret
L75:


	# Test 77
	# b32?i =0 +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L76
	ret
L76:


	# Test 78
	# b32?i =0 +Inf -> 0x1 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L77
	ret
L77:


	# Test 79
	# b32?i =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L78
	ret
L78:


	# Test 80
	# b32?i =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x81
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L79
	ret
L79:


	# Test 81
	# b32?N =0 i -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L80
	ret
L80:


	# Test 82
	# b32?N =0 i -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L81
	ret
L81:


	# Test 83
	# b32?N =0 i -1.4F1D3DP103 -> 0x0 


	addi a1, a1, 1
	li t0, 0xf34f1d3d	 #-0x1.9e3a7a00p+103
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L82
	ret
L82:


	# Test 84
	# b32?N =0 i -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L83
	ret
L83:


	# Test 85
	# b32?N =0 i -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L84
	ret
L84:


	# Test 86
	# b32?N =0 i -0.1FC33CP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x801fc33c	 #-0x1.fc33c000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L85
	ret
L85:


	# Test 87
	# b32?N =0 i -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L86
	ret
L86:


	# Test 88
	# b32?N =0 i -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L87
	ret
L87:


	# Test 89
	# b32?N =0 i -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L88
	ret
L88:


	# Test 90
	# b32?N =0 i +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L89
	ret
L89:


	# Test 91
	# b32?N =0 i +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L90
	ret
L90:


	# Test 92
	# b32?N =0 i +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L91
	ret
L91:


	# Test 93
	# b32?N =0 i +0.61348CP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x61348c	 #0x1.84d23000p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L92
	ret
L92:


	# Test 94
	# b32?N =0 i +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L93
	ret
L93:


	# Test 95
	# b32?N =0 i +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L94
	ret
L94:


	# Test 96
	# b32?N =0 i +1.0D62BDP-87 -> 0x0 


	addi a1, a1, 1
	li t0, 0x140d62bd	 #0x1.1ac57a00p-87
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L95
	ret
L95:


	# Test 97
	# b32?N =0 i +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L96
	ret
L96:


	# Test 98
	# b32?N =0 i +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L97
	ret
L97:


	# Test 99
	# b32?N =0 i q -> 0x1 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L98
	ret
L98:


	# Test 100
	# b32?N =0 i q -> 0x1 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L99
	ret
L99:


	# Test 101
	# b32?N =0 -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L100
	ret
L100:


	# Test 102
	# b32?N =0 -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L101
	ret
L101:


	# Test 103
	# b32?N =0 -1.52494DP27 -> 0x0 


	addi a1, a1, 1
	li t0, 0xcd52494d	 #-0x1.a4929a00p+27
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L102
	ret
L102:


	# Test 104
	# b32?N =0 -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L103
	ret
L103:


	# Test 105
	# b32?N =0 -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L104
	ret
L104:


	# Test 106
	# b32?N =0 -0.53CF97P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x8053cf97	 #-0x1.4f3e5c00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L105
	ret
L105:


	# Test 107
	# b32?N =0 -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L106
	ret
L106:


	# Test 108
	# b32?N =0 -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L107
	ret
L107:


	# Test 109
	# b32?N =0 -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L108
	ret
L108:


	# Test 110
	# b32?N =0 +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L109
	ret
L109:


	# Test 111
	# b32?N =0 +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L110
	ret
L110:


	# Test 112
	# b32?N =0 +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L111
	ret
L111:


	# Test 113
	# b32?N =0 +0.7938B4P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7938b4	 #0x1.e4e2d000p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L112
	ret
L112:


	# Test 114
	# b32?N =0 +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L113
	ret
L113:


	# Test 115
	# b32?N =0 +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L114
	ret
L114:


	# Test 116
	# b32?N =0 +1.0F8ECDP-99 -> 0x0 


	addi a1, a1, 1
	li t0, 0xe0f8ecd	 #0x1.1f1d9a00p-99
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L115
	ret
L115:


	# Test 117
	# b32?N =0 +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L116
	ret
L116:


	# Test 118
	# b32?N =0 +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L117
	ret
L117:


	# Test 119
	# b32?N =0 q -> 0x1 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L118
	ret
L118:


	# Test 120
	# b32?N =0 q -> 0x1 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x300
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L119
	ret
L119:


	# Test 121
	# b32?n =0 i -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L120
	ret
L120:


	# Test 122
	# b32?n =0 i -1.7FFFFFP127 -> 0x1 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L121
	ret
L121:


	# Test 123
	# b32?n =0 i -1.0BB428P-44 -> 0x1 


	addi a1, a1, 1
	li t0, 0xa98bb428	 #-0x1.17685000p-44
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L122
	ret
L122:


	# Test 124
	# b32?n =0 i -1.000000P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L123
	ret
L123:


	# Test 125
	# b32?n =0 i -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L124
	ret
L124:


	# Test 126
	# b32?n =0 i -0.4D7A72P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x804d7a72	 #-0x1.35e9c800p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L125
	ret
L125:


	# Test 127
	# b32?n =0 i -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L126
	ret
L126:


	# Test 128
	# b32?n =0 i -1.000000P0 -> 0x1 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L127
	ret
L127:


	# Test 129
	# b32?n =0 i -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L128
	ret
L128:


	# Test 130
	# b32?n =0 i +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L129
	ret
L129:


	# Test 131
	# b32?n =0 i +1.000000P0 -> 0x1 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L130
	ret
L130:


	# Test 132
	# b32?n =0 i +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L131
	ret
L131:


	# Test 133
	# b32?n =0 i +0.5D0B77P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x5d0b77	 #0x1.742ddc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L132
	ret
L132:


	# Test 134
	# b32?n =0 i +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L133
	ret
L133:


	# Test 135
	# b32?n =0 i +1.000000P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L134
	ret
L134:


	# Test 136
	# b32?n =0 i +1.49B9A8P21 -> 0x1 


	addi a1, a1, 1
	li t0, 0x4a49b9a8	 #0x1.93735000p+21
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L135
	ret
L135:


	# Test 137
	# b32?n =0 i +1.7FFFFFP127 -> 0x1 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L136
	ret
L136:


	# Test 138
	# b32?n =0 i +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L137
	ret
L137:


	# Test 139
	# b32?n =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L138
	ret
L138:


	# Test 140
	# b32?n =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L139
	ret
L139:


	# Test 141
	# b32?n =0 -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L140
	ret
L140:


	# Test 142
	# b32?n =0 -1.7FFFFFP127 -> 0x1 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L141
	ret
L141:


	# Test 143
	# b32?n =0 -1.0FC083P66 -> 0x1 


	addi a1, a1, 1
	li t0, 0xe08fc083	 #-0x1.1f810600p+66
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L142
	ret
L142:


	# Test 144
	# b32?n =0 -1.000000P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L143
	ret
L143:


	# Test 145
	# b32?n =0 -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L144
	ret
L144:


	# Test 146
	# b32?n =0 -0.653E9BP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80653e9b	 #-0x1.94fa6c00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L145
	ret
L145:


	# Test 147
	# b32?n =0 -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L146
	ret
L146:


	# Test 148
	# b32?n =0 -1.000000P0 -> 0x1 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L147
	ret
L147:


	# Test 149
	# b32?n =0 -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L148
	ret
L148:


	# Test 150
	# b32?n =0 +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L149
	ret
L149:


	# Test 151
	# b32?n =0 +1.000000P0 -> 0x1 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L150
	ret
L150:


	# Test 152
	# b32?n =0 +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L151
	ret
L151:


	# Test 153
	# b32?n =0 +0.0C7FBAP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0xc7fba	 #0x1.8ff74000p-130
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L152
	ret
L152:


	# Test 154
	# b32?n =0 +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L153
	ret
L153:


	# Test 155
	# b32?n =0 +1.000000P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L154
	ret
L154:


	# Test 156
	# b32?n =0 +1.4DC604P-61 -> 0x1 


	addi a1, a1, 1
	li t0, 0x214dc604	 #0x1.9b8c0800p-61
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L155
	ret
L155:


	# Test 157
	# b32?n =0 +1.7FFFFFP127 -> 0x1 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L156
	ret
L156:


	# Test 158
	# b32?n =0 +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L157
	ret
L157:


	# Test 159
	# b32?n =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L158
	ret
L158:


	# Test 160
	# b32?n =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x42
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L159
	ret
L159:


	# Test 161
	# b32?sN =0 i -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L160
	ret
L160:


	# Test 162
	# b32?sN =0 i -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L161
	ret
L161:


	# Test 163
	# b32?sN =0 i -1.0AF910P-123 -> 0x0 


	addi a1, a1, 1
	li t0, 0x820af910	 #-0x1.15f22000p-123
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L162
	ret
L162:


	# Test 164
	# b32?sN =0 i -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L163
	ret
L163:


	# Test 165
	# b32?sN =0 i -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L164
	ret
L164:


	# Test 166
	# b32?sN =0 i -0.087728P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80087728	 #-0x1.0ee50000p-130
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L165
	ret
L165:


	# Test 167
	# b32?sN =0 i -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L166
	ret
L166:


	# Test 168
	# b32?sN =0 i -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L167
	ret
L167:


	# Test 169
	# b32?sN =0 i -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L168
	ret
L168:


	# Test 170
	# b32?sN =0 i +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L169
	ret
L169:


	# Test 171
	# b32?sN =0 i +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L170
	ret
L170:


	# Test 172
	# b32?sN =0 i +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L171
	ret
L171:


	# Test 173
	# b32?sN =0 i +0.473847P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x473847	 #0x1.1ce11c00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L172
	ret
L172:


	# Test 174
	# b32?sN =0 i +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L173
	ret
L173:


	# Test 175
	# b32?sN =0 i +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L174
	ret
L174:


	# Test 176
	# b32?sN =0 i +1.483E91P98 -> 0x0 


	addi a1, a1, 1
	li t0, 0x70c83e91	 #0x1.907d2200p+98
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L175
	ret
L175:


	# Test 177
	# b32?sN =0 i +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L176
	ret
L176:


	# Test 178
	# b32?sN =0 i +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L177
	ret
L177:


	# Test 179
	# b32?sN =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L178
	ret
L178:


	# Test 180
	# b32?sN =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L179
	ret
L179:


	# Test 181
	# b32?sN =0 -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L180
	ret
L180:


	# Test 182
	# b32?sN =0 -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L181
	ret
L181:


	# Test 183
	# b32?sN =0 -1.22FD39P52 -> 0x0 


	addi a1, a1, 1
	li t0, 0xd9a2fd39	 #-0x1.45fa7200p+52
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L182
	ret
L182:


	# Test 184
	# b32?sN =0 -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L183
	ret
L183:


	# Test 185
	# b32?sN =0 -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L184
	ret
L184:


	# Test 186
	# b32?sN =0 -0.640383P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80640383	 #-0x1.900e0c00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L185
	ret
L185:


	# Test 187
	# b32?sN =0 -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L186
	ret
L186:


	# Test 188
	# b32?sN =0 -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L187
	ret
L187:


	# Test 189
	# b32?sN =0 -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L188
	ret
L188:


	# Test 190
	# b32?sN =0 +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L189
	ret
L189:


	# Test 191
	# b32?sN =0 +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L190
	ret
L190:


	# Test 192
	# b32?sN =0 +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L191
	ret
L191:


	# Test 193
	# b32?sN =0 +0.5F7C6FP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x5f7c6f	 #0x1.7df1bc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L192
	ret
L192:


	# Test 194
	# b32?sN =0 +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L193
	ret
L193:


	# Test 195
	# b32?sN =0 +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L194
	ret
L194:


	# Test 196
	# b32?sN =0 +1.6082B9P-11 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3a6082b9	 #0x1.c1057200p-11
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L195
	ret
L195:


	# Test 197
	# b32?sN =0 +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L196
	ret
L196:


	# Test 198
	# b32?sN =0 +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L197
	ret
L197:


	# Test 199
	# b32?sN =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L198
	ret
L198:


	# Test 200
	# b32?sN =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x100
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L199
	ret
L199:


	# Test 201
	# b32?s =0 i -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L200
	ret
L200:


	# Test 202
	# b32?s =0 i -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L201
	ret
L201:


	# Test 203
	# b32?s =0 i -1.5FBE8FP-20 -> 0x0 


	addi a1, a1, 1
	li t0, 0xb5dfbe8f	 #-0x1.bf7d1e00p-20
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L202
	ret
L202:


	# Test 204
	# b32?s =0 i -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L203
	ret
L203:


	# Test 205
	# b32?s =0 i -0.7FFFFFP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L204
	ret
L204:


	# Test 206
	# b32?s =0 i -0.6144D9P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x806144d9	 #-0x1.85136400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L205
	ret
L205:


	# Test 207
	# b32?s =0 i -0.000001P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L206
	ret
L206:


	# Test 208
	# b32?s =0 i -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L207
	ret
L207:


	# Test 209
	# b32?s =0 i -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L208
	ret
L208:


	# Test 210
	# b32?s =0 i +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L209
	ret
L209:


	# Test 211
	# b32?s =0 i +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L210
	ret
L210:


	# Test 212
	# b32?s =0 i +0.000001P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L211
	ret
L211:


	# Test 213
	# b32?s =0 i +0.2845F8P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x2845f8	 #0x1.422fc000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L212
	ret
L212:


	# Test 214
	# b32?s =0 i +0.7FFFFFP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L213
	ret
L213:


	# Test 215
	# b32?s =0 i +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L214
	ret
L214:


	# Test 216
	# b32?s =0 i +1.1D840FP46 -> 0x0 


	addi a1, a1, 1
	li t0, 0x569d840f	 #0x1.3b081e00p+46
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L215
	ret
L215:


	# Test 217
	# b32?s =0 i +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L216
	ret
L216:


	# Test 218
	# b32?s =0 i +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L217
	ret
L217:


	# Test 219
	# b32?s =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L218
	ret
L218:


	# Test 220
	# b32?s =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L219
	ret
L219:


	# Test 221
	# b32?s =0 -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L220
	ret
L220:


	# Test 222
	# b32?s =0 -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L221
	ret
L221:


	# Test 223
	# b32?s =0 -1.4EB2D2P27 -> 0x0 


	addi a1, a1, 1
	li t0, 0xcd4eb2d2	 #-0x1.9d65a400p+27
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L222
	ret
L222:


	# Test 224
	# b32?s =0 -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L223
	ret
L223:


	# Test 225
	# b32?s =0 -0.7FFFFFP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L224
	ret
L224:


	# Test 226
	# b32?s =0 -0.30791CP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x8030791c	 #-0x1.83c8e000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L225
	ret
L225:


	# Test 227
	# b32?s =0 -0.000001P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L226
	ret
L226:


	# Test 228
	# b32?s =0 -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L227
	ret
L227:


	# Test 229
	# b32?s =0 -Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L228
	ret
L228:


	# Test 230
	# b32?s =0 +Zero -> 0x0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L229
	ret
L229:


	# Test 231
	# b32?s =0 +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L230
	ret
L230:


	# Test 232
	# b32?s =0 +0.000001P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L231
	ret
L231:


	# Test 233
	# b32?s =0 +0.200A20P-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x200a20	 #0x1.00510000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L232
	ret
L232:


	# Test 234
	# b32?s =0 +0.7FFFFFP-126 -> 0x1 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L233
	ret
L233:


	# Test 235
	# b32?s =0 +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L234
	ret
L234:


	# Test 236
	# b32?s =0 +1.0C3852P-67 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1e0c3852	 #0x1.1870a400p-67
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L235
	ret
L235:


	# Test 237
	# b32?s =0 +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L236
	ret
L236:


	# Test 238
	# b32?s =0 +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L237
	ret
L237:


	# Test 239
	# b32?s =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L238
	ret
L238:


	# Test 240
	# b32?s =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x24
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L239
	ret
L239:


	# Test 241
	# b32?0 =0 i -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L240
	ret
L240:


	# Test 242
	# b32?0 =0 i -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L241
	ret
L241:


	# Test 243
	# b32?0 =0 i -1.24E2BBP19 -> 0x0 


	addi a1, a1, 1
	li t0, 0xc924e2bb	 #-0x1.49c57600p+19
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L242
	ret
L242:


	# Test 244
	# b32?0 =0 i -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L243
	ret
L243:


	# Test 245
	# b32?0 =0 i -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L244
	ret
L244:


	# Test 246
	# b32?0 =0 i -0.5090EDP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x805090ed	 #-0x1.4243b400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L245
	ret
L245:


	# Test 247
	# b32?0 =0 i -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L246
	ret
L246:


	# Test 248
	# b32?0 =0 i -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L247
	ret
L247:


	# Test 249
	# b32?0 =0 i -Zero -> 0x1 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L248
	ret
L248:


	# Test 250
	# b32?0 =0 i +Zero -> 0x1 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L249
	ret
L249:


	# Test 251
	# b32?0 =0 i +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L250
	ret
L250:


	# Test 252
	# b32?0 =0 i +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L251
	ret
L251:


	# Test 253
	# b32?0 =0 i +0.62023DP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x62023d	 #0x1.8808f400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L252
	ret
L252:


	# Test 254
	# b32?0 =0 i +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L253
	ret
L253:


	# Test 255
	# b32?0 =0 i +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L254
	ret
L254:


	# Test 256
	# b32?0 =0 i +1.4D1023P101 -> 0x0 


	addi a1, a1, 1
	li t0, 0x724d1023	 #0x1.9a204600p+101
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L255
	ret
L255:


	# Test 257
	# b32?0 =0 i +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L256
	ret
L256:


	# Test 258
	# b32?0 =0 i +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L257
	ret
L257:


	# Test 259
	# b32?0 =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L258
	ret
L258:


	# Test 260
	# b32?0 =0 i q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L259
	ret
L259:


	# Test 261
	# b32?0 =0 -Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L260
	ret
L260:


	# Test 262
	# b32?0 =0 -1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L261
	ret
L261:


	# Test 263
	# b32?0 =0 -1.28EF17P-111 -> 0x0 


	addi a1, a1, 1
	li t0, 0x8828ef17	 #-0x1.51de2e00p-111
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L262
	ret
L262:


	# Test 264
	# b32?0 =0 -1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L263
	ret
L263:


	# Test 265
	# b32?0 =0 -0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L264
	ret
L264:


	# Test 266
	# b32?0 =0 -0.68D515P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x8068d515	 #-0x1.a3545400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L265
	ret
L265:


	# Test 267
	# b32?0 =0 -0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L266
	ret
L266:


	# Test 268
	# b32?0 =0 -1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L267
	ret
L267:


	# Test 269
	# b32?0 =0 -Zero -> 0x1 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L268
	ret
L268:


	# Test 270
	# b32?0 =0 +Zero -> 0x1 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x1
	beq t0, a0, L269
	ret
L269:


	# Test 271
	# b32?0 =0 +1.000000P0 -> 0x0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L270
	ret
L270:


	# Test 272
	# b32?0 =0 +0.000001P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L271
	ret
L271:


	# Test 273
	# b32?0 =0 +0.652E4DP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x652e4d	 #0x1.94b93400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L272
	ret
L272:


	# Test 274
	# b32?0 =0 +0.7FFFFFP-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L273
	ret
L273:


	# Test 275
	# b32?0 =0 +1.000000P-126 -> 0x0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L274
	ret
L274:


	# Test 276
	# b32?0 =0 +1.515C7FP-117 -> 0x0 


	addi a1, a1, 1
	li t0, 0x5515c7f	 #0x1.a2b8fe00p-117
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L275
	ret
L275:


	# Test 277
	# b32?0 =0 +1.7FFFFFP127 -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L276
	ret
L276:


	# Test 278
	# b32?0 =0 +Inf -> 0x0 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L277
	ret
L277:


	# Test 279
	# b32?0 =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L278
	ret
L278:


	# Test 280
	# b32?0 =0 q -> 0x0 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fclass.s a0, f1
	andi a0, a0, 0x18
	sltiu a0, a0, 1
	xori a0, a0, 1
	li t0, 0x0
	beq t0, a0, L279
	ret
L279:


	# Test 281
	# b32A =0 i -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L280
	ret
L280:


	# Test 282
	# b32A =0 i -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L281
	ret
L281:


	# Test 283
	# b32A =0 i -1.49EB5FP-6 -> +1.49EB5FP-6 


	addi a1, a1, 1
	li t0, 0xbcc9eb5f	 #-0x1.93d6be00p-6
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x3cc9eb5f	 #0x1.93d6be00p-6
	beq t0, a0, L282
	ret
L282:


	# Test 284
	# b32A =0 i -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L283
	ret
L283:


	# Test 285
	# b32A =0 i -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L284
	ret
L284:


	# Test 286
	# b32A =0 i -0.4AB1A9P-126 -> +0.4AB1A9P-126 


	addi a1, a1, 1
	li t0, 0x804ab1a9	 #-0x1.2ac6a400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x4ab1a9	 #0x1.2ac6a400p-127
	beq t0, a0, L285
	ret
L285:


	# Test 287
	# b32A =0 i -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L286
	ret
L286:


	# Test 288
	# b32A =0 i -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L287
	ret
L287:


	# Test 289
	# b32A =0 i -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L288
	ret
L288:


	# Test 290
	# b32A =0 i +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L289
	ret
L289:


	# Test 291
	# b32A =0 i +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L290
	ret
L290:


	# Test 292
	# b32A =0 i +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L291
	ret
L291:


	# Test 293
	# b32A =0 i +0.5B42ADP-126 -> +0.5B42ADP-126 


	addi a1, a1, 1
	li t0, 0x5b42ad	 #0x1.6d0ab400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x5b42ad	 #0x1.6d0ab400p-127
	beq t0, a0, L292
	ret
L292:


	# Test 294
	# b32A =0 i +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L293
	ret
L293:


	# Test 295
	# b32A =0 i +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L294
	ret
L294:


	# Test 296
	# b32A =0 i +1.0770DFP92 -> +1.0770DFP92 


	addi a1, a1, 1
	li t0, 0x6d8770df	 #0x1.0ee1be00p+92
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x6d8770df	 #0x1.0ee1be00p+92
	beq t0, a0, L295
	ret
L295:


	# Test 297
	# b32A =0 i +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L296
	ret
L296:


	# Test 298
	# b32A =0 i +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L297
	ret
L297:


	# Test 299
	# b32A =0 i q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L298
	ret
L298:


	# Test 300
	# b32A =0 i q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L299
	ret
L299:


	# Test 301
	# b32A =0 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L300
	ret
L300:


	# Test 302
	# b32A =0 -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L301
	ret
L301:


	# Test 303
	# b32A =0 -1.61AF87P77 -> +1.61AF87P77 


	addi a1, a1, 1
	li t0, 0xe661af87	 #-0x1.c35f0e00p+77
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x6661af87	 #0x1.c35f0e00p+77
	beq t0, a0, L302
	ret
L302:


	# Test 304
	# b32A =0 -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L303
	ret
L303:


	# Test 305
	# b32A =0 -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L304
	ret
L304:


	# Test 306
	# b32A =0 -0.6375D1P-126 -> +0.6375D1P-126 


	addi a1, a1, 1
	li t0, 0x806375d1	 #-0x1.8dd74400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x6375d1	 #0x1.8dd74400p-127
	beq t0, a0, L305
	ret
L305:


	# Test 307
	# b32A =0 -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L306
	ret
L306:


	# Test 308
	# b32A =0 -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L307
	ret
L307:


	# Test 309
	# b32A =0 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L308
	ret
L308:


	# Test 310
	# b32A =0 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L309
	ret
L309:


	# Test 311
	# b32A =0 +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L310
	ret
L310:


	# Test 312
	# b32A =0 +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L311
	ret
L311:


	# Test 313
	# b32A =0 +0.5E6EBDP-126 -> +0.5E6EBDP-126 


	addi a1, a1, 1
	li t0, 0x5e6ebd	 #0x1.79baf400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x5e6ebd	 #0x1.79baf400p-127
	beq t0, a0, L312
	ret
L312:


	# Test 314
	# b32A =0 +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L313
	ret
L313:


	# Test 315
	# b32A =0 +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L314
	ret
L314:


	# Test 316
	# b32A =0 +1.1F3507P79 -> +1.1F3507P79 


	addi a1, a1, 1
	li t0, 0x671f3507	 #0x1.3e6a0e00p+79
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x671f3507	 #0x1.3e6a0e00p+79
	beq t0, a0, L315
	ret
L315:


	# Test 317
	# b32A =0 +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L316
	ret
L316:


	# Test 318
	# b32A =0 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L317
	ret
L317:


	# Test 319
	# b32A =0 q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L318
	ret
L318:


	# Test 320
	# b32A =0 q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjx.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L319
	ret
L319:


	# Test 321
	# b32cp =0 i -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L320
	ret
L320:


	# Test 322
	# b32cp =0 i -1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L321
	ret
L321:


	# Test 323
	# b32cp =0 i -1.4654E4P-70 -> -1.4654E4P-70 


	addi a1, a1, 1
	li t0, 0x9cc654e4	 #-0x1.8ca9c800p-70
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff9cc654e4	 #-0x1.8ca9c800p-70
	beq t0, a0, L322
	ret
L322:


	# Test 324
	# b32cp =0 i -1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L323
	ret
L323:


	# Test 325
	# b32cp =0 i -0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L324
	ret
L324:


	# Test 326
	# b32cp =0 i -0.463AE3P-126 -> -0.463AE3P-126 


	addi a1, a1, 1
	li t0, 0x80463ae3	 #-0x1.18eb8c00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80463ae3	 #-0x1.18eb8c00p-127
	beq t0, a0, L325
	ret
L325:


	# Test 327
	# b32cp =0 i -0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L326
	ret
L326:


	# Test 328
	# b32cp =0 i -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L327
	ret
L327:


	# Test 329
	# b32cp =0 i -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L328
	ret
L328:


	# Test 330
	# b32cp =0 i +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L329
	ret
L329:


	# Test 331
	# b32cp =0 i +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L330
	ret
L330:


	# Test 332
	# b32cp =0 i +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L331
	ret
L331:


	# Test 333
	# b32cp =0 i +0.57AC33P-126 -> +0.57AC33P-126 


	addi a1, a1, 1
	li t0, 0x57ac33	 #0x1.5eb0cc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x57ac33	 #0x1.5eb0cc00p-127
	beq t0, a0, L332
	ret
L332:


	# Test 334
	# b32cp =0 i +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L333
	ret
L333:


	# Test 335
	# b32cp =0 i +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L334
	ret
L334:


	# Test 336
	# b32cp =0 i +1.03DA64P92 -> +1.03DA64P92 


	addi a1, a1, 1
	li t0, 0x6d83da64	 #0x1.07b4c800p+92
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x6d83da64	 #0x1.07b4c800p+92
	beq t0, a0, L335
	ret
L335:


	# Test 337
	# b32cp =0 i +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L336
	ret
L336:


	# Test 338
	# b32cp =0 i +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L337
	ret
L337:


	# Test 339
	# b32cp =0 i q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L338
	ret
L338:


	# Test 340
	# b32cp =0 i q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L339
	ret
L339:


	# Test 341
	# b32cp =0 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L340
	ret
L340:


	# Test 342
	# b32cp =0 -1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L341
	ret
L341:


	# Test 343
	# b32cp =0 -1.5E590CP109 -> -1.5E590CP109 


	addi a1, a1, 1
	li t0, 0xf65e590c	 #-0x1.bcb21800p+109
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xfffffffff65e590c	 #-0x1.bcb21800p+109
	beq t0, a0, L342
	ret
L342:


	# Test 344
	# b32cp =0 -1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L343
	ret
L343:


	# Test 345
	# b32cp =0 -0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L344
	ret
L344:


	# Test 346
	# b32cp =0 -0.2A473EP-126 -> -0.2A473EP-126 


	addi a1, a1, 1
	li t0, 0x802a473e	 #-0x1.5239f000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff802a473e	 #-0x1.5239f000p-128
	beq t0, a0, L345
	ret
L345:


	# Test 347
	# b32cp =0 -0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L346
	ret
L346:


	# Test 348
	# b32cp =0 -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L347
	ret
L347:


	# Test 349
	# b32cp =0 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L348
	ret
L348:


	# Test 350
	# b32cp =0 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L349
	ret
L349:


	# Test 351
	# b32cp =0 +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L350
	ret
L350:


	# Test 352
	# b32cp =0 +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L351
	ret
L351:


	# Test 353
	# b32cp =0 +0.3AD842P-126 -> +0.3AD842P-126 


	addi a1, a1, 1
	li t0, 0x3ad842	 #0x1.d6c21000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x3ad842	 #0x1.d6c21000p-128
	beq t0, a0, L352
	ret
L352:


	# Test 354
	# b32cp =0 +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L353
	ret
L353:


	# Test 355
	# b32cp =0 +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L354
	ret
L354:


	# Test 356
	# b32cp =0 +1.1B9E8DP15 -> +1.1B9E8DP15 


	addi a1, a1, 1
	li t0, 0x471b9e8d	 #0x1.373d1a00p+15
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x471b9e8d	 #0x1.373d1a00p+15
	beq t0, a0, L355
	ret
L355:


	# Test 357
	# b32cp =0 +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L356
	ret
L356:


	# Test 358
	# b32cp =0 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L357
	ret
L357:


	# Test 359
	# b32cp =0 q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L358
	ret
L358:


	# Test 360
	# b32cp =0 q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmv.s f4, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L359
	ret
L359:


	# Test 361
	# b32~ =0 i -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L360
	ret
L360:


	# Test 362
	# b32~ =0 i -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L361
	ret
L361:


	# Test 363
	# b32~ =0 i -1.1A1F4BP-45 -> +1.1A1F4BP-45 


	addi a1, a1, 1
	li t0, 0xa91a1f4b	 #-0x1.343e9600p-45
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x291a1f4b	 #0x1.343e9600p-45
	beq t0, a0, L362
	ret
L362:


	# Test 364
	# b32~ =0 i -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L363
	ret
L363:


	# Test 365
	# b32~ =0 i -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L364
	ret
L364:


	# Test 366
	# b32~ =0 i -0.464D7DP-126 -> +0.464D7DP-126 


	addi a1, a1, 1
	li t0, 0x80464d7d	 #-0x1.1935f400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x464d7d	 #0x1.1935f400p-127
	beq t0, a0, L365
	ret
L365:


	# Test 367
	# b32~ =0 i -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L366
	ret
L366:


	# Test 368
	# b32~ =0 i -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L367
	ret
L367:


	# Test 369
	# b32~ =0 i -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L368
	ret
L368:


	# Test 370
	# b32~ =0 i +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L369
	ret
L369:


	# Test 371
	# b32~ =0 i +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L370
	ret
L370:


	# Test 372
	# b32~ =0 i +0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L371
	ret
L371:


	# Test 373
	# b32~ =0 i +0.561E81P-126 -> -0.561E81P-126 


	addi a1, a1, 1
	li t0, 0x561e81	 #0x1.587a0400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80561e81	 #-0x1.587a0400p-127
	beq t0, a0, L372
	ret
L372:


	# Test 374
	# b32~ =0 i +0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L373
	ret
L373:


	# Test 375
	# b32~ =0 i +1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L374
	ret
L374:


	# Test 376
	# b32~ =0 i +1.57A4CBP100 -> -1.57A4CBP100 


	addi a1, a1, 1
	li t0, 0x71d7a4cb	 #0x1.af499600p+100
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xfffffffff1d7a4cb	 #-0x1.af499600p+100
	beq t0, a0, L375
	ret
L375:


	# Test 377
	# b32~ =0 i +1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L376
	ret
L376:


	# Test 378
	# b32~ =0 i +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L377
	ret
L377:


	# Test 379
	# b32~ =0 i q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L378
	ret
L378:


	# Test 380
	# b32~ =0 i q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L379
	ret
L379:


	# Test 381
	# b32~ =0 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L380
	ret
L380:


	# Test 382
	# b32~ =0 -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L381
	ret
L381:


	# Test 383
	# b32~ =0 -1.1D0B5BP-25 -> +1.1D0B5BP-25 


	addi a1, a1, 1
	li t0, 0xb31d0b5b	 #-0x1.3a16b600p-25
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x331d0b5b	 #0x1.3a16b600p-25
	beq t0, a0, L382
	ret
L382:


	# Test 384
	# b32~ =0 -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L383
	ret
L383:


	# Test 385
	# b32~ =0 -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L384
	ret
L384:


	# Test 386
	# b32~ =0 -0.49398CP-126 -> +0.49398CP-126 


	addi a1, a1, 1
	li t0, 0x8049398c	 #-0x1.24e63000p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x49398c	 #0x1.24e63000p-127
	beq t0, a0, L385
	ret
L385:


	# Test 387
	# b32~ =0 -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L386
	ret
L386:


	# Test 388
	# b32~ =0 -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L387
	ret
L387:


	# Test 389
	# b32~ =0 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L388
	ret
L388:


	# Test 390
	# b32~ =0 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L389
	ret
L389:


	# Test 391
	# b32~ =0 +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L390
	ret
L390:


	# Test 392
	# b32~ =0 +0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L391
	ret
L391:


	# Test 393
	# b32~ =0 +0.6EE2A9P-126 -> -0.6EE2A9P-126 


	addi a1, a1, 1
	li t0, 0x6ee2a9	 #0x1.bb8aa400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff806ee2a9	 #-0x1.bb8aa400p-127
	beq t0, a0, L392
	ret
L392:


	# Test 394
	# b32~ =0 +0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L393
	ret
L393:


	# Test 395
	# b32~ =0 +1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L394
	ret
L394:


	# Test 396
	# b32~ =0 +1.5A90DBP-120 -> -1.5A90DBP-120 


	addi a1, a1, 1
	li t0, 0x3da90db	 #0x1.b521b600p-120
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffff83da90db	 #-0x1.b521b600p-120
	beq t0, a0, L395
	ret
L395:


	# Test 397
	# b32~ =0 +1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L396
	ret
L396:


	# Test 398
	# b32~ =0 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L397
	ret
L397:


	# Test 399
	# b32~ =0 q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L398
	ret
L398:


	# Test 400
	# b32~ =0 q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsgnjn.s f4, f1, f1
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L399
	ret
L399:


	# Test 401
	# b32+ =0 i -Inf -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L400
	ret
L400:


	# Test 402
	# b32+ =0 i -1.7FFFFFP127 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L401
	ret
L401:


	# Test 403
	# b32+ =0 i -1.1C2060P21 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xca1c2060	 #-0x1.3840c000p+21
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L402
	ret
L402:


	# Test 404
	# b32+ =0 i -1.000000P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L403
	ret
L403:


	# Test 405
	# b32+ =0 i -0.7FFFFFP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L404
	ret
L404:


	# Test 406
	# b32+ =0 i -0.243F7FP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80243f7f	 #-0x1.21fbf800p-128
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L405
	ret
L405:


	# Test 407
	# b32+ =0 i -0.000001P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L406
	ret
L406:


	# Test 408
	# b32+ =0 i -1.000000P0 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L407
	ret
L407:


	# Test 409
	# b32+ =0 i -Zero -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L408
	ret
L408:


	# Test 410
	# b32+ =0 i +Zero -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L409
	ret
L409:


	# Test 411
	# b32+ =0 i +1.000000P0 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L410
	ret
L410:


	# Test 412
	# b32+ =0 i +0.000001P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L411
	ret
L411:


	# Test 413
	# b32+ =0 i +0.3EE03BP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3ee03b	 #0x1.f701d800p-128
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L412
	ret
L412:


	# Test 414
	# b32+ =0 i +0.7FFFFFP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L413
	ret
L413:


	# Test 415
	# b32+ =0 i +1.000000P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L414
	ret
L414:


	# Test 416
	# b32+ =0 i +1.504F3EP-18 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x36d04f3e	 #0x1.a09e7c00p-18
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L415
	ret
L415:


	# Test 417
	# b32+ =0 i +1.7FFFFFP127 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L416
	ret
L416:


	# Test 418
	# b32+ =0 i -Inf -1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L417
	ret
L417:


	# Test 419
	# b32+ =0 i -1.7FFFFFP127 -1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L418
	ret
L418:


	# Test 420
	# b32+ =0 i -1.6A851CP67 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xe16a851c	 #-0x1.d50a3800p+67
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L419
	ret
L419:


	# Test 421
	# b32+ =0 i -1.000000P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L420
	ret
L420:


	# Test 422
	# b32+ =0 i -0.7FFFFFP-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L421
	ret
L421:


	# Test 423
	# b32+ =0 i -0.287C52P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80287c52	 #-0x1.43e29000p-128
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L422
	ret
L422:


	# Test 424
	# b32+ =0 i -0.000001P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L423
	ret
L423:


	# Test 425
	# b32+ =0 i -1.000000P0 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L424
	ret
L424:


	# Test 426
	# b32+ =0 i -Zero -1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L425
	ret
L425:


	# Test 427
	# b32+ =0 i +Zero -1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L426
	ret
L426:


	# Test 428
	# b32+ =0 i +1.000000P0 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L427
	ret
L427:


	# Test 429
	# b32+ =0 i +0.000001P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L428
	ret
L428:


	# Test 430
	# b32+ =0 i +0.613CC3P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x613cc3	 #0x1.84f30c00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L429
	ret
L429:


	# Test 431
	# b32+ =0 i +0.7FFFFFP-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L430
	ret
L430:


	# Test 432
	# b32+ =0 i +1.000000P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L431
	ret
L431:


	# Test 433
	# b32+ =0 i +1.1F73F9P-35 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x2e1f73f9	 #0x1.3ee7f200p-35
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L432
	ret
L432:


	# Test 434
	# b32+ =0 i +1.7FFFFFP127 -1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L433
	ret
L433:


	# Test 435
	# b32+ =0 i +Inf -1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L434
	ret
L434:


	# Test 436
	# b32+ =0 i -Inf -1.24CDEBP61 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xde24cdeb	 #-0x1.499bd600p+61
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L435
	ret
L435:


	# Test 437
	# b32+ =0 i -1.7FFFFFP127 -1.1B70A7P-108 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x899b70a7	 #-0x1.36e14e00p-108
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L436
	ret
L436:


	# Test 438
	# b32+ =0 i -1.3929D7P-14 -1.44E63BP107 -> -1.44E63BP107 x


	addi a1, a1, 1
	li t0, 0xb8b929d7	 #-0x1.7253ae00p-14
	fmv.s.x f1, t0
	li t0, 0xf544e63b	 #-0x1.89cc7600p+107
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff544e63b	 #-0x1.89cc7600p+107
	beq t0, a0, L437
	ret
L437:


	# Test 439
	# b32+ =0 i -1.000000P-126 -1.7A5BBDP97 -> -1.7A5BBDP97 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xf07a5bbd	 #-0x1.f4b77a00p+97
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff07a5bbd	 #-0x1.f4b77a00p+97
	beq t0, a0, L438
	ret
L438:


	# Test 440
	# b32+ =0 i -0.7FFFFFP-126 -1.70BE78P88 -> -1.70BE78P88 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xebf0be78	 #-0x1.e17cf000p+88
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffebf0be78	 #-0x1.e17cf000p+88
	beq t0, a0, L439
	ret
L439:


	# Test 441
	# b32+ =0 i -0.37210EP-126 -1.4D7C52P68 -> -1.4D7C52P68 x


	addi a1, a1, 1
	li t0, 0x8037210e	 #-0x1.b9087000p-128
	fmv.s.x f1, t0
	li t0, 0xe1cd7c52	 #-0x1.9af8a400p+68
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe1cd7c52	 #-0x1.9af8a400p+68
	beq t0, a0, L440
	ret
L440:


	# Test 442
	# b32+ =0 i -0.000001P-126 -1.06BD47P3 -> -1.06BD47P3 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xc106bd47	 #-0x1.0d7a8e00p+3
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc106bd47	 #-0x1.0d7a8e00p+3
	beq t0, a0, L441
	ret
L441:


	# Test 443
	# b32+ =0 i -1.000000P0 -1.125CAFP8 -> -1.12DCAFP8 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xc3925caf	 #-0x1.24b95e00p+8
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc392dcaf	 #-0x1.25b95e00p+8
	beq t0, a0, L442
	ret
L442:


	# Test 444
	# b32+ =0 i -Zero -1.512BB1P-46 -> -1.512BB1P-46 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xa8d12bb1	 #-0x1.a2576200p-46
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa8d12bb1	 #-0x1.a2576200p-46
	beq t0, a0, L443
	ret
L443:


	# Test 445
	# b32+ =0 i +Zero -1.5C0B19P-41 -> -1.5C0B19P-41 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xab5c0b19	 #-0x1.b8163200p-41
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffab5c0b19	 #-0x1.b8163200p-41
	beq t0, a0, L444
	ret
L444:


	# Test 446
	# b32+ =0 i +1.000000P0 -1.7C672BP58 -> -1.7C672BP58 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xdcfc672b	 #-0x1.f8ce5600p+58
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffdcfc672b	 #-0x1.f8ce5600p+58
	beq t0, a0, L445
	ret
L445:


	# Test 447
	# b32+ =0 i +0.000001P-126 -1.72C9E7P-119 -> -1.72C9E7P-119 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x8472c9e7	 #-0x1.e593ce00p-119
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8472c9e7	 #-0x1.e593ce00p-119
	beq t0, a0, L446
	ret
L446:


	# Test 448
	# b32+ =0 i +0.70217EP-126 -1.5A4281P71 -> -1.5A4281P71 x


	addi a1, a1, 1
	li t0, 0x70217e	 #0x1.c085f800p-127
	fmv.s.x f1, t0
	li t0, 0xe35a4281	 #-0x1.b4850200p+71
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe35a4281	 #-0x1.b4850200p+71
	beq t0, a0, L447
	ret
L447:


	# Test 449
	# b32+ =0 i +0.7FFFFFP-126 -1.5134FDP-49 -> -1.5134FDP-49 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xa75134fd	 #-0x1.a269fa00p-49
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa75134fd	 #-0x1.a269fa00p-49
	beq t0, a0, L448
	ret
L448:


	# Test 450
	# b32+ =0 i +1.000000P-126 -1.5CD464P-76 -> -1.5CD464P-76 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x99dcd464	 #-0x1.b9a8c800p-76
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff99dcd464	 #-0x1.b9a8c800p-76
	beq t0, a0, L449
	ret
L449:


	# Test 451
	# b32+ =0 i +1.6D98B5P-53 -1.62D898P0 -> -1.62D898P0 x


	addi a1, a1, 1
	li t0, 0x256d98b5	 #0x1.db316a00p-53
	fmv.s.x f1, t0
	li t0, 0xbfe2d898	 #-0x1.c5b13000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbfe2d898	 #-0x1.c5b13000p+0
	beq t0, a0, L450
	ret
L450:


	# Test 452
	# b32+ =0 i +1.7FFFFFP127 -1.5E1688P-64 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x9fde1688	 #-0x1.bc2d1000p-64
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L451
	ret
L451:


	# Test 453
	# b32+ =0 i +Inf -1.6975EFP-59 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xa26975ef	 #-0x1.d2ebde00p-59
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L452
	ret
L452:


	# Test 454
	# b32+ =0 i -Inf -1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L453
	ret
L453:


	# Test 455
	# b32+ =0 i -1.7FFFFFP127 -1.000000P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L454
	ret
L454:


	# Test 456
	# b32+ =0 i -1.084E92P-31 -1.000000P-126 -> -1.084E92P-31 x


	addi a1, a1, 1
	li t0, 0xb0084e92	 #-0x1.109d2400p-31
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb0084e92	 #-0x1.109d2400p-31
	beq t0, a0, L455
	ret
L455:


	# Test 457
	# b32+ =0 i -1.000000P-126 -1.000000P-126 -> -1.000000P-125 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff81000000	 #-0x1.00000000p-125
	beq t0, a0, L456
	ret
L456:


	# Test 458
	# b32+ =0 i -0.7FFFFFP-126 -1.000000P-126 -> -1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80ffffff	 #-0x1.fffffe00p-126
	beq t0, a0, L457
	ret
L457:


	# Test 459
	# b32+ =0 i -0.2585C9P-126 -1.000000P-126 -> -1.2585C9P-126 


	addi a1, a1, 1
	li t0, 0x802585c9	 #-0x1.2c2e4800p-128
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80a585c9	 #-0x1.4b0b9200p-126
	beq t0, a0, L458
	ret
L458:


	# Test 460
	# b32+ =0 i -0.000001P-126 -1.000000P-126 -> -1.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800001	 #-0x1.00000200p-126
	beq t0, a0, L459
	ret
L459:


	# Test 461
	# b32+ =0 i -1.000000P0 -1.000000P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L460
	ret
L460:


	# Test 462
	# b32+ =0 i -Zero -1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L461
	ret
L461:


	# Test 463
	# b32+ =0 i +Zero -1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L462
	ret
L462:


	# Test 464
	# b32+ =0 i +1.000000P0 -1.000000P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L463
	ret
L463:


	# Test 465
	# b32+ =0 i +0.000001P-126 -1.000000P-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L464
	ret
L464:


	# Test 466
	# b32+ =0 i +0.545E52P-126 -1.000000P-126 -> -0.2BA1AEP-126 


	addi a1, a1, 1
	li t0, 0x545e52	 #0x1.51794800p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff802ba1ae	 #-0x1.5d0d7000p-128
	beq t0, a0, L465
	ret
L465:


	# Test 467
	# b32+ =0 i +0.7FFFFFP-126 -1.000000P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L466
	ret
L466:


	# Test 468
	# b32+ =0 i +1.000000P-126 -1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L467
	ret
L467:


	# Test 469
	# b32+ =0 i +1.3C3D70P106 -1.000000P-126 -> +1.3C3D70P106 x


	addi a1, a1, 1
	li t0, 0x74bc3d70	 #0x1.787ae000p+106
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x74bc3d70	 #0x1.787ae000p+106
	beq t0, a0, L468
	ret
L468:


	# Test 470
	# b32+ =0 i +1.7FFFFFP127 -1.000000P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L469
	ret
L469:


	# Test 471
	# b32+ =0 i +Inf -1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L470
	ret
L470:


	# Test 472
	# b32+ =0 i -Inf -0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L471
	ret
L471:


	# Test 473
	# b32+ =0 i -1.7FFFFFP127 -0.7FFFFFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L472
	ret
L472:


	# Test 474
	# b32+ =0 i -1.56F34DP-49 -0.7FFFFFP-126 -> -1.56F34DP-49 x


	addi a1, a1, 1
	li t0, 0xa756f34d	 #-0x1.ade69a00p-49
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa756f34d	 #-0x1.ade69a00p-49
	beq t0, a0, L473
	ret
L473:


	# Test 475
	# b32+ =0 i -1.000000P-126 -0.7FFFFFP-126 -> -1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80ffffff	 #-0x1.fffffe00p-126
	beq t0, a0, L474
	ret
L474:


	# Test 476
	# b32+ =0 i -0.7FFFFFP-126 -0.7FFFFFP-126 -> -1.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80fffffe	 #-0x1.fffffc00p-126
	beq t0, a0, L475
	ret
L475:


	# Test 477
	# b32+ =0 i -0.14AA84P-126 -0.7FFFFFP-126 -> -1.14AA83P-126 


	addi a1, a1, 1
	li t0, 0x8014aa84	 #-0x1.4aa84000p-129
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8094aa83	 #-0x1.29550600p-126
	beq t0, a0, L476
	ret
L476:


	# Test 478
	# b32+ =0 i -0.000001P-126 -0.7FFFFFP-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L477
	ret
L477:


	# Test 479
	# b32+ =0 i -1.000000P0 -0.7FFFFFP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L478
	ret
L478:


	# Test 480
	# b32+ =0 i -Zero -0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L479
	ret
L479:


	# Test 481
	# b32+ =0 i +Zero -0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L480
	ret
L480:


	# Test 482
	# b32+ =0 i +1.000000P0 -0.7FFFFFP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L481
	ret
L481:


	# Test 483
	# b32+ =0 i +0.000001P-126 -0.7FFFFFP-126 -> -0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ffffe	 #-0x1.fffff800p-127
	beq t0, a0, L482
	ret
L482:


	# Test 484
	# b32+ =0 i +0.62830DP-126 -0.7FFFFFP-126 -> -0.1D7CF2P-126 


	addi a1, a1, 1
	li t0, 0x62830d	 #0x1.8a0c3400p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff801d7cf2	 #-0x1.d7cf2000p-129
	beq t0, a0, L483
	ret
L483:


	# Test 485
	# b32+ =0 i +0.7FFFFFP-126 -0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L484
	ret
L484:


	# Test 486
	# b32+ =0 i +1.000000P-126 -0.7FFFFFP-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L485
	ret
L485:


	# Test 487
	# b32+ =0 i +1.203A44P-24 -0.7FFFFFP-126 -> +1.203A44P-24 x


	addi a1, a1, 1
	li t0, 0x33a03a44	 #0x1.40748800p-24
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x33a03a44	 #0x1.40748800p-24
	beq t0, a0, L486
	ret
L486:


	# Test 488
	# b32+ =0 i +1.7FFFFFP127 -0.7FFFFFP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L487
	ret
L487:


	# Test 489
	# b32+ =0 i +Inf -0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L488
	ret
L488:


	# Test 490
	# b32+ =0 i -Inf -0.31D0D7P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x8031d0d7	 #-0x1.8e86b800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L489
	ret
L489:


	# Test 491
	# b32+ =0 i -1.7FFFFFP127 -0.7CF03FP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807cf03f	 #-0x1.f3c0fc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L490
	ret
L490:


	# Test 492
	# b32+ =0 i -1.3AB021P-3 -0.6EEF46P-126 -> -1.3AB021P-3 x


	addi a1, a1, 1
	li t0, 0xbe3ab021	 #-0x1.75604200p-3
	fmv.s.x f1, t0
	li t0, 0x806eef46	 #-0x1.bbbd1800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbe3ab021	 #-0x1.75604200p-3
	beq t0, a0, L491
	ret
L491:


	# Test 493
	# b32+ =0 i -1.000000P-126 -0.7E3262P-126 -> -1.7E3262P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807e3262	 #-0x1.f8c98800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80fe3262	 #-0x1.fc64c400p-126
	beq t0, a0, L492
	ret
L492:


	# Test 494
	# b32+ =0 i -0.7FFFFFP-126 -0.2991CAP-126 -> -1.2991C9P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x802991ca	 #-0x1.4c8e5000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80a991c9	 #-0x1.53239200p-126
	beq t0, a0, L493
	ret
L493:


	# Test 495
	# b32+ =0 i -0.134F40P-126 -0.631C9FP-126 -> -0.766BDFP-126 


	addi a1, a1, 1
	li t0, 0x80134f40	 #-0x1.34f40000p-129
	fmv.s.x f1, t0
	li t0, 0x80631c9f	 #-0x1.8c727c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80766bdf	 #-0x1.d9af7c00p-127
	beq t0, a0, L494
	ret
L494:


	# Test 496
	# b32+ =0 i -0.000001P-126 -0.28BCDFP-126 -> -0.28BCE0P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x8028bcdf	 #-0x1.45e6f800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8028bce0	 #-0x1.45e70000p-128
	beq t0, a0, L495
	ret
L495:


	# Test 497
	# b32+ =0 i -1.000000P0 -0.73DC46P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8073dc46	 #-0x1.cf711800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L496
	ret
L496:


	# Test 498
	# b32+ =0 i -Zero -0.1EFBAEP-126 -> -0.1EFBAEP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x801efbae	 #-0x1.efbae000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff801efbae	 #-0x1.efbae000p-129
	beq t0, a0, L497
	ret
L497:


	# Test 499
	# b32+ =0 i +Zero -0.7DCAB0P-126 -> -0.7DCAB0P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807dcab0	 #-0x1.f72ac000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807dcab0	 #-0x1.f72ac000p-127
	beq t0, a0, L498
	ret
L498:


	# Test 500
	# b32+ =0 i +1.000000P0 -0.18AA18P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8018aa18	 #-0x1.8aa18000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L499
	ret
L499:


	# Test 501
	# b32+ =0 i +0.000001P-126 -0.54497FP-126 -> -0.54497EP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x8054497f	 #-0x1.5125fc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8054497e	 #-0x1.5125f800p-127
	beq t0, a0, L500
	ret
L500:


	# Test 502
	# b32+ =0 i +0.7127C8P-126 -0.244B8DP-126 -> +0.4CDC3BP-126 


	addi a1, a1, 1
	li t0, 0x7127c8	 #0x1.c49f2000p-127
	fmv.s.x f1, t0
	li t0, 0x80244b8d	 #-0x1.225c6800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4cdc3b	 #0x1.3370ec00p-127
	beq t0, a0, L501
	ret
L501:


	# Test 503
	# b32+ =0 i +0.7FFFFFP-126 -0.558BA2P-126 -> +0.2A745DP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80558ba2	 #-0x1.562e8800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2a745d	 #0x1.53a2e800p-128
	beq t0, a0, L502
	ret
L502:


	# Test 504
	# b32+ =0 i +1.000000P-126 -0.195750P-126 -> +0.66A8B0P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80195750	 #-0x1.95750000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x66a8b0	 #0x1.9aa2c000p-127
	beq t0, a0, L503
	ret
L503:


	# Test 505
	# b32+ =0 i +1.59C6E6P23 -0.18F8E5P-126 -> +1.59C6E6P23 x


	addi a1, a1, 1
	li t0, 0x4b59c6e6	 #0x1.b38dcc00p+23
	fmv.s.x f1, t0
	li t0, 0x8018f8e5	 #-0x1.8f8e5000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4b59c6e6	 #0x1.b38dcc00p+23
	beq t0, a0, L504
	ret
L504:


	# Test 506
	# b32+ =0 i +1.7FFFFFP127 -0.2A9974P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x802a9974	 #-0x1.54cba000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L505
	ret
L505:


	# Test 507
	# b32+ =0 i +Inf -0.7678DBP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807678db	 #-0x1.d9e36c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L506
	ret
L506:


	# Test 508
	# b32+ =0 i -Inf -0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L507
	ret
L507:


	# Test 509
	# b32+ =0 i -1.7FFFFFP127 -0.000001P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L508
	ret
L508:


	# Test 510
	# b32+ =0 i -1.09D4DDP-20 -0.000001P-126 -> -1.09D4DDP-20 x


	addi a1, a1, 1
	li t0, 0xb589d4dd	 #-0x1.13a9ba00p-20
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb589d4dd	 #-0x1.13a9ba00p-20
	beq t0, a0, L509
	ret
L509:


	# Test 511
	# b32+ =0 i -1.000000P-126 -0.000001P-126 -> -1.000001P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800001	 #-0x1.00000200p-126
	beq t0, a0, L510
	ret
L510:


	# Test 512
	# b32+ =0 i -0.7FFFFFP-126 -0.000001P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L511
	ret
L511:


	# Test 513
	# b32+ =0 i -0.170C13P-126 -0.000001P-126 -> -0.170C14P-126 


	addi a1, a1, 1
	li t0, 0x80170c13	 #-0x1.70c13000p-129
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80170c14	 #-0x1.70c14000p-129
	beq t0, a0, L512
	ret
L512:


	# Test 514
	# b32+ =0 i -0.000001P-126 -0.000001P-126 -> -0.000002P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000002	 #-0x1.00000000p-148
	beq t0, a0, L513
	ret
L513:


	# Test 515
	# b32+ =0 i -1.000000P0 -0.000001P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L514
	ret
L514:


	# Test 516
	# b32+ =0 i -Zero -0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L515
	ret
L515:


	# Test 517
	# b32+ =0 i +Zero -0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L516
	ret
L516:


	# Test 518
	# b32+ =0 i +1.000000P0 -0.000001P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L517
	ret
L517:


	# Test 519
	# b32+ =0 i +0.000001P-126 -0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L518
	ret
L518:


	# Test 520
	# b32+ =0 i +0.204C83P-126 -0.000001P-126 -> +0.204C82P-126 


	addi a1, a1, 1
	li t0, 0x204c83	 #0x1.02641800p-128
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x204c82	 #0x1.02641000p-128
	beq t0, a0, L519
	ret
L519:


	# Test 521
	# b32+ =0 i +0.7FFFFFP-126 -0.000001P-126 -> +0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7ffffe	 #0x1.fffff800p-127
	beq t0, a0, L520
	ret
L520:


	# Test 522
	# b32+ =0 i +1.000000P-126 -0.000001P-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L521
	ret
L521:


	# Test 523
	# b32+ =0 i +1.28EBA2P-90 -0.000001P-126 -> +1.28EBA2P-90 x


	addi a1, a1, 1
	li t0, 0x12a8eba2	 #0x1.51d74400p-90
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x12a8eba2	 #0x1.51d74400p-90
	beq t0, a0, L522
	ret
L522:


	# Test 524
	# b32+ =0 i +1.7FFFFFP127 -0.000001P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L523
	ret
L523:


	# Test 525
	# b32+ =0 i +Inf -0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L524
	ret
L524:


	# Test 526
	# b32+ =0 i -Inf -1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L525
	ret
L525:


	# Test 527
	# b32+ =0 i -1.7FFFFFP127 -1.000000P0 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L526
	ret
L526:


	# Test 528
	# b32+ =0 i -1.583998P26 -1.000000P0 -> -1.583998P26 x


	addi a1, a1, 1
	li t0, 0xccd83998	 #-0x1.b0733000p+26
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffccd83998	 #-0x1.b0733000p+26
	beq t0, a0, L527
	ret
L527:


	# Test 529
	# b32+ =0 i -1.000000P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L528
	ret
L528:


	# Test 530
	# b32+ =0 i -0.7FFFFFP-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L529
	ret
L529:


	# Test 531
	# b32+ =0 i -0.0038B6P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800038b6	 #-0x1.c5b00000p-136
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L530
	ret
L530:


	# Test 532
	# b32+ =0 i -0.000001P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L531
	ret
L531:


	# Test 533
	# b32+ =0 i -1.000000P0 -1.000000P0 -> -1.000000P1 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0000000	 #-0x1.00000000p+1
	beq t0, a0, L532
	ret
L532:


	# Test 534
	# b32+ =0 i -Zero -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L533
	ret
L533:


	# Test 535
	# b32+ =0 i +Zero -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L534
	ret
L534:


	# Test 536
	# b32+ =0 i +1.000000P0 -1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L535
	ret
L535:


	# Test 537
	# b32+ =0 i +0.000001P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L536
	ret
L536:


	# Test 538
	# b32+ =0 i +0.399926P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x399926	 #0x1.ccc93000p-128
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L537
	ret
L537:


	# Test 539
	# b32+ =0 i +0.7FFFFFP-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L538
	ret
L538:


	# Test 540
	# b32+ =0 i +1.000000P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L539
	ret
L539:


	# Test 541
	# b32+ =0 i +1.77105DP-76 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x19f7105d	 #0x1.ee20ba00p-76
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L540
	ret
L540:


	# Test 542
	# b32+ =0 i +1.7FFFFFP127 -1.000000P0 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L541
	ret
L541:


	# Test 543
	# b32+ =0 i +Inf -1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L542
	ret
L542:


	# Test 544
	# b32+ =0 i -Inf -Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L543
	ret
L543:


	# Test 545
	# b32+ =0 i -1.7FFFFFP127 -Zero -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L544
	ret
L544:


	# Test 546
	# b32+ =0 i -1.26DE53P9 -Zero -> -1.26DE53P9 


	addi a1, a1, 1
	li t0, 0xc426de53	 #-0x1.4dbca600p+9
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc426de53	 #-0x1.4dbca600p+9
	beq t0, a0, L545
	ret
L545:


	# Test 547
	# b32+ =0 i -1.000000P-126 -Zero -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L546
	ret
L546:


	# Test 548
	# b32+ =0 i -0.7FFFFFP-126 -Zero -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L547
	ret
L547:


	# Test 549
	# b32+ =0 i -0.23353EP-126 -Zero -> -0.23353EP-126 


	addi a1, a1, 1
	li t0, 0x8023353e	 #-0x1.19a9f000p-128
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8023353e	 #-0x1.19a9f000p-128
	beq t0, a0, L548
	ret
L548:


	# Test 550
	# b32+ =0 i -0.000001P-126 -Zero -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L549
	ret
L549:


	# Test 551
	# b32+ =0 i -1.000000P0 -Zero -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L550
	ret
L550:


	# Test 552
	# b32+ =0 i -Zero -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L551
	ret
L551:


	# Test 553
	# b32+ =0 i +Zero -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L552
	ret
L552:


	# Test 554
	# b32+ =0 i +1.000000P0 -Zero -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L553
	ret
L553:


	# Test 555
	# b32+ =0 i +0.000001P-126 -Zero -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L554
	ret
L554:


	# Test 556
	# b32+ =0 i +0.3DD5FAP-126 -Zero -> +0.3DD5FAP-126 


	addi a1, a1, 1
	li t0, 0x3dd5fa	 #0x1.eeafd000p-128
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3dd5fa	 #0x1.eeafd000p-128
	beq t0, a0, L555
	ret
L555:


	# Test 557
	# b32+ =0 i +0.7FFFFFP-126 -Zero -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L556
	ret
L556:


	# Test 558
	# b32+ =0 i +1.000000P-126 -Zero -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L557
	ret
L557:


	# Test 559
	# b32+ =0 i +1.463518P-93 -Zero -> +1.463518P-93 


	addi a1, a1, 1
	li t0, 0x11463518	 #0x1.8c6a3000p-93
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x11463518	 #0x1.8c6a3000p-93
	beq t0, a0, L558
	ret
L558:


	# Test 560
	# b32+ =0 i +1.7FFFFFP127 -Zero -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L559
	ret
L559:


	# Test 561
	# b32+ =0 i +Inf -Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L560
	ret
L560:


	# Test 562
	# b32+ =0 i -Inf +Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L561
	ret
L561:


	# Test 563
	# b32+ =0 i -1.7FFFFFP127 +Zero -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L562
	ret
L562:


	# Test 564
	# b32+ =0 i -1.75830EP55 +Zero -> -1.75830EP55 


	addi a1, a1, 1
	li t0, 0xdb75830e	 #-0x1.eb061c00p+55
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffdb75830e	 #-0x1.eb061c00p+55
	beq t0, a0, L563
	ret
L563:


	# Test 565
	# b32+ =0 i -1.000000P-126 +Zero -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L564
	ret
L564:


	# Test 566
	# b32+ =0 i -0.7FFFFFP-126 +Zero -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L565
	ret
L565:


	# Test 567
	# b32+ =0 i -0.1259F9P-126 +Zero -> -0.1259F9P-126 


	addi a1, a1, 1
	li t0, 0x801259f9	 #-0x1.259f9000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff801259f9	 #-0x1.259f9000p-129
	beq t0, a0, L566
	ret
L566:


	# Test 568
	# b32+ =0 i -0.000001P-126 +Zero -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L567
	ret
L567:


	# Test 569
	# b32+ =0 i -1.000000P0 +Zero -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L568
	ret
L568:


	# Test 570
	# b32+ =0 i -Zero +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L569
	ret
L569:


	# Test 571
	# b32+ =0 i +Zero +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L570
	ret
L570:


	# Test 572
	# b32+ =0 i +1.000000P0 +Zero -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L571
	ret
L571:


	# Test 573
	# b32+ =0 i +0.000001P-126 +Zero -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L572
	ret
L572:


	# Test 574
	# b32+ =0 i +0.1C7AB5P-126 +Zero -> +0.1C7AB5P-126 


	addi a1, a1, 1
	li t0, 0x1c7ab5	 #0x1.c7ab5000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1c7ab5	 #0x1.c7ab5000p-129
	beq t0, a0, L573
	ret
L573:


	# Test 575
	# b32+ =0 i +0.7FFFFFP-126 +Zero -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L574
	ret
L574:


	# Test 576
	# b32+ =0 i +1.000000P-126 +Zero -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L575
	ret
L575:


	# Test 577
	# b32+ =0 i +1.3DE9B9P-74 +Zero -> +1.3DE9B9P-74 


	addi a1, a1, 1
	li t0, 0x1abde9b9	 #0x1.7bd37200p-74
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1abde9b9	 #0x1.7bd37200p-74
	beq t0, a0, L576
	ret
L576:


	# Test 578
	# b32+ =0 i +1.7FFFFFP127 +Zero -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L577
	ret
L577:


	# Test 579
	# b32+ =0 i +Inf +Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L578
	ret
L578:


	# Test 580
	# b32+ =0 i -Inf +1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L579
	ret
L579:


	# Test 581
	# b32+ =0 i -1.7FFFFFP127 +1.000000P0 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L580
	ret
L580:


	# Test 582
	# b32+ =0 i -1.4427CAP38 +1.000000P0 -> -1.4427CAP38 x


	addi a1, a1, 1
	li t0, 0xd2c427ca	 #-0x1.884f9400p+38
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd2c427ca	 #-0x1.884f9400p+38
	beq t0, a0, L581
	ret
L581:


	# Test 583
	# b32+ =0 i -1.000000P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L582
	ret
L582:


	# Test 584
	# b32+ =0 i -0.7FFFFFP-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L583
	ret
L583:


	# Test 585
	# b32+ =0 i -0.3616CDP-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x803616cd	 #-0x1.b0b66800p-128
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L584
	ret
L584:


	# Test 586
	# b32+ =0 i -0.000001P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L585
	ret
L585:


	# Test 587
	# b32+ =0 i -1.000000P0 +1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L586
	ret
L586:


	# Test 588
	# b32+ =0 i -Zero +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L587
	ret
L587:


	# Test 589
	# b32+ =0 i +Zero +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L588
	ret
L588:


	# Test 590
	# b32+ =0 i +1.000000P0 +1.000000P0 -> +1.000000P1 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x40000000	 #0x1.00000000p+1
	beq t0, a0, L589
	ret
L589:


	# Test 591
	# b32+ =0 i +0.000001P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L590
	ret
L590:


	# Test 592
	# b32+ =0 i +0.1A9F71P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1a9f71	 #0x1.a9f71000p-129
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L591
	ret
L591:


	# Test 593
	# b32+ =0 i +0.7FFFFFP-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L592
	ret
L592:


	# Test 594
	# b32+ =0 i +1.000000P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L593
	ret
L593:


	# Test 595
	# b32+ =0 i +1.0C8E74P37 +1.000000P0 -> +1.0C8E74P37 x


	addi a1, a1, 1
	li t0, 0x520c8e74	 #0x1.191ce800p+37
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x520c8e74	 #0x1.191ce800p+37
	beq t0, a0, L594
	ret
L594:


	# Test 596
	# b32+ =0 i +1.7FFFFFP127 +1.000000P0 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L595
	ret
L595:


	# Test 597
	# b32+ =0 i +Inf +1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L596
	ret
L596:


	# Test 598
	# b32+ =0 i -Inf +0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L597
	ret
L597:


	# Test 599
	# b32+ =0 i -1.7FFFFFP127 +0.000001P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L598
	ret
L598:


	# Test 600
	# b32+ =0 i -1.134C85P-75 +0.000001P-126 -> -1.134C85P-75 x


	addi a1, a1, 1
	li t0, 0x9a134c85	 #-0x1.26990a00p-75
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff9a134c85	 #-0x1.26990a00p-75
	beq t0, a0, L599
	ret
L599:


	# Test 601
	# b32+ =0 i -1.000000P-126 +0.000001P-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L600
	ret
L600:


	# Test 602
	# b32+ =0 i -0.7FFFFFP-126 +0.000001P-126 -> -0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ffffe	 #-0x1.fffff800p-127
	beq t0, a0, L601
	ret
L601:


	# Test 603
	# b32+ =0 i -0.14BB88P-126 +0.000001P-126 -> -0.14BB87P-126 


	addi a1, a1, 1
	li t0, 0x8014bb88	 #-0x1.4bb88000p-129
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8014bb87	 #-0x1.4bb87000p-129
	beq t0, a0, L602
	ret
L602:


	# Test 604
	# b32+ =0 i -0.000001P-126 +0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L603
	ret
L603:


	# Test 605
	# b32+ =0 i -1.000000P0 +0.000001P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L604
	ret
L604:


	# Test 606
	# b32+ =0 i -Zero +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L605
	ret
L605:


	# Test 607
	# b32+ =0 i +Zero +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L606
	ret
L606:


	# Test 608
	# b32+ =0 i +1.000000P0 +0.000001P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L607
	ret
L607:


	# Test 609
	# b32+ =0 i +0.000001P-126 +0.000001P-126 -> +0.000002P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2	 #0x1.00000000p-148
	beq t0, a0, L608
	ret
L608:


	# Test 610
	# b32+ =0 i +0.09C42CP-126 +0.000001P-126 -> +0.09C42DP-126 


	addi a1, a1, 1
	li t0, 0x9c42c	 #0x1.38858000p-130
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x9c42d	 #0x1.3885a000p-130
	beq t0, a0, L609
	ret
L609:


	# Test 611
	# b32+ =0 i +0.7FFFFFP-126 +0.000001P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L610
	ret
L610:


	# Test 612
	# b32+ =0 i +1.000000P-126 +0.000001P-126 -> +1.000001P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800001	 #0x1.00000200p-126
	beq t0, a0, L611
	ret
L611:


	# Test 613
	# b32+ =0 i +1.5B332FP19 +0.000001P-126 -> +1.5B332FP19 x


	addi a1, a1, 1
	li t0, 0x495b332f	 #0x1.b6665e00p+19
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x495b332f	 #0x1.b6665e00p+19
	beq t0, a0, L612
	ret
L612:


	# Test 614
	# b32+ =0 i +1.7FFFFFP127 +0.000001P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L613
	ret
L613:


	# Test 615
	# b32+ =0 i +Inf +0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L614
	ret
L614:


	# Test 616
	# b32+ =0 i -Inf +0.7826F2P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7826f2	 #0x1.e09bc800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L615
	ret
L615:


	# Test 617
	# b32+ =0 i -1.7FFFFFP127 +0.23065AP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x23065a	 #0x1.1832d000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L616
	ret
L616:


	# Test 618
	# b32+ =0 i -1.61B140P-61 +0.127AA5P-126 -> -1.61B140P-61 x


	addi a1, a1, 1
	li t0, 0xa161b140	 #-0x1.c3628000p-61
	fmv.s.x f1, t0
	li t0, 0x127aa5	 #0x1.27aa5000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa161b140	 #-0x1.c3628000p-61
	beq t0, a0, L617
	ret
L617:


	# Test 619
	# b32+ =0 i -1.000000P-126 +0.2D74C3P-126 -> -0.528B3DP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x2d74c3	 #0x1.6ba61800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80528b3d	 #-0x1.4a2cf400p-127
	beq t0, a0, L618
	ret
L618:


	# Test 620
	# b32+ =0 i -0.7FFFFFP-126 +0.58D42BP-126 -> -0.272BD4P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x58d42b	 #0x1.6350ac00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80272bd4	 #-0x1.395ea000p-128
	beq t0, a0, L619
	ret
L619:


	# Test 621
	# b32+ =0 i -0.136044P-126 +0.1A7776P-126 -> +0.071732P-126 


	addi a1, a1, 1
	li t0, 0x80136044	 #-0x1.36044000p-129
	fmv.s.x f1, t0
	li t0, 0x1a7776	 #0x1.a7776000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x71732	 #0x1.c5cc8000p-131
	beq t0, a0, L620
	ret
L620:


	# Test 622
	# b32+ =0 i -0.000001P-126 +0.6E92FAP-126 -> +0.6E92F9P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x6e92fa	 #0x1.ba4be800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6e92f9	 #0x1.ba4be400p-127
	beq t0, a0, L621
	ret
L621:


	# Test 623
	# b32+ =0 i -1.000000P0 +0.2DA1FCP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x2da1fc	 #0x1.6d0fe000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L622
	ret
L622:


	# Test 624
	# b32+ =0 i -Zero +0.790164P-126 -> +0.790164P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x790164	 #0x1.e4059000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x790164	 #0x1.e4059000p-127
	beq t0, a0, L623
	ret
L623:


	# Test 625
	# b32+ =0 i +Zero +0.2460CBP-126 -> +0.2460CBP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x2460cb	 #0x1.23065800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2460cb	 #0x1.23065800p-128
	beq t0, a0, L624
	ret
L624:


	# Test 626
	# b32+ =0 i +1.000000P0 +0.06C387P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x6c387	 #0x1.b0e1c000p-131
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L625
	ret
L625:


	# Test 627
	# b32+ =0 i +0.000001P-126 +0.45E2EEP-126 -> +0.45E2EFP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x45e2ee	 #0x1.178bb800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x45e2ef	 #0x1.178bbc00p-127
	beq t0, a0, L626
	ret
L626:


	# Test 628
	# b32+ =0 i +0.0468E7P-126 +0.7796ECP-126 -> +0.7BFFD3P-126 


	addi a1, a1, 1
	li t0, 0x468e7	 #0x1.1a39c000p-131
	fmv.s.x f1, t0
	li t0, 0x7796ec	 #0x1.de5bb000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7bffd3	 #0x1.efff4c00p-127
	beq t0, a0, L627
	ret
L627:


	# Test 629
	# b32+ =0 i +0.7FFFFFP-126 +0.190AAFP-126 -> +1.190AAEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x190aaf	 #0x1.90aaf000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x990aae	 #0x1.32155c00p-126
	beq t0, a0, L628
	ret
L628:


	# Test 630
	# b32+ =0 i +1.000000P-126 +0.6FAD6BP-126 -> +1.6FAD6BP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x6fad6b	 #0x1.beb5ac00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xefad6b	 #0x1.df5ad600p-126
	beq t0, a0, L629
	ret
L629:


	# Test 631
	# b32+ =0 i +1.2A57EBP2 +0.4F53BDP-126 -> +1.2A57EBP2 x


	addi a1, a1, 1
	li t0, 0x40aa57eb	 #0x1.54afd600p+2
	fmv.s.x f1, t0
	li t0, 0x4f53bd	 #0x1.3d4ef400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x40aa57eb	 #0x1.54afd600p+2
	beq t0, a0, L630
	ret
L630:


	# Test 632
	# b32+ =0 i +1.7FFFFFP127 +0.466C3AP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x466c3a	 #0x1.19b0e800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L631
	ret
L631:


	# Test 633
	# b32+ =0 i +Inf +0.253B3CP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x253b3c	 #0x1.29d9e000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L632
	ret
L632:


	# Test 634
	# b32+ =0 i -Inf +0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L633
	ret
L633:


	# Test 635
	# b32+ =0 i -1.7FFFFFP127 +0.7FFFFFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L634
	ret
L634:


	# Test 636
	# b32+ =0 i -1.440DC8P-125 +0.7FFFFFP-126 -> -1.040DC8P-125 x


	addi a1, a1, 1
	li t0, 0x81440dc8	 #-0x1.881b9000p-125
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff81040dc8	 #-0x1.081b9000p-125
	beq t0, a0, L635
	ret
L635:


	# Test 637
	# b32+ =0 i -1.000000P-126 +0.7FFFFFP-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L636
	ret
L636:


	# Test 638
	# b32+ =0 i -0.7FFFFFP-126 +0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L637
	ret
L637:


	# Test 639
	# b32+ =0 i -0.0A44FFP-126 +0.7FFFFFP-126 -> +0.75BB00P-126 


	addi a1, a1, 1
	li t0, 0x800a44ff	 #-0x1.489fe000p-130
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x75bb00	 #0x1.d6ec0000p-127
	beq t0, a0, L638
	ret
L638:


	# Test 640
	# b32+ =0 i -0.000001P-126 +0.7FFFFFP-126 -> +0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7ffffe	 #0x1.fffff800p-127
	beq t0, a0, L639
	ret
L639:


	# Test 641
	# b32+ =0 i -1.000000P0 +0.7FFFFFP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L640
	ret
L640:


	# Test 642
	# b32+ =0 i -Zero +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L641
	ret
L641:


	# Test 643
	# b32+ =0 i +Zero +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L642
	ret
L642:


	# Test 644
	# b32+ =0 i +1.000000P0 +0.7FFFFFP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L643
	ret
L643:


	# Test 645
	# b32+ =0 i +0.000001P-126 +0.7FFFFFP-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L644
	ret
L644:


	# Test 646
	# b32+ =0 i +0.0C65BBP-126 +0.7FFFFFP-126 -> +1.0C65BAP-126 


	addi a1, a1, 1
	li t0, 0xc65bb	 #0x1.8cb76000p-130
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x8c65ba	 #0x1.18cb7400p-126
	beq t0, a0, L645
	ret
L645:


	# Test 647
	# b32+ =0 i +0.7FFFFFP-126 +0.7FFFFFP-126 -> +1.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffe	 #0x1.fffffc00p-126
	beq t0, a0, L646
	ret
L646:


	# Test 648
	# b32+ =0 i +1.000000P-126 +0.7FFFFFP-126 -> +1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffff	 #0x1.fffffe00p-126
	beq t0, a0, L647
	ret
L647:


	# Test 649
	# b32+ =0 i +1.78FCA6P-80 +0.7FFFFFP-126 -> +1.78FCA6P-80 x


	addi a1, a1, 1
	li t0, 0x17f8fca6	 #0x1.f1f94c00p-80
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x17f8fca6	 #0x1.f1f94c00p-80
	beq t0, a0, L648
	ret
L648:


	# Test 650
	# b32+ =0 i +1.7FFFFFP127 +0.7FFFFFP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L649
	ret
L649:


	# Test 651
	# b32+ =0 i +Inf +0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L650
	ret
L650:


	# Test 652
	# b32+ =0 i -Inf +1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L651
	ret
L651:


	# Test 653
	# b32+ =0 i -1.7FFFFFP127 +1.000000P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L652
	ret
L652:


	# Test 654
	# b32+ =0 i -1.284A9CP5 +1.000000P-126 -> -1.284A9CP5 x


	addi a1, a1, 1
	li t0, 0xc2284a9c	 #-0x1.50953800p+5
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc2284a9c	 #-0x1.50953800p+5
	beq t0, a0, L653
	ret
L653:


	# Test 655
	# b32+ =0 i -1.000000P-126 +1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L654
	ret
L654:


	# Test 656
	# b32+ =0 i -0.7FFFFFP-126 +1.000000P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L655
	ret
L655:


	# Test 657
	# b32+ =0 i -0.01E9BAP-126 +1.000000P-126 -> +0.7E1646P-126 


	addi a1, a1, 1
	li t0, 0x8001e9ba	 #-0x1.e9ba0000p-133
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e1646	 #0x1.f8591800p-127
	beq t0, a0, L656
	ret
L656:


	# Test 658
	# b32+ =0 i -0.000001P-126 +1.000000P-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L657
	ret
L657:


	# Test 659
	# b32+ =0 i -1.000000P0 +1.000000P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L658
	ret
L658:


	# Test 660
	# b32+ =0 i -Zero +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L659
	ret
L659:


	# Test 661
	# b32+ =0 i +Zero +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L660
	ret
L660:


	# Test 662
	# b32+ =0 i +1.000000P0 +1.000000P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L661
	ret
L661:


	# Test 663
	# b32+ =0 i +0.000001P-126 +1.000000P-126 -> +1.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800001	 #0x1.00000200p-126
	beq t0, a0, L662
	ret
L662:


	# Test 664
	# b32+ =0 i +0.3F4243P-126 +1.000000P-126 -> +1.3F4243P-126 


	addi a1, a1, 1
	li t0, 0x3f4243	 #0x1.fa121800p-128
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xbf4243	 #0x1.7e848600p-126
	beq t0, a0, L663
	ret
L663:


	# Test 665
	# b32+ =0 i +0.7FFFFFP-126 +1.000000P-126 -> +1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffff	 #0x1.fffffe00p-126
	beq t0, a0, L664
	ret
L664:


	# Test 666
	# b32+ =0 i +1.000000P-126 +1.000000P-126 -> +1.000000P-125 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1000000	 #0x1.00000000p-125
	beq t0, a0, L665
	ret
L665:


	# Test 667
	# b32+ =0 i +1.5CF97AP30 +1.000000P-126 -> +1.5CF97AP30 x


	addi a1, a1, 1
	li t0, 0x4edcf97a	 #0x1.b9f2f400p+30
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4edcf97a	 #0x1.b9f2f400p+30
	beq t0, a0, L666
	ret
L666:


	# Test 668
	# b32+ =0 i +1.7FFFFFP127 +1.000000P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L667
	ret
L667:


	# Test 669
	# b32+ =0 i +Inf +1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L668
	ret
L668:


	# Test 670
	# b32+ =0 i -Inf +1.59A68AP9 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x4459a68a	 #0x1.b34d1400p+9
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L669
	ret
L669:


	# Test 671
	# b32+ =0 i -1.7FFFFFP127 +1.504946P48 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x57d04946	 #0x1.a0928c00p+48
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L670
	ret
L670:


	# Test 672
	# b32+ =0 i -1.776F57P51 +1.5BEA6BP37 -> -1.776BE7P51 x


	addi a1, a1, 1
	li t0, 0xd9776f57	 #-0x1.eedeae00p+51
	fmv.s.x f1, t0
	li t0, 0x525bea6b	 #0x1.b7d4d600p+37
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd9776be7	 #-0x1.eed7ce00p+51
	beq t0, a0, L671
	ret
L671:


	# Test 673
	# b32+ =0 i -1.000000P-126 +1.660815P90 -> +1.660815P90 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x6ce60815	 #0x1.cc102a00p+90
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6ce60815	 #0x1.cc102a00p+90
	beq t0, a0, L672
	ret
L672:


	# Test 674
	# b32+ =0 i -0.7FFFFFP-126 +1.255717P5 -> +1.255717P5 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x42255717	 #0x1.4aae2e00p+5
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x42255717	 #0x1.4aae2e00p+5
	beq t0, a0, L673
	ret
L673:


	# Test 675
	# b32+ =0 i -0.04A68EP-126 +1.644082P-2 -> +1.644082P-2 x


	addi a1, a1, 1
	li t0, 0x8004a68e	 #-0x1.29a38000p-131
	fmv.s.x f1, t0
	li t0, 0x3ee44082	 #0x1.c8810400p-2
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3ee44082	 #0x1.c8810400p-2
	beq t0, a0, L674
	ret
L674:


	# Test 676
	# b32+ =0 i -0.000001P-126 +1.3BD5E6P-49 -> +1.3BD5E6P-49 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x273bd5e6	 #0x1.77abcc00p-49
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x273bd5e6	 #0x1.77abcc00p-49
	beq t0, a0, L675
	ret
L675:


	# Test 677
	# b32+ =0 i -1.000000P0 +1.5BB1F9P82 -> +1.5BB1F9P82 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x68dbb1f9	 #0x1.b763f200p+82
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x68dbb1f9	 #0x1.b763f200p+82
	beq t0, a0, L676
	ret
L676:


	# Test 678
	# b32+ =0 i -Zero +1.1A80FBP-123 -> +1.1A80FBP-123 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x21a80fb	 #0x1.3501f600p-123
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x21a80fb	 #0x1.3501f600p-123
	beq t0, a0, L677
	ret
L677:


	# Test 679
	# b32+ =0 i +Zero +1.25A063P-62 -> +1.25A063P-62 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x20a5a063	 #0x1.4b40c600p-62
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x20a5a063	 #0x1.4b40c600p-62
	beq t0, a0, L678
	ret
L678:


	# Test 680
	# b32+ =0 i +1.000000P0 +1.30FFCAP-57 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x2330ffca	 #0x1.61ff9400p-57
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L679
	ret
L679:


	# Test 681
	# b32+ =0 i +0.000001P-126 +1.276286P78 -> +1.276286P78 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x66a76286	 #0x1.4ec50c00p+78
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x66a76286	 #0x1.4ec50c00p+78
	beq t0, a0, L680
	ret
L680:


	# Test 682
	# b32+ =0 i +0.2DE6FEP-126 +1.7146B1P-119 -> +1.71A27FP-119 x


	addi a1, a1, 1
	li t0, 0x2de6fe	 #0x1.6f37f000p-128
	fmv.s.x f1, t0
	li t0, 0x47146b1	 #0x1.e28d6200p-119
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x471a27f	 #0x1.e344fe00p-119
	beq t0, a0, L681
	ret
L681:


	# Test 683
	# b32+ =0 i +0.7FFFFFP-126 +1.064D9BP28 -> +1.064D9BP28 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x4d864d9b	 #0x1.0c9b3600p+28
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4d864d9b	 #0x1.0c9b3600p+28
	beq t0, a0, L682
	ret
L682:


	# Test 684
	# b32+ =0 i +1.000000P-126 +1.116D03P113 -> +1.116D03P113 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x78116d03	 #0x1.22da0600p+113
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x78116d03	 #0x1.22da0600p+113
	beq t0, a0, L683
	ret
L683:


	# Test 685
	# b32+ =0 i +1.2B1E35P-115 +1.799CC8P-70 -> +1.799CC8P-70 x


	addi a1, a1, 1
	li t0, 0x62b1e35	 #0x1.563c6a00p-115
	fmv.s.x f1, t0
	li t0, 0x1cf99cc8	 #0x1.f3399000p-70
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1cf99cc8	 #0x1.f3399000p-70
	beq t0, a0, L684
	ret
L684:


	# Test 686
	# b32+ =0 i +1.7FFFFFP127 +1.12EF26P13 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x4612ef26	 #0x1.25de4c00p+13
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L685
	ret
L685:


	# Test 687
	# b32+ =0 i +Inf +1.1E4E8DP82 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x689e4e8d	 #0x1.3c9d1a00p+82
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L686
	ret
L686:


	# Test 688
	# b32+ =0 i -Inf +1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L687
	ret
L687:


	# Test 689
	# b32+ =0 i -1.7FFFFFP127 +1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L688
	ret
L688:


	# Test 690
	# b32+ =0 i -1.459413P34 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xd0c59413	 #-0x1.8b282600p+34
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L689
	ret
L689:


	# Test 691
	# b32+ =0 i -1.000000P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L690
	ret
L690:


	# Test 692
	# b32+ =0 i -0.7FFFFFP-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L691
	ret
L691:


	# Test 693
	# b32+ =0 i -0.43CB49P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x8043cb49	 #-0x1.0f2d2400p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L692
	ret
L692:


	# Test 694
	# b32+ =0 i -0.000001P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L693
	ret
L693:


	# Test 695
	# b32+ =0 i -1.000000P0 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L694
	ret
L694:


	# Test 696
	# b32+ =0 i -Zero +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L695
	ret
L695:


	# Test 697
	# b32+ =0 i +Zero +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L696
	ret
L696:


	# Test 698
	# b32+ =0 i +1.000000P0 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L697
	ret
L697:


	# Test 699
	# b32+ =0 i +0.000001P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L698
	ret
L698:


	# Test 700
	# b32+ =0 i +0.1C0BBAP-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1c0bba	 #0x1.c0bba000p-129
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L699
	ret
L699:


	# Test 701
	# b32+ =0 i +0.7FFFFFP-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L700
	ret
L700:


	# Test 702
	# b32+ =0 i +1.000000P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L701
	ret
L701:


	# Test 703
	# b32+ =0 i +1.7A42F0P-117 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x57a42f0	 #0x1.f485e000p-117
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L702
	ret
L702:


	# Test 704
	# b32+ =0 i +1.7FFFFFP127 +1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L703
	ret
L703:


	# Test 705
	# b32+ =0 i +Inf +1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L704
	ret
L704:


	# Test 706
	# b32+ =0 i -1.7FFFFFP127 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L705
	ret
L705:


	# Test 707
	# b32+ =0 i -1.14F8CEP97 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xf014f8ce	 #-0x1.29f19c00p+97
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L706
	ret
L706:


	# Test 708
	# b32+ =0 i -1.000000P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L707
	ret
L707:


	# Test 709
	# b32+ =0 i -0.7FFFFFP-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L708
	ret
L708:


	# Test 710
	# b32+ =0 i -0.527005P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80527005	 #-0x1.49c01400p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L709
	ret
L709:


	# Test 711
	# b32+ =0 i -0.000001P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L710
	ret
L710:


	# Test 712
	# b32+ =0 i -1.000000P0 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L711
	ret
L711:


	# Test 713
	# b32+ =0 i -Zero +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L712
	ret
L712:


	# Test 714
	# b32+ =0 i +Zero +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L713
	ret
L713:


	# Test 715
	# b32+ =0 i +1.000000P0 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L714
	ret
L714:


	# Test 716
	# b32+ =0 i +0.000001P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L715
	ret
L715:


	# Test 717
	# b32+ =0 i +0.20488DP-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x20488d	 #0x1.02446800p-128
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L716
	ret
L716:


	# Test 718
	# b32+ =0 i +0.7FFFFFP-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L717
	ret
L717:


	# Test 719
	# b32+ =0 i +1.000000P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L718
	ret
L718:


	# Test 720
	# b32+ =0 i +1.48E7ACP-122 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x2c8e7ac	 #0x1.91cf5800p-122
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L719
	ret
L719:


	# Test 721
	# b32+ =0 i +1.7FFFFFP127 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L720
	ret
L720:


	# Test 722
	# b32+ =0 i +Inf +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L721
	ret
L721:


	# Test 723
	# b32+ =0 -Inf -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L722
	ret
L722:


	# Test 724
	# b32+ =0 -1.7FFFFFP127 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L723
	ret
L723:


	# Test 725
	# b32+ =0 -1.3BEF24P42 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xd4bbef24	 #-0x1.77de4800p+42
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L724
	ret
L724:


	# Test 726
	# b32+ =0 -1.000000P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L725
	ret
L725:


	# Test 727
	# b32+ =0 -0.7FFFFFP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L726
	ret
L726:


	# Test 728
	# b32+ =0 -0.140E43P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80140e43	 #-0x1.40e43000p-129
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L727
	ret
L727:


	# Test 729
	# b32+ =0 -0.000001P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L728
	ret
L728:


	# Test 730
	# b32+ =0 -1.000000P0 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L729
	ret
L729:


	# Test 731
	# b32+ =0 -Zero -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L730
	ret
L730:


	# Test 732
	# b32+ =0 +Zero -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L731
	ret
L731:


	# Test 733
	# b32+ =0 +1.000000P0 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L732
	ret
L732:


	# Test 734
	# b32+ =0 +0.000001P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L733
	ret
L733:


	# Test 735
	# b32+ =0 +0.3D4EB3P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3d4eb3	 #0x1.ea759800p-128
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L734
	ret
L734:


	# Test 736
	# b32+ =0 +0.7FFFFFP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L735
	ret
L735:


	# Test 737
	# b32+ =0 +1.000000P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L736
	ret
L736:


	# Test 738
	# b32+ =0 +1.5A85EAP-60 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x21da85ea	 #0x1.b50bd400p-60
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L737
	ret
L737:


	# Test 739
	# b32+ =0 +1.7FFFFFP127 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L738
	ret
L738:


	# Test 740
	# b32+ =0 +Inf -Inf -> q i


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L739
	ret
L739:


	# Test 741
	# b32+ =0 q -Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L740
	ret
L740:


	# Test 742
	# b32+ =0 q -Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L741
	ret
L741:


	# Test 743
	# b32+ =0 -Inf -1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L742
	ret
L742:


	# Test 744
	# b32+ =0 -1.7FFFFFP127 -1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L743
	ret
L743:


	# Test 745
	# b32+ =0 -1.0A53E0P89 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xec0a53e0	 #-0x1.14a7c000p+89
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L744
	ret
L744:


	# Test 746
	# b32+ =0 -1.000000P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L745
	ret
L745:


	# Test 747
	# b32+ =0 -0.7FFFFFP-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L746
	ret
L746:


	# Test 748
	# b32+ =0 -0.0AB2FEP-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800ab2fe	 #-0x1.565fc000p-130
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L747
	ret
L747:


	# Test 749
	# b32+ =0 -0.000001P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L748
	ret
L748:


	# Test 750
	# b32+ =0 -1.000000P0 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L749
	ret
L749:


	# Test 751
	# b32+ =0 -Zero -1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L750
	ret
L750:


	# Test 752
	# b32+ =0 +Zero -1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L751
	ret
L751:


	# Test 753
	# b32+ =0 +1.000000P0 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L752
	ret
L752:


	# Test 754
	# b32+ =0 +0.000001P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L753
	ret
L753:


	# Test 755
	# b32+ =0 +0.2BF36EP-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x2bf36e	 #0x1.5f9b7000p-128
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L754
	ret
L754:


	# Test 756
	# b32+ =0 +0.7FFFFFP-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L755
	ret
L755:


	# Test 757
	# b32+ =0 +1.000000P-126 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L756
	ret
L756:


	# Test 758
	# b32+ =0 +1.292AA5P83 -1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x69292aa5	 #0x1.52554a00p+83
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L757
	ret
L757:


	# Test 759
	# b32+ =0 +1.7FFFFFP127 -1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L758
	ret
L758:


	# Test 760
	# b32+ =0 +Inf -1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L759
	ret
L759:


	# Test 761
	# b32+ =0 q -1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L760
	ret
L760:


	# Test 762
	# b32+ =0 q -1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L761
	ret
L761:


	# Test 763
	# b32+ =0 -Inf -1.2AACE2P-22 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xb4aaace2	 #-0x1.5559c400p-22
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L762
	ret
L762:


	# Test 764
	# b32+ =0 -1.7FFFFFP127 -1.358C49P-17 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xb7358c49	 #-0x1.6b189200p-17
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L763
	ret
L763:


	# Test 765
	# b32+ =0 -1.43E082P-88 -1.15AD25P-107 -> -1.43E095P-88 x


	addi a1, a1, 1
	li t0, 0x93c3e082	 #-0x1.87c10400p-88
	fmv.s.x f1, t0
	li t0, 0x8a15ad25	 #-0x1.2b5a4a00p-107
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff93c3e095	 #-0x1.87c12a00p-88
	beq t0, a0, L764
	ret
L764:


	# Test 766
	# b32+ =0 -1.000000P-126 -1.374E6CP-37 -> -1.374E6CP-37 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xad374e6c	 #-0x1.6e9cd800p-37
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffad374e6c	 #-0x1.6e9cd800p-37
	beq t0, a0, L765
	ret
L765:


	# Test 767
	# b32+ =0 -0.7FFFFFP-126 -1.0B5A1AP4 -> -1.0B5A1AP4 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xc18b5a1a	 #-0x1.16b43400p+4
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc18b5a1a	 #-0x1.16b43400p+4
	beq t0, a0, L766
	ret
L766:


	# Test 768
	# b32+ =0 -0.0597B9P-126 -1.1D833CP-66 -> -1.1D833CP-66 x


	addi a1, a1, 1
	li t0, 0x800597b9	 #-0x1.65ee4000p-131
	fmv.s.x f1, t0
	li t0, 0x9e9d833c	 #-0x1.3b067800p-66
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff9e9d833c	 #-0x1.3b067800p-66
	beq t0, a0, L767
	ret
L767:


	# Test 769
	# b32+ =0 -0.000001P-126 -1.0C9C3EP48 -> -1.0C9C3EP48 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xd78c9c3e	 #-0x1.19387c00p+48
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd78c9c3e	 #-0x1.19387c00p+48
	beq t0, a0, L768
	ret
L768:


	# Test 770
	# b32+ =0 -1.000000P0 -1.17BBA5P101 -> -1.17BBA5P101 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xf217bba5	 #-0x1.2f774a00p+101
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff217bba5	 #-0x1.2f774a00p+101
	beq t0, a0, L769
	ret
L769:


	# Test 771
	# b32+ =0 -Zero -1.22DB0CP58 -> -1.22DB0CP58 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xdca2db0c	 #-0x1.45b61800p+58
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffdca2db0c	 #-0x1.45b61800p+58
	beq t0, a0, L770
	ret
L770:


	# Test 772
	# b32+ =0 +Zero -1.76A6BAP34 -> -1.76A6BAP34 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xd0f6a6ba	 #-0x1.ed4d7400p+34
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd0f6a6ba	 #-0x1.ed4d7400p+34
	beq t0, a0, L771
	ret
L771:


	# Test 773
	# b32+ =0 +1.000000P0 -1.6D0976P9 -> -1.6CC976P9 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xc46d0976	 #-0x1.da12ec00p+9
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc46cc976	 #-0x1.d992ec00p+9
	beq t0, a0, L772
	ret
L772:


	# Test 774
	# b32+ =0 +0.000001P-126 -1.7828DEP110 -> -1.7828DEP110 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xf6f828de	 #-0x1.f051bc00p+110
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff6f828de	 #-0x1.f051bc00p+110
	beq t0, a0, L773
	ret
L773:


	# Test 775
	# b32+ =0 +0.2FF042P-126 -1.7F322AP-77 -> -1.7F322AP-77 x


	addi a1, a1, 1
	li t0, 0x2ff042	 #0x1.7f821000p-128
	fmv.s.x f1, t0
	li t0, 0x997f322a	 #-0x1.fe645400p-77
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff997f322a	 #-0x1.fe645400p-77
	beq t0, a0, L774
	ret
L774:


	# Test 776
	# b32+ =0 +0.7FFFFFP-126 -1.79EB01P-6 -> -1.79EB01P-6 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbcf9eb01	 #-0x1.f3d60200p-6
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbcf9eb01	 #-0x1.f3d60200p-6
	beq t0, a0, L775
	ret
L775:


	# Test 777
	# b32+ =0 +1.000000P-126 -1.62335BP-119 -> -1.61335BP-119 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x8462335b	 #-0x1.c466b600p-119
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8461335b	 #-0x1.c266b600p-119
	beq t0, a0, L776
	ret
L776:


	# Test 778
	# b32+ =0 +1.784F60P65 -1.331F83P106 -> -1.331F83P106 x


	addi a1, a1, 1
	li t0, 0x60784f60	 #0x1.f09ec000p+65
	fmv.s.x f1, t0
	li t0, 0xf4b31f83	 #-0x1.663f0600p+106
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff4b31f83	 #-0x1.663f0600p+106
	beq t0, a0, L777
	ret
L777:


	# Test 779
	# b32+ =0 +1.7FFFFFP127 -1.78B229P-125 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x8178b229	 #-0x1.f1645200p-125
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L778
	ret
L778:


	# Test 780
	# b32+ =0 +Inf -1.6E94E5P-14 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xb8ee94e5	 #-0x1.dd29ca00p-14
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L779
	ret
L779:


	# Test 781
	# b32+ =0 q -1.4B5C53P20 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xc9cb5c53	 #-0x1.96b8a600p+20
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L780
	ret
L780:


	# Test 782
	# b32+ =0 q -1.7CD1A0P-11 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xba7cd1a0	 #-0x1.f9a34000p-11
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L781
	ret
L781:


	# Test 783
	# b32+ =0 -Inf -1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L782
	ret
L782:


	# Test 784
	# b32+ =0 -1.7FFFFFP127 -1.000000P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L783
	ret
L783:


	# Test 785
	# b32+ =0 -1.12853EP-73 -1.000000P-126 -> -1.12853EP-73 x


	addi a1, a1, 1
	li t0, 0x9b12853e	 #-0x1.250a7c00p-73
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff9b12853e	 #-0x1.250a7c00p-73
	beq t0, a0, L784
	ret
L784:


	# Test 786
	# b32+ =0 -1.000000P-126 -1.000000P-126 -> -1.000000P-125 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff81000000	 #-0x1.00000000p-125
	beq t0, a0, L785
	ret
L785:


	# Test 787
	# b32+ =0 -0.7FFFFFP-126 -1.000000P-126 -> -1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80ffffff	 #-0x1.fffffe00p-126
	beq t0, a0, L786
	ret
L786:


	# Test 788
	# b32+ =0 -0.503C74P-126 -1.000000P-126 -> -1.503C74P-126 


	addi a1, a1, 1
	li t0, 0x80503c74	 #-0x1.40f1d000p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80d03c74	 #-0x1.a078e800p-126
	beq t0, a0, L787
	ret
L787:


	# Test 789
	# b32+ =0 -0.000001P-126 -1.000000P-126 -> -1.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800001	 #-0x1.00000200p-126
	beq t0, a0, L788
	ret
L788:


	# Test 790
	# b32+ =0 -1.000000P0 -1.000000P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L789
	ret
L789:


	# Test 791
	# b32+ =0 -Zero -1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L790
	ret
L790:


	# Test 792
	# b32+ =0 +Zero -1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L791
	ret
L791:


	# Test 793
	# b32+ =0 +1.000000P0 -1.000000P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L792
	ret
L792:


	# Test 794
	# b32+ =0 +0.000001P-126 -1.000000P-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L793
	ret
L793:


	# Test 795
	# b32+ =0 +0.3E14FDP-126 -1.000000P-126 -> -0.41EB03P-126 


	addi a1, a1, 1
	li t0, 0x3e14fd	 #0x1.f0a7e800p-128
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8041eb03	 #-0x1.07ac0c00p-127
	beq t0, a0, L794
	ret
L794:


	# Test 796
	# b32+ =0 +0.7FFFFFP-126 -1.000000P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L795
	ret
L795:


	# Test 797
	# b32+ =0 +1.000000P-126 -1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L796
	ret
L796:


	# Test 798
	# b32+ =0 +1.5C4C34P79 -1.000000P-126 -> +1.5C4C34P79 x


	addi a1, a1, 1
	li t0, 0x675c4c34	 #0x1.b8986800p+79
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x675c4c34	 #0x1.b8986800p+79
	beq t0, a0, L797
	ret
L797:


	# Test 799
	# b32+ =0 +1.7FFFFFP127 -1.000000P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L798
	ret
L798:


	# Test 800
	# b32+ =0 +Inf -1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L799
	ret
L799:


	# Test 801
	# b32+ =0 q -1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L800
	ret
L800:


	# Test 802
	# b32+ =0 q -1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L801
	ret
L801:


	# Test 803
	# b32+ =0 -Inf -0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L802
	ret
L802:


	# Test 804
	# b32+ =0 -1.7FFFFFP127 -0.7FFFFFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L803
	ret
L803:


	# Test 805
	# b32+ =0 -1.76C211P-28 -0.7FFFFFP-126 -> -1.76C211P-28 x


	addi a1, a1, 1
	li t0, 0xb1f6c211	 #-0x1.ed842200p-28
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb1f6c211	 #-0x1.ed842200p-28
	beq t0, a0, L804
	ret
L804:


	# Test 806
	# b32+ =0 -1.000000P-126 -0.7FFFFFP-126 -> -1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80ffffff	 #-0x1.fffffe00p-126
	beq t0, a0, L805
	ret
L805:


	# Test 807
	# b32+ =0 -0.7FFFFFP-126 -0.7FFFFFP-126 -> -1.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80fffffe	 #-0x1.fffffc00p-126
	beq t0, a0, L806
	ret
L806:


	# Test 808
	# b32+ =0 -0.017948P-126 -0.7FFFFFP-126 -> -1.017947P-126 


	addi a1, a1, 1
	li t0, 0x80017948	 #-0x1.79480000p-133
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80817947	 #-0x1.02f28e00p-126
	beq t0, a0, L807
	ret
L807:


	# Test 809
	# b32+ =0 -0.000001P-126 -0.7FFFFFP-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L808
	ret
L808:


	# Test 810
	# b32+ =0 -1.000000P0 -0.7FFFFFP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L809
	ret
L809:


	# Test 811
	# b32+ =0 -Zero -0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L810
	ret
L810:


	# Test 812
	# b32+ =0 +Zero -0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L811
	ret
L811:


	# Test 813
	# b32+ =0 +1.000000P0 -0.7FFFFFP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L812
	ret
L812:


	# Test 814
	# b32+ =0 +0.000001P-126 -0.7FFFFFP-126 -> -0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ffffe	 #-0x1.fffff800p-127
	beq t0, a0, L813
	ret
L813:


	# Test 815
	# b32+ =0 +0.2D39B8P-126 -0.7FFFFFP-126 -> -0.52C647P-126 


	addi a1, a1, 1
	li t0, 0x2d39b8	 #0x1.69cdc000p-128
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8052c647	 #-0x1.4b191c00p-127
	beq t0, a0, L814
	ret
L814:


	# Test 816
	# b32+ =0 +0.7FFFFFP-126 -0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L815
	ret
L815:


	# Test 817
	# b32+ =0 +1.000000P-126 -0.7FFFFFP-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L816
	ret
L816:


	# Test 818
	# b32+ =0 +1.2AF0EFP94 -0.7FFFFFP-126 -> +1.2AF0EFP94 x


	addi a1, a1, 1
	li t0, 0x6eaaf0ef	 #0x1.55e1de00p+94
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6eaaf0ef	 #0x1.55e1de00p+94
	beq t0, a0, L817
	ret
L817:


	# Test 819
	# b32+ =0 +1.7FFFFFP127 -0.7FFFFFP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L818
	ret
L818:


	# Test 820
	# b32+ =0 +Inf -0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L819
	ret
L819:


	# Test 821
	# b32+ =0 q -0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L820
	ret
L820:


	# Test 822
	# b32+ =0 q -0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L821
	ret
L821:


	# Test 823
	# b32+ =0 -Inf -0.6EC387P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x806ec387	 #-0x1.bb0e1c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L822
	ret
L822:


	# Test 824
	# b32+ =0 -1.7FFFFFP127 -0.06CF35P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x8006cf35	 #-0x1.b3cd4000p-131
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L823
	ret
L823:


	# Test 825
	# b32+ =0 -1.4566CDP-45 -0.1FB631P-126 -> -1.4566CDP-45 x


	addi a1, a1, 1
	li t0, 0xa94566cd	 #-0x1.8acd9a00p-45
	fmv.s.x f1, t0
	li t0, 0x801fb631	 #-0x1.fb631000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa94566cd	 #-0x1.8acd9a00p-45
	beq t0, a0, L824
	ret
L824:


	# Test 826
	# b32+ =0 -1.000000P-126 -0.141159P-126 -> -1.141159P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80141159	 #-0x1.41159000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80941159	 #-0x1.2822b200p-126
	beq t0, a0, L825
	ret
L825:


	# Test 827
	# b32+ =0 -0.7FFFFFP-126 -0.4F30C0P-126 -> -1.4F30BFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x804f30c0	 #-0x1.3cc30000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80cf30bf	 #-0x1.9e617e00p-126
	beq t0, a0, L826
	ret
L826:


	# Test 828
	# b32+ =0 -0.429E03P-126 -0.180C48P-126 -> -0.5AAA4BP-126 


	addi a1, a1, 1
	li t0, 0x80429e03	 #-0x1.0a780c00p-127
	fmv.s.x f1, t0
	li t0, 0x80180c48	 #-0x1.80c48000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff805aaa4b	 #-0x1.6aa92c00p-127
	beq t0, a0, L827
	ret
L827:


	# Test 829
	# b32+ =0 -0.000001P-126 -0.6E1BD5P-126 -> -0.6E1BD6P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x806e1bd5	 #-0x1.b86f5400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff806e1bd6	 #-0x1.b86f5800p-127
	beq t0, a0, L828
	ret
L828:


	# Test 830
	# b32+ =0 -1.000000P0 -0.14FE91P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8014fe91	 #-0x1.4fe91000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L829
	ret
L829:


	# Test 831
	# b32+ =0 -Zero -0.6F9DF9P-126 -> -0.6F9DF9P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x806f9df9	 #-0x1.be77e400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff806f9df9	 #-0x1.be77e400p-127
	beq t0, a0, L830
	ret
L830:


	# Test 832
	# b32+ =0 +Zero -0.4FFA0CP-126 -> -0.4FFA0CP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x804ffa0c	 #-0x1.3fe83000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff804ffa0c	 #-0x1.3fe83000p-127
	beq t0, a0, L831
	ret
L831:


	# Test 833
	# b32+ =0 +1.000000P0 -0.2E890EP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x802e890e	 #-0x1.74487000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L832
	ret
L832:


	# Test 834
	# b32+ =0 +0.000001P-126 -0.59A875P-126 -> -0.59A874P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x8059a875	 #-0x1.66a1d400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8059a874	 #-0x1.66a1d000p-127
	beq t0, a0, L833
	ret
L833:


	# Test 835
	# b32+ =0 +0.1BDE74P-126 -0.351277P-126 -> -0.193403P-126 


	addi a1, a1, 1
	li t0, 0x1bde74	 #0x1.bde74000p-129
	fmv.s.x f1, t0
	li t0, 0x80351277	 #-0x1.a893b800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80193403	 #-0x1.93403000p-129
	beq t0, a0, L834
	ret
L834:


	# Test 836
	# b32+ =0 +0.7FFFFFP-126 -0.702744P-126 -> +0.0FD8BBP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80702744	 #-0x1.c09d1000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfd8bb	 #0x1.fb176000p-130
	beq t0, a0, L835
	ret
L835:


	# Test 837
	# b32+ =0 +1.000000P-126 -0.664A00P-126 -> +0.19B600P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80664a00	 #-0x1.99280000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x19b600	 #0x1.9b600000p-129
	beq t0, a0, L836
	ret
L836:


	# Test 838
	# b32+ =0 +1.7995AAP76 -0.3DE88EP-126 -> +1.7995AAP76 x


	addi a1, a1, 1
	li t0, 0x65f995aa	 #0x1.f32b5400p+76
	fmv.s.x f1, t0
	li t0, 0x803de88e	 #-0x1.ef447000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x65f995aa	 #0x1.f32b5400p+76
	beq t0, a0, L837
	ret
L837:


	# Test 839
	# b32+ =0 +1.7FFFFFP127 -0.257516P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80257516	 #-0x1.2ba8b000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L838
	ret
L838:


	# Test 840
	# b32+ =0 +Inf -0.0B97D2P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800b97d2	 #-0x1.72fa4000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L839
	ret
L839:


	# Test 841
	# b32+ =0 q -0.35255FP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x8035255f	 #-0x1.a92af800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L840
	ret
L840:


	# Test 842
	# b32+ =0 q -0.76C165P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x8076c165	 #-0x1.db059400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L841
	ret
L841:


	# Test 843
	# b32+ =0 -Inf -0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L842
	ret
L842:


	# Test 844
	# b32+ =0 -1.7FFFFFP127 -0.000001P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L843
	ret
L843:


	# Test 845
	# b32+ =0 -1.13CB88P2 -0.000001P-126 -> -1.13CB88P2 x


	addi a1, a1, 1
	li t0, 0xc093cb88	 #-0x1.27971000p+2
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc093cb88	 #-0x1.27971000p+2
	beq t0, a0, L844
	ret
L844:


	# Test 846
	# b32+ =0 -1.000000P-126 -0.000001P-126 -> -1.000001P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800001	 #-0x1.00000200p-126
	beq t0, a0, L845
	ret
L845:


	# Test 847
	# b32+ =0 -0.7FFFFFP-126 -0.000001P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L846
	ret
L846:


	# Test 848
	# b32+ =0 -0.15FA8CP-126 -0.000001P-126 -> -0.15FA8DP-126 


	addi a1, a1, 1
	li t0, 0x8015fa8c	 #-0x1.5fa8c000p-129
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8015fa8d	 #-0x1.5fa8d000p-129
	beq t0, a0, L847
	ret
L847:


	# Test 849
	# b32+ =0 -0.000001P-126 -0.000001P-126 -> -0.000002P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000002	 #-0x1.00000000p-148
	beq t0, a0, L848
	ret
L848:


	# Test 850
	# b32+ =0 -1.000000P0 -0.000001P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L849
	ret
L849:


	# Test 851
	# b32+ =0 -Zero -0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L850
	ret
L850:


	# Test 852
	# b32+ =0 +Zero -0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L851
	ret
L851:


	# Test 853
	# b32+ =0 +1.000000P0 -0.000001P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L852
	ret
L852:


	# Test 854
	# b32+ =0 +0.000001P-126 -0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L853
	ret
L853:


	# Test 855
	# b32+ =0 +0.1A832FP-126 -0.000001P-126 -> +0.1A832EP-126 


	addi a1, a1, 1
	li t0, 0x1a832f	 #0x1.a832f000p-129
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1a832e	 #0x1.a832e000p-129
	beq t0, a0, L854
	ret
L854:


	# Test 856
	# b32+ =0 +0.7FFFFFP-126 -0.000001P-126 -> +0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7ffffe	 #0x1.fffff800p-127
	beq t0, a0, L855
	ret
L855:


	# Test 857
	# b32+ =0 +1.000000P-126 -0.000001P-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L856
	ret
L856:


	# Test 858
	# b32+ =0 +1.483A66P91 -0.000001P-126 -> +1.483A66P91 x


	addi a1, a1, 1
	li t0, 0x6d483a66	 #0x1.9074cc00p+91
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6d483a66	 #0x1.9074cc00p+91
	beq t0, a0, L857
	ret
L857:


	# Test 859
	# b32+ =0 +1.7FFFFFP127 -0.000001P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L858
	ret
L858:


	# Test 860
	# b32+ =0 +Inf -0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L859
	ret
L859:


	# Test 861
	# b32+ =0 q -0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L860
	ret
L860:


	# Test 862
	# b32+ =0 q -0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L861
	ret
L861:


	# Test 863
	# b32+ =0 -Inf -1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L862
	ret
L862:


	# Test 864
	# b32+ =0 -1.7FFFFFP127 -1.000000P0 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L863
	ret
L863:


	# Test 865
	# b32+ =0 -1.62F043P48 -1.000000P0 -> -1.62F043P48 x


	addi a1, a1, 1
	li t0, 0xd7e2f043	 #-0x1.c5e08600p+48
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd7e2f043	 #-0x1.c5e08600p+48
	beq t0, a0, L864
	ret
L864:


	# Test 866
	# b32+ =0 -1.000000P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L865
	ret
L865:


	# Test 867
	# b32+ =0 -0.7FFFFFP-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L866
	ret
L866:


	# Test 868
	# b32+ =0 -0.141F47P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80141f47	 #-0x1.41f47000p-129
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L867
	ret
L867:


	# Test 869
	# b32+ =0 -0.000001P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L868
	ret
L868:


	# Test 870
	# b32+ =0 -1.000000P0 -1.000000P0 -> -1.000000P1 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0000000	 #-0x1.00000000p+1
	beq t0, a0, L869
	ret
L869:


	# Test 871
	# b32+ =0 -Zero -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L870
	ret
L870:


	# Test 872
	# b32+ =0 +Zero -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L871
	ret
L871:


	# Test 873
	# b32+ =0 +1.000000P0 -1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L872
	ret
L872:


	# Test 874
	# b32+ =0 +0.000001P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L873
	ret
L873:


	# Test 875
	# b32+ =0 +0.1E8003P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1e8003	 #0x1.e8003000p-129
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L874
	ret
L874:


	# Test 876
	# b32+ =0 +0.7FFFFFP-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L875
	ret
L875:


	# Test 877
	# b32+ =0 +1.000000P-126 -1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L876
	ret
L876:


	# Test 878
	# b32+ =0 +1.2A96EEP-17 -1.000000P0 -> -1.7FFF55P-1 x


	addi a1, a1, 1
	li t0, 0x372a96ee	 #0x1.552ddc00p-17
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf7fff55	 #-0x1.fffeaa00p-1
	beq t0, a0, L877
	ret
L877:


	# Test 879
	# b32+ =0 +1.7FFFFFP127 -1.000000P0 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L878
	ret
L878:


	# Test 880
	# b32+ =0 +Inf -1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L879
	ret
L879:


	# Test 881
	# b32+ =0 q -1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L880
	ret
L880:


	# Test 882
	# b32+ =0 q -1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L881
	ret
L881:


	# Test 883
	# b32+ =0 -Inf -Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L882
	ret
L882:


	# Test 884
	# b32+ =0 -1.7FFFFFP127 -Zero -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L883
	ret
L883:


	# Test 885
	# b32+ =0 -1.46AD17P30 -Zero -> -1.46AD17P30 


	addi a1, a1, 1
	li t0, 0xcec6ad17	 #-0x1.8d5a2e00p+30
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcec6ad17	 #-0x1.8d5a2e00p+30
	beq t0, a0, L884
	ret
L884:


	# Test 886
	# b32+ =0 -1.000000P-126 -Zero -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L885
	ret
L885:


	# Test 887
	# b32+ =0 -0.7FFFFFP-126 -Zero -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L886
	ret
L886:


	# Test 888
	# b32+ =0 -0.0B0402P-126 -Zero -> -0.0B0402P-126 


	addi a1, a1, 1
	li t0, 0x800b0402	 #-0x1.60804000p-130
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff800b0402	 #-0x1.60804000p-130
	beq t0, a0, L887
	ret
L887:


	# Test 889
	# b32+ =0 -0.000001P-126 -Zero -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L888
	ret
L888:


	# Test 890
	# b32+ =0 -1.000000P0 -Zero -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L889
	ret
L889:


	# Test 891
	# b32+ =0 -Zero -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L890
	ret
L890:


	# Test 892
	# b32+ =0 +Zero -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L891
	ret
L891:


	# Test 893
	# b32+ =0 +1.000000P0 -Zero -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L892
	ret
L892:


	# Test 894
	# b32+ =0 +0.000001P-126 -Zero -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L893
	ret
L893:


	# Test 895
	# b32+ =0 +0.0D24BEP-126 -Zero -> +0.0D24BEP-126 


	addi a1, a1, 1
	li t0, 0xd24be	 #0x1.a497c000p-130
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xd24be	 #0x1.a497c000p-130
	beq t0, a0, L894
	ret
L894:


	# Test 896
	# b32+ =0 +0.7FFFFFP-126 -Zero -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L895
	ret
L895:


	# Test 897
	# b32+ =0 +1.000000P-126 -Zero -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L896
	ret
L896:


	# Test 898
	# b32+ =0 +1.0E93C2P-67 -Zero -> +1.0E93C2P-67 


	addi a1, a1, 1
	li t0, 0x1e0e93c2	 #0x1.1d278400p-67
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1e0e93c2	 #0x1.1d278400p-67
	beq t0, a0, L897
	ret
L897:


	# Test 899
	# b32+ =0 +1.7FFFFFP127 -Zero -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L898
	ret
L898:


	# Test 900
	# b32+ =0 +Inf -Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L899
	ret
L899:


	# Test 901
	# b32+ =0 q -Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L900
	ret
L900:


	# Test 902
	# b32+ =0 q -Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L901
	ret
L901:


	# Test 903
	# b32+ =0 -Inf +Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L902
	ret
L902:


	# Test 904
	# b32+ =0 -1.7FFFFFP127 +Zero -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L903
	ret
L903:


	# Test 905
	# b32+ =0 -1.1551D2P77 +Zero -> -1.1551D2P77 


	addi a1, a1, 1
	li t0, 0xe61551d2	 #-0x1.2aa3a400p+77
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe61551d2	 #-0x1.2aa3a400p+77
	beq t0, a0, L904
	ret
L904:


	# Test 906
	# b32+ =0 -1.000000P-126 +Zero -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L905
	ret
L905:


	# Test 907
	# b32+ =0 -0.7FFFFFP-126 +Zero -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L906
	ret
L906:


	# Test 908
	# b32+ =0 -0.16C0D6P-126 +Zero -> -0.16C0D6P-126 


	addi a1, a1, 1
	li t0, 0x8016c0d6	 #-0x1.6c0d6000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8016c0d6	 #-0x1.6c0d6000p-129
	beq t0, a0, L907
	ret
L907:


	# Test 909
	# b32+ =0 -0.000001P-126 +Zero -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L908
	ret
L908:


	# Test 910
	# b32+ =0 -1.000000P0 +Zero -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L909
	ret
L909:


	# Test 911
	# b32+ =0 -Zero +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L910
	ret
L910:


	# Test 912
	# b32+ =0 +Zero +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L911
	ret
L911:


	# Test 913
	# b32+ =0 +1.000000P0 +Zero -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L912
	ret
L912:


	# Test 914
	# b32+ =0 +0.000001P-126 +Zero -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L913
	ret
L913:


	# Test 915
	# b32+ =0 +0.07C979P-126 +Zero -> +0.07C979P-126 


	addi a1, a1, 1
	li t0, 0x7c979	 #0x1.f25e4000p-131
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7c979	 #0x1.f25e4000p-131
	beq t0, a0, L914
	ret
L914:


	# Test 916
	# b32+ =0 +0.7FFFFFP-126 +Zero -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L915
	ret
L915:


	# Test 917
	# b32+ =0 +1.000000P-126 +Zero -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L916
	ret
L916:


	# Test 918
	# b32+ =0 +1.5DB87DP-85 +Zero -> +1.5DB87DP-85 


	addi a1, a1, 1
	li t0, 0x155db87d	 #0x1.bb70fa00p-85
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x155db87d	 #0x1.bb70fa00p-85
	beq t0, a0, L917
	ret
L917:


	# Test 919
	# b32+ =0 +1.7FFFFFP127 +Zero -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L918
	ret
L918:


	# Test 920
	# b32+ =0 +Inf +Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L919
	ret
L919:


	# Test 921
	# b32+ =0 q +Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L920
	ret
L920:


	# Test 922
	# b32+ =0 q +Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L921
	ret
L921:


	# Test 923
	# b32+ =0 -Inf +1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L922
	ret
L922:


	# Test 924
	# b32+ =0 -1.7FFFFFP127 +1.000000P0 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L923
	ret
L923:


	# Test 925
	# b32+ =0 -1.64768EP59 +1.000000P0 -> -1.64768EP59 x


	addi a1, a1, 1
	li t0, 0xdd64768e	 #-0x1.c8ed1c00p+59
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffdd64768e	 #-0x1.c8ed1c00p+59
	beq t0, a0, L924
	ret
L924:


	# Test 926
	# b32+ =0 -1.000000P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L925
	ret
L925:


	# Test 927
	# b32+ =0 -0.7FFFFFP-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L926
	ret
L926:


	# Test 928
	# b32+ =0 -0.0DE591P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800de591	 #-0x1.bcb22000p-130
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L927
	ret
L927:


	# Test 929
	# b32+ =0 -0.000001P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L928
	ret
L928:


	# Test 930
	# b32+ =0 -1.000000P0 +1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L929
	ret
L929:


	# Test 931
	# b32+ =0 -Zero +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L930
	ret
L930:


	# Test 932
	# b32+ =0 +Zero +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L931
	ret
L931:


	# Test 933
	# b32+ =0 +1.000000P0 +1.000000P0 -> +1.000000P1 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x40000000	 #0x1.00000000p+1
	beq t0, a0, L932
	ret
L932:


	# Test 934
	# b32+ =0 +0.000001P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L933
	ret
L933:


	# Test 935
	# b32+ =0 +0.5AEE35P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x5aee35	 #0x1.6bb8d400p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L934
	ret
L934:


	# Test 936
	# b32+ =0 +0.7FFFFFP-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L935
	ret
L935:


	# Test 937
	# b32+ =0 +1.000000P-126 +1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L936
	ret
L936:


	# Test 938
	# b32+ =0 +1.2C5D38P58 +1.000000P0 -> +1.2C5D38P58 x


	addi a1, a1, 1
	li t0, 0x5cac5d38	 #0x1.58ba7000p+58
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5cac5d38	 #0x1.58ba7000p+58
	beq t0, a0, L937
	ret
L937:


	# Test 939
	# b32+ =0 +1.7FFFFFP127 +1.000000P0 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L938
	ret
L938:


	# Test 940
	# b32+ =0 +Inf +1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L939
	ret
L939:


	# Test 941
	# b32+ =0 q +1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L940
	ret
L940:


	# Test 942
	# b32+ =0 q +1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L941
	ret
L941:


	# Test 943
	# b32+ =0 -Inf +0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L942
	ret
L942:


	# Test 944
	# b32+ =0 -1.7FFFFFP127 +0.000001P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L943
	ret
L943:


	# Test 945
	# b32+ =0 -1.469316P-113 +0.000001P-126 -> -1.469316P-113 x


	addi a1, a1, 1
	li t0, 0x87469316	 #-0x1.8d262c00p-113
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff87469316	 #-0x1.8d262c00p-113
	beq t0, a0, L944
	ret
L944:


	# Test 946
	# b32+ =0 -1.000000P-126 +0.000001P-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L945
	ret
L945:


	# Test 947
	# b32+ =0 -0.7FFFFFP-126 +0.000001P-126 -> -0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ffffe	 #-0x1.fffff800p-127
	beq t0, a0, L946
	ret
L946:


	# Test 948
	# b32+ =0 -0.020A4CP-126 +0.000001P-126 -> -0.020A4BP-126 


	addi a1, a1, 1
	li t0, 0x80020a4c	 #-0x1.05260000p-132
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80020a4b	 #-0x1.05258000p-132
	beq t0, a0, L947
	ret
L947:


	# Test 949
	# b32+ =0 -0.000001P-126 +0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L948
	ret
L948:


	# Test 950
	# b32+ =0 -1.000000P0 +0.000001P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L949
	ret
L949:


	# Test 951
	# b32+ =0 -Zero +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L950
	ret
L950:


	# Test 952
	# b32+ =0 +Zero +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L951
	ret
L951:


	# Test 953
	# b32+ =0 +1.000000P0 +0.000001P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L952
	ret
L952:


	# Test 954
	# b32+ =0 +0.000001P-126 +0.000001P-126 -> +0.000002P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2	 #0x1.00000000p-148
	beq t0, a0, L953
	ret
L953:


	# Test 955
	# b32+ =0 +0.02AB08P-126 +0.000001P-126 -> +0.02AB09P-126 


	addi a1, a1, 1
	li t0, 0x2ab08	 #0x1.55840000p-132
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2ab09	 #0x1.55848000p-132
	beq t0, a0, L954
	ret
L954:


	# Test 956
	# b32+ =0 +0.7FFFFFP-126 +0.000001P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L955
	ret
L955:


	# Test 957
	# b32+ =0 +1.000000P-126 +0.000001P-126 -> +1.000001P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800001	 #0x1.00000200p-126
	beq t0, a0, L956
	ret
L956:


	# Test 958
	# b32+ =0 +1.65E9DBP41 +0.000001P-126 -> +1.65E9DBP41 x


	addi a1, a1, 1
	li t0, 0x5465e9db	 #0x1.cbd3b600p+41
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5465e9db	 #0x1.cbd3b600p+41
	beq t0, a0, L957
	ret
L957:


	# Test 959
	# b32+ =0 +1.7FFFFFP127 +0.000001P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L958
	ret
L958:


	# Test 960
	# b32+ =0 +Inf +0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L959
	ret
L959:


	# Test 961
	# b32+ =0 q +0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L960
	ret
L960:


	# Test 962
	# b32+ =0 q +0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L961
	ret
L961:


	# Test 963
	# b32+ =0 -Inf +0.1DC5E8P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1dc5e8	 #0x1.dc5e8000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L962
	ret
L962:


	# Test 964
	# b32+ =0 -1.7FFFFFP127 +0.5DA1FBP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x5da1fb	 #0x1.7687ec00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L963
	ret
L963:


	# Test 965
	# b32+ =0 -1.1537D1P-98 +0.36D108P-126 -> -1.1537D1P-98 x


	addi a1, a1, 1
	li t0, 0x8e9537d1	 #-0x1.2a6fa200p-98
	fmv.s.x f1, t0
	li t0, 0x36d108	 #0x1.b6884000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8e9537d1	 #-0x1.2a6fa200p-98
	beq t0, a0, L964
	ret
L964:


	# Test 966
	# b32+ =0 -1.000000P-126 +0.679065P-126 -> -0.186F9BP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x679065	 #0x1.9e419400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80186f9b	 #-0x1.86f9b000p-129
	beq t0, a0, L965
	ret
L965:


	# Test 967
	# b32+ =0 -0.7FFFFFP-126 +0.72EFCDP-126 -> -0.0D1032P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x72efcd	 #0x1.cbbf3400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff800d1032	 #-0x1.a2064000p-130
	beq t0, a0, L966
	ret
L966:


	# Test 968
	# b32+ =0 -0.532F08P-126 +0.3F671FP-126 -> -0.13C7E9P-126 


	addi a1, a1, 1
	li t0, 0x80532f08	 #-0x1.4cbc2000p-127
	fmv.s.x f1, t0
	li t0, 0x3f671f	 #0x1.fb38f800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8013c7e9	 #-0x1.3c7e9000p-129
	beq t0, a0, L967
	ret
L967:


	# Test 969
	# b32+ =0 -0.000001P-126 +0.14F1F0P-126 -> +0.14F1EFP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x14f1f0	 #0x1.4f1f0000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x14f1ef	 #0x1.4f1ef000p-129
	beq t0, a0, L968
	ret
L968:


	# Test 970
	# b32+ =0 -1.000000P0 +0.7FD157P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fd157	 #0x1.ff455c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L969
	ret
L969:


	# Test 971
	# b32+ =0 -Zero +0.531D05P-126 -> +0.531D05P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x531d05	 #0x1.4c741400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x531d05	 #0x1.4c741400p-127
	beq t0, a0, L970
	ret
L970:


	# Test 972
	# b32+ =0 +Zero +0.49BFC1P-126 -> +0.49BFC1P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x49bfc1	 #0x1.26ff0400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x49bfc1	 #0x1.26ff0400p-127
	beq t0, a0, L971
	ret
L971:


	# Test 973
	# b32+ =0 +1.000000P0 +0.0CDF29P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xcdf29	 #0x1.9be52000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L972
	ret
L972:


	# Test 974
	# b32+ =0 +0.000001P-126 +0.607E90P-126 -> +0.607E91P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x607e90	 #0x1.81fa4000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x607e91	 #0x1.81fa4400p-127
	beq t0, a0, L973
	ret
L973:


	# Test 975
	# b32+ =0 +0.214790P-126 +0.40560DP-126 -> +0.619D9DP-126 


	addi a1, a1, 1
	li t0, 0x214790	 #0x1.0a3c8000p-128
	fmv.s.x f1, t0
	li t0, 0x40560d	 #0x1.01583400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x619d9d	 #0x1.86767400p-127
	beq t0, a0, L974
	ret
L974:


	# Test 976
	# b32+ =0 +0.7FFFFFP-126 +0.6A2CFAP-126 -> +1.6A2CF9P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x6a2cfa	 #0x1.a8b3e800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xea2cf9	 #0x1.d459f200p-126
	beq t0, a0, L975
	ret
L975:


	# Test 977
	# b32+ =0 +1.000000P-126 +0.0D0C61P-126 -> +1.0D0C61P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xd0c61	 #0x1.a18c2000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x8d0c61	 #0x1.1a18c200p-126
	beq t0, a0, L976
	ret
L976:


	# Test 978
	# b32+ =0 +1.49A6AFP23 +0.74C366P-126 -> +1.49A6AFP23 x


	addi a1, a1, 1
	li t0, 0x4b49a6af	 #0x1.934d5e00p+23
	fmv.s.x f1, t0
	li t0, 0x74c366	 #0x1.d30d9800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4b49a6af	 #0x1.934d5e00p+23
	beq t0, a0, L977
	ret
L977:


	# Test 979
	# b32+ =0 +1.7FFFFFP127 +0.0BCB30P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbcb30	 #0x1.79660000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L978
	ret
L978:


	# Test 980
	# b32+ =0 +Inf +0.56EA98P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x56ea98	 #0x1.5baa6000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L979
	ret
L979:


	# Test 981
	# b32+ =0 q +0.68F0BEP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x68f0be	 #0x1.a3c2f800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L980
	ret
L980:


	# Test 982
	# b32+ =0 q +0.0E7583P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xe7583	 #0x1.ceb06000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L981
	ret
L981:


	# Test 983
	# b32+ =0 -Inf +0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L982
	ret
L982:


	# Test 984
	# b32+ =0 -1.7FFFFFP127 +0.7FFFFFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L983
	ret
L983:


	# Test 985
	# b32+ =0 -1.7974A5P-85 +0.7FFFFFP-126 -> -1.7974A5P-85 x


	addi a1, a1, 1
	li t0, 0x957974a5	 #-0x1.f2e94a00p-85
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff957974a5	 #-0x1.f2e94a00p-85
	beq t0, a0, L984
	ret
L984:


	# Test 986
	# b32+ =0 -1.000000P-126 +0.7FFFFFP-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L985
	ret
L985:


	# Test 987
	# b32+ =0 -0.7FFFFFP-126 +0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L986
	ret
L986:


	# Test 988
	# b32+ =0 -0.4CFBABP-126 +0.7FFFFFP-126 -> +0.330454P-126 


	addi a1, a1, 1
	li t0, 0x804cfbab	 #-0x1.33eeac00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x330454	 #0x1.9822a000p-128
	beq t0, a0, L987
	ret
L987:


	# Test 989
	# b32+ =0 -0.000001P-126 +0.7FFFFFP-126 -> +0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7ffffe	 #0x1.fffff800p-127
	beq t0, a0, L988
	ret
L988:


	# Test 990
	# b32+ =0 -1.000000P0 +0.7FFFFFP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L989
	ret
L989:


	# Test 991
	# b32+ =0 -Zero +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L990
	ret
L990:


	# Test 992
	# b32+ =0 +Zero +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L991
	ret
L991:


	# Test 993
	# b32+ =0 +1.000000P0 +0.7FFFFFP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L992
	ret
L992:


	# Test 994
	# b32+ =0 +0.000001P-126 +0.7FFFFFP-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L993
	ret
L993:


	# Test 995
	# b32+ =0 +0.1AD433P-126 +0.7FFFFFP-126 -> +1.1AD432P-126 


	addi a1, a1, 1
	li t0, 0x1ad433	 #0x1.ad433000p-129
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x9ad432	 #0x1.35a86400p-126
	beq t0, a0, L994
	ret
L994:


	# Test 996
	# b32+ =0 +0.7FFFFFP-126 +0.7FFFFFP-126 -> +1.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffe	 #0x1.fffffc00p-126
	beq t0, a0, L995
	ret
L995:


	# Test 997
	# b32+ =0 +1.000000P-126 +0.7FFFFFP-126 -> +1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffff	 #0x1.fffffe00p-126
	beq t0, a0, L996
	ret
L996:


	# Test 998
	# b32+ =0 +1.184B6AP6 +0.7FFFFFP-126 -> +1.184B6AP6 x


	addi a1, a1, 1
	li t0, 0x42984b6a	 #0x1.3096d400p+6
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x42984b6a	 #0x1.3096d400p+6
	beq t0, a0, L997
	ret
L997:


	# Test 999
	# b32+ =0 +1.7FFFFFP127 +0.7FFFFFP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L998
	ret
L998:


	# Test 1000
	# b32+ =0 +Inf +0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L999
	ret
L999:


	# Test 1001
	# b32+ =0 q +0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1000
	ret
L1000:


	# Test 1002
	# b32+ =0 q +0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1001
	ret
L1001:


	# Test 1003
	# b32+ =0 -Inf +1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1002
	ret
L1002:


	# Test 1004
	# b32+ =0 -1.7FFFFFP127 +1.000000P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1003
	ret
L1003:


	# Test 1005
	# b32+ =0 -1.481960P26 +1.000000P-126 -> -1.481960P26 x


	addi a1, a1, 1
	li t0, 0xccc81960	 #-0x1.9032c000p+26
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffccc81960	 #-0x1.9032c000p+26
	beq t0, a0, L1004
	ret
L1004:


	# Test 1006
	# b32+ =0 -1.000000P-126 +1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1005
	ret
L1005:


	# Test 1007
	# b32+ =0 -0.7FFFFFP-126 +1.000000P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1006
	ret
L1006:


	# Test 1008
	# b32+ =0 -0.70B87EP-126 +1.000000P-126 -> +0.0F4782P-126 


	addi a1, a1, 1
	li t0, 0x8070b87e	 #-0x1.c2e1f800p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xf4782	 #0x1.e8f04000p-130
	beq t0, a0, L1007
	ret
L1007:


	# Test 1009
	# b32+ =0 -0.000001P-126 +1.000000P-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1008
	ret
L1008:


	# Test 1010
	# b32+ =0 -1.000000P0 +1.000000P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1009
	ret
L1009:


	# Test 1011
	# b32+ =0 -Zero +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1010
	ret
L1010:


	# Test 1012
	# b32+ =0 +Zero +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1011
	ret
L1011:


	# Test 1013
	# b32+ =0 +1.000000P0 +1.000000P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1012
	ret
L1012:


	# Test 1014
	# b32+ =0 +0.000001P-126 +1.000000P-126 -> +1.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800001	 #0x1.00000200p-126
	beq t0, a0, L1013
	ret
L1013:


	# Test 1015
	# b32+ =0 +0.0978EFP-126 +1.000000P-126 -> +1.0978EFP-126 


	addi a1, a1, 1
	li t0, 0x978ef	 #0x1.2f1de000p-130
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x8978ef	 #0x1.12f1de00p-126
	beq t0, a0, L1014
	ret
L1014:


	# Test 1016
	# b32+ =0 +0.7FFFFFP-126 +1.000000P-126 -> +1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffff	 #0x1.fffffe00p-126
	beq t0, a0, L1015
	ret
L1015:


	# Test 1017
	# b32+ =0 +1.000000P-126 +1.000000P-126 -> +1.000000P-125 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1000000	 #0x1.00000000p-125
	beq t0, a0, L1016
	ret
L1016:


	# Test 1018
	# b32+ =0 +1.673025P-12 +1.000000P-126 -> +1.673025P-12 x


	addi a1, a1, 1
	li t0, 0x39e73025	 #0x1.ce604a00p-12
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x39e73025	 #0x1.ce604a00p-12
	beq t0, a0, L1017
	ret
L1017:


	# Test 1019
	# b32+ =0 +1.7FFFFFP127 +1.000000P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1018
	ret
L1018:


	# Test 1020
	# b32+ =0 +Inf +1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1019
	ret
L1019:


	# Test 1021
	# b32+ =0 q +1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1020
	ret
L1020:


	# Test 1022
	# b32+ =0 q +1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1021
	ret
L1021:


	# Test 1023
	# b32+ =0 -Inf +1.5F0580P86 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x6adf0580	 #0x1.be0b0000p+86
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1022
	ret
L1022:


	# Test 1024
	# b32+ =0 -1.7FFFFFP127 +1.6A64E8P115 -> -1.7FF159P127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x796a64e8	 #0x1.d4c9d000p+115
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7ff159	 #-0x1.ffe2b200p+127
	beq t0, a0, L1023
	ret
L1023:


	# Test 1025
	# b32+ =0 -1.01A603P10 +1.2CB155P95 -> +1.2CB155P95 x


	addi a1, a1, 1
	li t0, 0xc481a603	 #-0x1.034c0600p+10
	fmv.s.x f1, t0
	li t0, 0x6f2cb155	 #0x1.5962aa00p+95
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6f2cb155	 #0x1.5962aa00p+95
	beq t0, a0, L1024
	ret
L1024:


	# Test 1026
	# b32+ =0 -1.000000P-126 +1.6C670BP39 -> +1.6C670BP39 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x536c670b	 #0x1.d8ce1600p+39
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x536c670b	 #0x1.d8ce1600p+39
	beq t0, a0, L1025
	ret
L1025:


	# Test 1027
	# b32+ =0 -0.7FFFFFP-126 +1.3FF2B9P80 -> +1.3FF2B9P80 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x67bff2b9	 #0x1.7fe57200p+80
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x67bff2b9	 #0x1.7fe57200p+80
	beq t0, a0, L1026
	ret
L1026:


	# Test 1028
	# b32+ =0 -0.7F5D3AP-126 +1.35076CP104 -> +1.35076CP104 x


	addi a1, a1, 1
	li t0, 0x807f5d3a	 #-0x1.fd74e800p-127
	fmv.s.x f1, t0
	li t0, 0x73b5076c	 #0x1.6a0ed800p+104
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x73b5076c	 #0x1.6a0ed800p+104
	beq t0, a0, L1027
	ret
L1027:


	# Test 1029
	# b32+ =0 -0.000001P-126 +1.4134DCP-4 -> +1.4134DCP-4 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3dc134dc	 #0x1.8269b800p-4
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3dc134dc	 #0x1.8269b800p-4
	beq t0, a0, L1028
	ret
L1028:


	# Test 1030
	# b32+ =0 -1.000000P0 +1.4C5444P-95 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x104c5444	 #0x1.98a88800p-95
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1029
	ret
L1029:


	# Test 1031
	# b32+ =0 -Zero +1.57F3ABP-58 -> +1.57F3ABP-58 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x22d7f3ab	 #0x1.afe75600p-58
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x22d7f3ab	 #0x1.afe75600p-58
	beq t0, a0, L1030
	ret
L1030:


	# Test 1032
	# b32+ =0 +Zero +1.2B7F59P-81 -> +1.2B7F59P-81 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x172b7f59	 #0x1.56feb200p-81
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x172b7f59	 #0x1.56feb200p-81
	beq t0, a0, L1031
	ret
L1031:


	# Test 1033
	# b32+ =0 +1.000000P0 +1.21A215P-42 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x2aa1a215	 #0x1.43442a00p-42
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1032
	ret
L1032:


	# Test 1034
	# b32+ =0 +0.000001P-126 +1.41BE28P-7 -> +1.41BE28P-7 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3c41be28	 #0x1.837c5000p-7
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3c41be28	 #0x1.837c5000p-7
	beq t0, a0, L1033
	ret
L1033:


	# Test 1035
	# b32+ =0 +0.005DAAP-126 +1.420D9CP59 -> +1.420D9CP59 x


	addi a1, a1, 1
	li t0, 0x5daa	 #0x1.76a80000p-135
	fmv.s.x f1, t0
	li t0, 0x5d420d9c	 #0x1.841b3800p+59
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5d420d9c	 #0x1.841b3800p+59
	beq t0, a0, L1034
	ret
L1034:


	# Test 1036
	# b32+ =0 +0.7FFFFFP-126 +1.43404BP-107 -> +1.43405BP-107 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xa43404b	 #0x1.86809600p-107
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xa43405b	 #0x1.8680b600p-107
	beq t0, a0, L1035
	ret
L1035:


	# Test 1037
	# b32+ =0 +1.000000P-126 +1.174BF9P78 -> +1.174BF9P78 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x66974bf9	 #0x1.2e97f200p+78
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x66974bf9	 #0x1.2e97f200p+78
	beq t0, a0, L1036
	ret
L1036:


	# Test 1038
	# b32+ =0 +1.35D4E1P-29 +1.4A63B3P100 -> +1.4A63B3P100 x


	addi a1, a1, 1
	li t0, 0x3135d4e1	 #0x1.6ba9c200p-29
	fmv.s.x f1, t0
	li t0, 0x71ca63b3	 #0x1.94c76600p+100
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x71ca63b3	 #0x1.94c76600p+100
	beq t0, a0, L1037
	ret
L1037:


	# Test 1039
	# b32+ =0 +1.7FFFFFP127 +1.2D0AC8P24 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x4bad0ac8	 #0x1.5a159000p+24
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1038
	ret
L1038:


	# Test 1040
	# b32+ =0 +Inf +1.23AD84P63 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x5f23ad84	 #0x1.475b0800p+63
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1039
	ret
L1039:


	# Test 1041
	# b32+ =0 q +1.622084P14 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x46e22084	 #0x1.c4410800p+14
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1040
	ret
L1040:


	# Test 1042
	# b32+ =0 q +1.13D5D0P48 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x5793d5d0	 #0x1.27aba000p+48
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1041
	ret
L1041:


	# Test 1043
	# b32+ =0 -Inf +1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1042
	ret
L1042:


	# Test 1044
	# b32+ =0 -1.7FFFFFP127 +1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1043
	ret
L1043:


	# Test 1045
	# b32+ =0 -1.504ABEP-112 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x87d04abe	 #-0x1.a0957c00p-112
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1044
	ret
L1044:


	# Test 1046
	# b32+ =0 -1.000000P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1045
	ret
L1045:


	# Test 1047
	# b32+ =0 -0.7FFFFFP-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1046
	ret
L1046:


	# Test 1048
	# b32+ =0 -0.4D81F5P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x804d81f5	 #-0x1.3607d400p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1047
	ret
L1047:


	# Test 1049
	# b32+ =0 -0.000001P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1048
	ret
L1048:


	# Test 1050
	# b32+ =0 -1.000000P0 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1049
	ret
L1049:


	# Test 1051
	# b32+ =0 -Zero +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1050
	ret
L1050:


	# Test 1052
	# b32+ =0 +Zero +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1051
	ret
L1051:


	# Test 1053
	# b32+ =0 +1.000000P0 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1052
	ret
L1052:


	# Test 1054
	# b32+ =0 +0.000001P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1053
	ret
L1053:


	# Test 1055
	# b32+ =0 +0.045A7EP-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x45a7e	 #0x1.169f8000p-131
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1054
	ret
L1054:


	# Test 1056
	# b32+ =0 +0.7FFFFFP-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1055
	ret
L1055:


	# Test 1057
	# b32+ =0 +1.000000P-126 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1056
	ret
L1056:


	# Test 1058
	# b32+ =0 +1.04F99CP-46 +1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x2884f99c	 #0x1.09f33800p-46
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1057
	ret
L1057:


	# Test 1059
	# b32+ =0 +1.7FFFFFP127 +1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1058
	ret
L1058:


	# Test 1060
	# b32+ =0 +Inf +1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1059
	ret
L1059:


	# Test 1061
	# b32+ =0 q +1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1060
	ret
L1060:


	# Test 1062
	# b32+ =0 q +1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1061
	ret
L1061:


	# Test 1063
	# b32+ =0 -Inf +Inf -> q i


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1062
	ret
L1062:


	# Test 1064
	# b32+ =0 -1.7FFFFFP127 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1063
	ret
L1063:


	# Test 1065
	# b32+ =0 -1.1EAF79P-25 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xb31eaf79	 #-0x1.3d5ef200p-25
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1064
	ret
L1064:


	# Test 1066
	# b32+ =0 -1.000000P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1065
	ret
L1065:


	# Test 1067
	# b32+ =0 -0.7FFFFFP-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1066
	ret
L1066:


	# Test 1068
	# b32+ =0 -0.5CA6B0P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x805ca6b0	 #-0x1.729ac000p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1067
	ret
L1067:


	# Test 1069
	# b32+ =0 -0.000001P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1068
	ret
L1068:


	# Test 1070
	# b32+ =0 -1.000000P0 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1069
	ret
L1069:


	# Test 1071
	# b32+ =0 -Zero +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1070
	ret
L1070:


	# Test 1072
	# b32+ =0 +Zero +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1071
	ret
L1071:


	# Test 1073
	# b32+ =0 +1.000000P0 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1072
	ret
L1072:


	# Test 1074
	# b32+ =0 +0.000001P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1073
	ret
L1073:


	# Test 1075
	# b32+ =0 +0.6ABF39P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x6abf39	 #0x1.aafce400p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1074
	ret
L1074:


	# Test 1076
	# b32+ =0 +0.7FFFFFP-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1075
	ret
L1075:


	# Test 1077
	# b32+ =0 +1.000000P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1076
	ret
L1076:


	# Test 1078
	# b32+ =0 +1.68B670P-97 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xf68b670	 #0x1.d16ce000p-97
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1077
	ret
L1077:


	# Test 1079
	# b32+ =0 +1.7FFFFFP127 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1078
	ret
L1078:


	# Test 1080
	# b32+ =0 +Inf +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1079
	ret
L1079:


	# Test 1081
	# b32+ =0 q +Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1080
	ret
L1080:


	# Test 1082
	# b32+ =0 q +Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1081
	ret
L1081:


	# Test 1083
	# b32+ =0 -Inf q -> q 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1082
	ret
L1082:


	# Test 1084
	# b32+ =0 -1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1083
	ret
L1083:


	# Test 1085
	# b32+ =0 -1.02EC4DP-43 q -> q 


	addi a1, a1, 1
	li t0, 0xaa02ec4d	 #-0x1.05d89a00p-43
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1084
	ret
L1084:


	# Test 1086
	# b32+ =0 -1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1085
	ret
L1085:


	# Test 1087
	# b32+ =0 -0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1086
	ret
L1086:


	# Test 1088
	# b32+ =0 -0.40A384P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x8040a384	 #-0x1.028e1000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1087
	ret
L1087:


	# Test 1089
	# b32+ =0 -0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1088
	ret
L1088:


	# Test 1090
	# b32+ =0 -1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1089
	ret
L1089:


	# Test 1091
	# b32+ =0 -Zero q -> q 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1090
	ret
L1090:


	# Test 1092
	# b32+ =0 +Zero q -> q 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1091
	ret
L1091:


	# Test 1093
	# b32+ =0 +1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1092
	ret
L1092:


	# Test 1094
	# b32+ =0 +0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1093
	ret
L1093:


	# Test 1095
	# b32+ =0 +0.79E3F4P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x79e3f4	 #0x1.e78fd000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1094
	ret
L1094:


	# Test 1096
	# b32+ =0 +0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1095
	ret
L1095:


	# Test 1097
	# b32+ =0 +1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1096
	ret
L1096:


	# Test 1098
	# b32+ =0 +1.375B2BP-98 q -> q 


	addi a1, a1, 1
	li t0, 0xeb75b2b	 #0x1.6eb65600p-98
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1097
	ret
L1097:


	# Test 1099
	# b32+ =0 +1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1098
	ret
L1098:


	# Test 1100
	# b32+ =0 +Inf q -> q 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1099
	ret
L1099:


	# Test 1101
	# b32+ =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1100
	ret
L1100:


	# Test 1102
	# b32+ =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1101
	ret
L1101:


	# Test 1103
	# b32+ =0 -Inf q -> q 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1102
	ret
L1102:


	# Test 1104
	# b32+ =0 -1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1103
	ret
L1103:


	# Test 1105
	# b32+ =0 -1.519108P-61 q -> q 


	addi a1, a1, 1
	li t0, 0xa1519108	 #-0x1.a3221000p-61
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1104
	ret
L1104:


	# Test 1106
	# b32+ =0 -1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1105
	ret
L1105:


	# Test 1107
	# b32+ =0 -0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1106
	ret
L1106:


	# Test 1108
	# b32+ =0 -0.63000CP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x8063000c	 #-0x1.8c003000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1107
	ret
L1107:


	# Test 1109
	# b32+ =0 -0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1108
	ret
L1108:


	# Test 1110
	# b32+ =0 -1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1109
	ret
L1109:


	# Test 1111
	# b32+ =0 -Zero q -> q 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1110
	ret
L1110:


	# Test 1112
	# b32+ =0 +Zero q -> q 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1111
	ret
L1111:


	# Test 1113
	# b32+ =0 +1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1112
	ret
L1112:


	# Test 1114
	# b32+ =0 +0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1113
	ret
L1113:


	# Test 1115
	# b32+ =0 +0.4808AFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x4808af	 #0x1.2022bc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1114
	ret
L1114:


	# Test 1116
	# b32+ =0 +0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1115
	ret
L1115:


	# Test 1117
	# b32+ =0 +1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1116
	ret
L1116:


	# Test 1118
	# b32+ =0 +1.19F7B3P114 q -> q 


	addi a1, a1, 1
	li t0, 0x7899f7b3	 #0x1.33ef6600p+114
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1117
	ret
L1117:


	# Test 1119
	# b32+ =0 +1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1118
	ret
L1118:


	# Test 1120
	# b32+ =0 +Inf q -> q 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1119
	ret
L1119:


	# Test 1121
	# b32+ =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1120
	ret
L1120:


	# Test 1122
	# b32+ =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fadd.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1121
	ret
L1121:


	# Test 1123
	# b32- =0 i -1.7FFFFFP127 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1122
	ret
L1122:


	# Test 1124
	# b32- =0 i -1.202CBCP-61 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xa1202cbc	 #-0x1.40597800p-61
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1123
	ret
L1123:


	# Test 1125
	# b32- =0 i -1.000000P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1124
	ret
L1124:


	# Test 1126
	# b32- =0 i -0.7FFFFFP-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1125
	ret
L1125:


	# Test 1127
	# b32- =0 i -0.3C83A7P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x803c83a7	 #-0x1.e41d3800p-128
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1126
	ret
L1126:


	# Test 1128
	# b32- =0 i -0.000001P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1127
	ret
L1127:


	# Test 1129
	# b32- =0 i -1.000000P0 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1128
	ret
L1128:


	# Test 1130
	# b32- =0 i -Zero -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1129
	ret
L1129:


	# Test 1131
	# b32- =0 i +Zero -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1130
	ret
L1130:


	# Test 1132
	# b32- =0 i +1.000000P0 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1131
	ret
L1131:


	# Test 1133
	# b32- =0 i +0.000001P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1132
	ret
L1132:


	# Test 1134
	# b32- =0 i +0.36A463P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x36a463	 #0x1.b5231800p-128
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1133
	ret
L1133:


	# Test 1135
	# b32- =0 i +0.7FFFFFP-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1134
	ret
L1134:


	# Test 1136
	# b32- =0 i +1.000000P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1135
	ret
L1135:


	# Test 1137
	# b32- =0 i +1.681366P-63 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x20681366	 #0x1.d026cc00p-63
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1136
	ret
L1136:


	# Test 1138
	# b32- =0 i +1.7FFFFFP127 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1137
	ret
L1137:


	# Test 1139
	# b32- =0 i +Inf -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1138
	ret
L1138:


	# Test 1140
	# b32- =0 i -Inf -1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1139
	ret
L1139:


	# Test 1141
	# b32- =0 i -1.7FFFFFP127 -1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1140
	ret
L1140:


	# Test 1142
	# b32- =0 i -1.6E9177P49 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xd86e9177	 #-0x1.dd22ee00p+49
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1141
	ret
L1141:


	# Test 1143
	# b32- =0 i -1.000000P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1142
	ret
L1142:


	# Test 1144
	# b32- =0 i -0.7FFFFFP-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1143
	ret
L1143:


	# Test 1145
	# b32- =0 i -0.20C07BP-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x8020c07b	 #-0x1.0603d800p-128
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1144
	ret
L1144:


	# Test 1146
	# b32- =0 i -0.000001P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1145
	ret
L1145:


	# Test 1147
	# b32- =0 i -1.000000P0 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1146
	ret
L1146:


	# Test 1148
	# b32- =0 i -Zero -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1147
	ret
L1147:


	# Test 1149
	# b32- =0 i +Zero -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1148
	ret
L1148:


	# Test 1150
	# b32- =0 i +1.000000P0 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1149
	ret
L1149:


	# Test 1151
	# b32- =0 i +0.000001P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1150
	ret
L1150:


	# Test 1152
	# b32- =0 i +0.25491EP-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x25491e	 #0x1.2a48f000p-128
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1151
	ret
L1151:


	# Test 1153
	# b32- =0 i +0.7FFFFFP-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1152
	ret
L1152:


	# Test 1154
	# b32- =0 i +1.000000P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1153
	ret
L1153:


	# Test 1155
	# b32- =0 i +1.373822P-16 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x37b73822	 #0x1.6e704400p-16
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1154
	ret
L1154:


	# Test 1156
	# b32- =0 i +1.7FFFFFP127 -1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1155
	ret
L1155:


	# Test 1157
	# b32- =0 i +Inf -1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1156
	ret
L1156:


	# Test 1158
	# b32- =0 i -Inf -1.5FCE9BP60 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xdddfce9b	 #-0x1.bf9d3600p+60
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1157
	ret
L1157:


	# Test 1159
	# b32- =0 i -1.7FFFFFP127 -1.6AAE03P1 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xc06aae03	 #-0x1.d55c0600p+1
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1158
	ret
L1158:


	# Test 1160
	# b32- =0 i -1.3DB632P32 -1.1EFB3FP-64 -> -1.3DB632P32 x


	addi a1, a1, 1
	li t0, 0xcfbdb632	 #-0x1.7b6c6400p+32
	fmv.s.x f1, t0
	li t0, 0x9f9efb3f	 #-0x1.3df67e00p-64
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcfbdb632	 #-0x1.7b6c6400p+32
	beq t0, a0, L1159
	ret
L1159:


	# Test 1161
	# b32- =0 i -1.000000P-126 -1.355C6DP125 -> +1.355C6DP125 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xfe355c6d	 #-0x1.6ab8da00p+125
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e355c6d	 #0x1.6ab8da00p+125
	beq t0, a0, L1160
	ret
L1160:


	# Test 1162
	# b32- =0 i -0.7FFFFFP-126 -1.403BD4P22 -> +1.403BD4P22 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xcac03bd4	 #-0x1.8077a800p+22
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4ac03bd4	 #0x1.8077a800p+22
	beq t0, a0, L1161
	ret
L1161:


	# Test 1163
	# b32- =0 i -0.2F2536P-126 -1.36B810P90 -> +1.36B810P90 x


	addi a1, a1, 1
	li t0, 0x802f2536	 #-0x1.7929b000p-128
	fmv.s.x f1, t0
	li t0, 0xecb6b810	 #-0x1.6d702000p+90
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6cb6b810	 #0x1.6d702000p+90
	beq t0, a0, L1162
	ret
L1162:


	# Test 1164
	# b32- =0 i -0.000001P-126 -1.41BDF7P-62 -> +1.41BDF7P-62 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xa0c1bdf7	 #-0x1.837bee00p-62
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x20c1bdf7	 #0x1.837bee00p-62
	beq t0, a0, L1163
	ret
L1163:


	# Test 1165
	# b32- =0 i -1.000000P0 -1.1589A5P-85 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x951589a5	 #-0x1.2b134a00p-85
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1164
	ret
L1164:


	# Test 1166
	# b32- =0 i -Zero -1.20E90DP-16 -> +1.20E90DP-16 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xb7a0e90d	 #-0x1.41d21a00p-16
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x37a0e90d	 #0x1.41d21a00p-16
	beq t0, a0, L1165
	ret
L1165:


	# Test 1167
	# b32- =0 i +Zero -1.170BC9P-41 -> +1.170BC9P-41 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xab170bc9	 #-0x1.2e179200p-41
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2b170bc9	 #0x1.2e179200p-41
	beq t0, a0, L1166
	ret
L1166:


	# Test 1168
	# b32- =0 i +1.000000P0 -1.36E7DCP-6 -> +1.02DB9FP0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbcb6e7dc	 #-0x1.6dcfb800p-6
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f82db9f	 #0x1.05b73e00p+0
	beq t0, a0, L1167
	ret
L1167:


	# Test 1169
	# b32- =0 i +0.000001P-126 -1.420743P-65 -> +1.420743P-65 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x9f420743	 #-0x1.840e8600p-65
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1f420743	 #0x1.840e8600p-65
	beq t0, a0, L1168
	ret
L1168:


	# Test 1170
	# b32- =0 i +0.142DD9P-126 -1.345786P28 -> +1.345786P28 x


	addi a1, a1, 1
	li t0, 0x142dd9	 #0x1.42dd9000p-129
	fmv.s.x f1, t0
	li t0, 0xcdb45786	 #-0x1.68af0c00p+28
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4db45786	 #0x1.68af0c00p+28
	beq t0, a0, L1169
	ret
L1169:


	# Test 1171
	# b32- =0 i +0.7FFFFFP-126 -1.0C35ADP-49 -> +1.0C35ADP-49 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xa70c35ad	 #-0x1.186b5a00p-49
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x270c35ad	 #0x1.186b5a00p-49
	beq t0, a0, L1170
	ret
L1170:


	# Test 1172
	# b32- =0 i +1.000000P-126 -1.175514P-44 -> +1.175514P-44 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xa9975514	 #-0x1.2eaa2800p-44
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x29975514	 #0x1.2eaa2800p-44
	beq t0, a0, L1171
	ret
L1171:


	# Test 1173
	# b32- =0 i +1.70C4C4P95 -1.77AB98P-29 -> +1.70C4C4P95 x


	addi a1, a1, 1
	li t0, 0x6f70c4c4	 #0x1.e1898800p+95
	fmv.s.x f1, t0
	li t0, 0xb177ab98	 #-0x1.ef573000p-29
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6f70c4c4	 #0x1.e1898800p+95
	beq t0, a0, L1172
	ret
L1172:


	# Test 1174
	# b32- =0 i +1.7FFFFFP127 -1.189738P64 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xdf989738	 #-0x1.312e7000p+64
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1173
	ret
L1173:


	# Test 1175
	# b32- =0 i +Inf -1.6CE2E6P40 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xd3ece2e6	 #-0x1.d9c5cc00p+40
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1174
	ret
L1174:


	# Test 1176
	# b32- =0 i -Inf -1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1175
	ret
L1175:


	# Test 1177
	# b32- =0 i -1.7FFFFFP127 -1.000000P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1176
	ret
L1176:


	# Test 1178
	# b32- =0 i -1.0C5AEEP15 -1.000000P-126 -> -1.0C5AEEP15 x


	addi a1, a1, 1
	li t0, 0xc70c5aee	 #-0x1.18b5dc00p+15
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc70c5aee	 #-0x1.18b5dc00p+15
	beq t0, a0, L1177
	ret
L1177:


	# Test 1179
	# b32- =0 i -1.000000P-126 -1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1178
	ret
L1178:


	# Test 1180
	# b32- =0 i -0.7FFFFFP-126 -1.000000P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1179
	ret
L1179:


	# Test 1181
	# b32- =0 i -0.3DC9F1P-126 -1.000000P-126 -> +0.42360FP-126 


	addi a1, a1, 1
	li t0, 0x803dc9f1	 #-0x1.ee4f8800p-128
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x42360f	 #0x1.08d83c00p-127
	beq t0, a0, L1180
	ret
L1180:


	# Test 1182
	# b32- =0 i -0.000001P-126 -1.000000P-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1181
	ret
L1181:


	# Test 1183
	# b32- =0 i -1.000000P0 -1.000000P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1182
	ret
L1182:


	# Test 1184
	# b32- =0 i -Zero -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1183
	ret
L1183:


	# Test 1185
	# b32- =0 i +Zero -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1184
	ret
L1184:


	# Test 1186
	# b32- =0 i +1.000000P0 -1.000000P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1185
	ret
L1185:


	# Test 1187
	# b32- =0 i +0.000001P-126 -1.000000P-126 -> +1.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800001	 #0x1.00000200p-126
	beq t0, a0, L1186
	ret
L1186:


	# Test 1188
	# b32- =0 i +0.386AADP-126 -1.000000P-126 -> +1.386AADP-126 


	addi a1, a1, 1
	li t0, 0x386aad	 #0x1.c3556800p-128
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xb86aad	 #0x1.70d55a00p-126
	beq t0, a0, L1187
	ret
L1187:


	# Test 1189
	# b32- =0 i +0.7FFFFFP-126 -1.000000P-126 -> +1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffff	 #0x1.fffffe00p-126
	beq t0, a0, L1188
	ret
L1188:


	# Test 1190
	# b32- =0 i +1.000000P-126 -1.000000P-126 -> +1.000000P-125 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1000000	 #0x1.00000000p-125
	beq t0, a0, L1189
	ret
L1189:


	# Test 1191
	# b32- =0 i +1.3F2980P14 -1.000000P-126 -> +1.3F2980P14 x


	addi a1, a1, 1
	li t0, 0x46bf2980	 #0x1.7e530000p+14
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x46bf2980	 #0x1.7e530000p+14
	beq t0, a0, L1190
	ret
L1190:


	# Test 1192
	# b32- =0 i +1.7FFFFFP127 -1.000000P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1191
	ret
L1191:


	# Test 1193
	# b32- =0 i +Inf -1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1192
	ret
L1192:


	# Test 1194
	# b32- =0 i -Inf -0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1193
	ret
L1193:


	# Test 1195
	# b32- =0 i -1.7FFFFFP127 -0.7FFFFFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1194
	ret
L1194:


	# Test 1196
	# b32- =0 i -1.6EB776P-30 -0.7FFFFFP-126 -> -1.6EB776P-30 x


	addi a1, a1, 1
	li t0, 0xb0eeb776	 #-0x1.dd6eec00p-30
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb0eeb776	 #-0x1.dd6eec00p-30
	beq t0, a0, L1195
	ret
L1195:


	# Test 1197
	# b32- =0 i -1.000000P-126 -0.7FFFFFP-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1196
	ret
L1196:


	# Test 1198
	# b32- =0 i -0.7FFFFFP-126 -0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1197
	ret
L1197:


	# Test 1199
	# b32- =0 i -0.0F5694P-126 -0.7FFFFFP-126 -> +0.70A96BP-126 


	addi a1, a1, 1
	li t0, 0x800f5694	 #-0x1.ead28000p-130
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x70a96b	 #0x1.c2a5ac00p-127
	beq t0, a0, L1198
	ret
L1198:


	# Test 1200
	# b32- =0 i -0.000001P-126 -0.7FFFFFP-126 -> +0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7ffffe	 #0x1.fffff800p-127
	beq t0, a0, L1199
	ret
L1199:


	# Test 1201
	# b32- =0 i -1.000000P0 -0.7FFFFFP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1200
	ret
L1200:


	# Test 1202
	# b32- =0 i -Zero -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1201
	ret
L1201:


	# Test 1203
	# b32- =0 i +Zero -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1202
	ret
L1202:


	# Test 1204
	# b32- =0 i +1.000000P0 -0.7FFFFFP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1203
	ret
L1203:


	# Test 1205
	# b32- =0 i +0.000001P-126 -0.7FFFFFP-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1204
	ret
L1204:


	# Test 1206
	# b32- =0 i +0.09F750P-126 -0.7FFFFFP-126 -> +1.09F74FP-126 


	addi a1, a1, 1
	li t0, 0x9f750	 #0x1.3eea0000p-130
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x89f74f	 #0x1.13ee9e00p-126
	beq t0, a0, L1205
	ret
L1205:


	# Test 1207
	# b32- =0 i +0.7FFFFFP-126 -0.7FFFFFP-126 -> +1.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffe	 #0x1.fffffc00p-126
	beq t0, a0, L1206
	ret
L1206:


	# Test 1208
	# b32- =0 i +1.000000P-126 -0.7FFFFFP-126 -> +1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffff	 #0x1.fffffe00p-126
	beq t0, a0, L1207
	ret
L1207:


	# Test 1209
	# b32- =0 i +1.236653P60 -0.7FFFFFP-126 -> +1.236653P60 x


	addi a1, a1, 1
	li t0, 0x5da36653	 #0x1.46cca600p+60
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5da36653	 #0x1.46cca600p+60
	beq t0, a0, L1208
	ret
L1208:


	# Test 1210
	# b32- =0 i +1.7FFFFFP127 -0.7FFFFFP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1209
	ret
L1209:


	# Test 1211
	# b32- =0 i +Inf -0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1210
	ret
L1210:


	# Test 1212
	# b32- =0 i -Inf -0.6CD188P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x806cd188	 #-0x1.b3462000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1211
	ret
L1211:


	# Test 1213
	# b32- =0 i -1.7FFFFFP127 -0.77B0EFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x8077b0ef	 #-0x1.dec3bc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1212
	ret
L1212:


	# Test 1214
	# b32- =0 i -1.52F449P-48 -0.586B05P-126 -> -1.52F449P-48 x


	addi a1, a1, 1
	li t0, 0xa7d2f449	 #-0x1.a5e89200p-48
	fmv.s.x f1, t0
	li t0, 0x80586b05	 #-0x1.61ac1400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa7d2f449	 #-0x1.a5e89200p-48
	beq t0, a0, L1213
	ret
L1213:


	# Test 1215
	# b32- =0 i -1.000000P-126 -0.419F59P-126 -> -0.3E60A7P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80419f59	 #-0x1.067d6400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff803e60a7	 #-0x1.f3053800p-128
	beq t0, a0, L1214
	ret
L1214:


	# Test 1216
	# b32- =0 i -0.7FFFFFP-126 -0.2D7EC0P-126 -> -0.52813FP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x802d7ec0	 #-0x1.6bf60000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8052813f	 #-0x1.4a04fc00p-127
	beq t0, a0, L1215
	ret
L1215:


	# Test 1217
	# b32- =0 i -0.017B4FP-126 -0.786F9FP-126 -> +0.76F450P-126 


	addi a1, a1, 1
	li t0, 0x80017b4f	 #-0x1.7b4f0000p-133
	fmv.s.x f1, t0
	li t0, 0x80786f9f	 #-0x1.e1be7c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x76f450	 #0x1.dbd14000p-127
	beq t0, a0, L1216
	ret
L1216:


	# Test 1218
	# b32- =0 i -0.000001P-126 -0.233D8FP-126 -> +0.233D8EP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80233d8f	 #-0x1.19ec7800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x233d8e	 #0x1.19ec7000p-128
	beq t0, a0, L1217
	ret
L1217:


	# Test 1219
	# b32- =0 i -1.000000P0 -0.6EDCF7P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x806edcf7	 #-0x1.bb73dc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1218
	ret
L1218:


	# Test 1220
	# b32- =0 i -Zero -0.2268A5P-126 -> +0.2268A5P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x802268a5	 #-0x1.13452800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2268a5	 #0x1.13452800p-128
	beq t0, a0, L1219
	ret
L1219:


	# Test 1221
	# b32- =0 i +Zero -0.78CB60P-126 -> +0.78CB60P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8078cb60	 #-0x1.e32d8000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x78cb60	 #0x1.e32d8000p-127
	beq t0, a0, L1220
	ret
L1220:


	# Test 1222
	# b32- =0 i +1.000000P0 -0.23AAC8P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8023aac8	 #-0x1.1d564000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1221
	ret
L1221:


	# Test 1223
	# b32- =0 i +0.000001P-126 -0.4ECA2FP-126 -> +0.4ECA30P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x804eca2f	 #-0x1.3b28bc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4eca30	 #0x1.3b28c000p-127
	beq t0, a0, L1222
	ret
L1222:


	# Test 1224
	# b32- =0 i +0.3453D8P-126 -0.199E8DP-126 -> +0.4DF265P-126 


	addi a1, a1, 1
	li t0, 0x3453d8	 #0x1.a29ec000p-128
	fmv.s.x f1, t0
	li t0, 0x80199e8d	 #-0x1.99e8d000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4df265	 #0x1.37c99400p-127
	beq t0, a0, L1223
	ret
L1223:


	# Test 1225
	# b32- =0 i +0.7FFFFFP-126 -0.597899P-126 -> +1.597898P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80597899	 #-0x1.65e26400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xd97898	 #0x1.b2f13000p-126
	beq t0, a0, L1224
	ret
L1224:


	# Test 1226
	# b32- =0 i +1.000000P-126 -0.241801P-126 -> +1.241801P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80241801	 #-0x1.20c00800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xa41801	 #0x1.48300200p-126
	beq t0, a0, L1225
	ret
L1225:


	# Test 1227
	# b32- =0 i +1.720B0FP42 -0.11F4A4P-126 -> +1.720B0FP42 x


	addi a1, a1, 1
	li t0, 0x54f20b0f	 #0x1.e4161e00p+42
	fmv.s.x f1, t0
	li t0, 0x8011f4a4	 #-0x1.1f4a4000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x54f20b0f	 #0x1.e4161e00p+42
	beq t0, a0, L1226
	ret
L1226:


	# Test 1228
	# b32- =0 i +1.7FFFFFP127 -0.65DA24P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x8065da24	 #-0x1.97689000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1227
	ret
L1227:


	# Test 1229
	# b32- =0 i +Inf -0.04F98BP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x8004f98b	 #-0x1.3e62c000p-131
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1228
	ret
L1228:


	# Test 1230
	# b32- =0 i -Inf -0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1229
	ret
L1229:


	# Test 1231
	# b32- =0 i -1.7FFFFFP127 -0.000001P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1230
	ret
L1230:


	# Test 1232
	# b32- =0 i -1.0C40ECP-124 -0.000001P-126 -> -1.0C40ECP-124 x


	addi a1, a1, 1
	li t0, 0x818c40ec	 #-0x1.1881d800p-124
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff818c40ec	 #-0x1.1881d800p-124
	beq t0, a0, L1231
	ret
L1231:


	# Test 1233
	# b32- =0 i -1.000000P-126 -0.000001P-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1232
	ret
L1232:


	# Test 1234
	# b32- =0 i -0.7FFFFFP-126 -0.000001P-126 -> -0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ffffe	 #-0x1.fffff800p-127
	beq t0, a0, L1233
	ret
L1233:


	# Test 1235
	# b32- =0 i -0.0A3823P-126 -0.000001P-126 -> -0.0A3822P-126 


	addi a1, a1, 1
	li t0, 0x800a3823	 #-0x1.47046000p-130
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff800a3822	 #-0x1.47044000p-130
	beq t0, a0, L1234
	ret
L1234:


	# Test 1236
	# b32- =0 i -0.000001P-126 -0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1235
	ret
L1235:


	# Test 1237
	# b32- =0 i -1.000000P0 -0.000001P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1236
	ret
L1236:


	# Test 1238
	# b32- =0 i -Zero -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1237
	ret
L1237:


	# Test 1239
	# b32- =0 i +Zero -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1238
	ret
L1238:


	# Test 1240
	# b32- =0 i +1.000000P0 -0.000001P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1239
	ret
L1239:


	# Test 1241
	# b32- =0 i +0.000001P-126 -0.000001P-126 -> +0.000002P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2	 #0x1.00000000p-148
	beq t0, a0, L1240
	ret
L1240:


	# Test 1242
	# b32- =0 i +0.237893P-126 -0.000001P-126 -> +0.237894P-126 


	addi a1, a1, 1
	li t0, 0x237893	 #0x1.1bc49800p-128
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x237894	 #0x1.1bc4a000p-128
	beq t0, a0, L1241
	ret
L1241:


	# Test 1243
	# b32- =0 i +0.7FFFFFP-126 -0.000001P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1242
	ret
L1242:


	# Test 1244
	# b32- =0 i +1.000000P-126 -0.000001P-126 -> +1.000001P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800001	 #0x1.00000200p-126
	beq t0, a0, L1243
	ret
L1243:


	# Test 1245
	# b32- =0 i +1.40AFCAP25 -0.000001P-126 -> +1.40AFCAP25 x


	addi a1, a1, 1
	li t0, 0x4c40afca	 #0x1.815f9400p+25
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4c40afca	 #0x1.815f9400p+25
	beq t0, a0, L1244
	ret
L1244:


	# Test 1246
	# b32- =0 i +1.7FFFFFP127 -0.000001P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1245
	ret
L1245:


	# Test 1247
	# b32- =0 i +Inf -0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1246
	ret
L1246:


	# Test 1248
	# b32- =0 i -Inf -1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1247
	ret
L1247:


	# Test 1249
	# b32- =0 i -1.7FFFFFP127 -1.000000P0 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1248
	ret
L1248:


	# Test 1250
	# b32- =0 i -1.5B65A8P-122 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x82db65a8	 #-0x1.b6cb5000p-122
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1249
	ret
L1249:


	# Test 1251
	# b32- =0 i -1.000000P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1250
	ret
L1250:


	# Test 1252
	# b32- =0 i -0.7FFFFFP-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1251
	ret
L1251:


	# Test 1253
	# b32- =0 i -0.589CDEP-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80589cde	 #-0x1.62737800p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1252
	ret
L1252:


	# Test 1254
	# b32- =0 i -0.000001P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1253
	ret
L1253:


	# Test 1255
	# b32- =0 i -1.000000P0 -1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1254
	ret
L1254:


	# Test 1256
	# b32- =0 i -Zero -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1255
	ret
L1255:


	# Test 1257
	# b32- =0 i +Zero -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1256
	ret
L1256:


	# Test 1258
	# b32- =0 i +1.000000P0 -1.000000P0 -> +1.000000P1 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x40000000	 #0x1.00000000p+1
	beq t0, a0, L1257
	ret
L1257:


	# Test 1259
	# b32- =0 i +0.000001P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1258
	ret
L1258:


	# Test 1260
	# b32- =0 i +0.11DD4EP-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x11dd4e	 #0x1.1dd4e000p-129
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1259
	ret
L1259:


	# Test 1261
	# b32- =0 i +0.7FFFFFP-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1260
	ret
L1260:


	# Test 1262
	# b32- =0 i +1.000000P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1261
	ret
L1261:


	# Test 1263
	# b32- =0 i +1.0F5485P-88 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x138f5485	 #0x1.1ea90a00p-88
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1262
	ret
L1262:


	# Test 1264
	# b32- =0 i +1.7FFFFFP127 -1.000000P0 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1263
	ret
L1263:


	# Test 1265
	# b32- =0 i +Inf -1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1264
	ret
L1264:


	# Test 1266
	# b32- =0 i -Inf -Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1265
	ret
L1265:


	# Test 1267
	# b32- =0 i -1.7FFFFFP127 -Zero -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1266
	ret
L1266:


	# Test 1268
	# b32- =0 i -1.298A63P-125 -Zero -> -1.298A63P-125 


	addi a1, a1, 1
	li t0, 0x81298a63	 #-0x1.5314c600p-125
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff81298a63	 #-0x1.5314c600p-125
	beq t0, a0, L1267
	ret
L1267:


	# Test 1269
	# b32- =0 i -1.000000P-126 -Zero -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1268
	ret
L1268:


	# Test 1270
	# b32- =0 i -0.7FFFFFP-126 -Zero -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1269
	ret
L1269:


	# Test 1271
	# b32- =0 i -0.67419AP-126 -Zero -> -0.67419AP-126 


	addi a1, a1, 1
	li t0, 0x8067419a	 #-0x1.9d066800p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8067419a	 #-0x1.9d066800p-127
	beq t0, a0, L1270
	ret
L1270:


	# Test 1272
	# b32- =0 i -0.000001P-126 -Zero -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1271
	ret
L1271:


	# Test 1273
	# b32- =0 i -1.000000P0 -Zero -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1272
	ret
L1272:


	# Test 1274
	# b32- =0 i -Zero -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1273
	ret
L1273:


	# Test 1275
	# b32- =0 i +Zero -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1274
	ret
L1274:


	# Test 1276
	# b32- =0 i +1.000000P0 -Zero -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1275
	ret
L1275:


	# Test 1277
	# b32- =0 i +0.000001P-126 -Zero -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1276
	ret
L1276:


	# Test 1278
	# b32- =0 i +0.359A22P-126 -Zero -> +0.359A22P-126 


	addi a1, a1, 1
	li t0, 0x359a22	 #0x1.acd11000p-128
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x359a22	 #0x1.acd11000p-128
	beq t0, a0, L1277
	ret
L1277:


	# Test 1279
	# b32- =0 i +0.7FFFFFP-126 -Zero -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1278
	ret
L1278:


	# Test 1280
	# b32- =0 i +1.000000P-126 -Zero -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1279
	ret
L1279:


	# Test 1281
	# b32- =0 i +1.5E3941P-10 -Zero -> +1.5E3941P-10 


	addi a1, a1, 1
	li t0, 0x3ade3941	 #0x1.bc728200p-10
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3ade3941	 #0x1.bc728200p-10
	beq t0, a0, L1280
	ret
L1280:


	# Test 1282
	# b32- =0 i +1.7FFFFFP127 -Zero -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1281
	ret
L1281:


	# Test 1283
	# b32- =0 i +Inf -Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1282
	ret
L1282:


	# Test 1284
	# b32- =0 i -Inf +Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1283
	ret
L1283:


	# Test 1285
	# b32- =0 i -1.7FFFFFP127 +Zero -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1284
	ret
L1284:


	# Test 1286
	# b32- =0 i -1.0DC737P11 +Zero -> -1.0DC737P11 


	addi a1, a1, 1
	li t0, 0xc50dc737	 #-0x1.1b8e6e00p+11
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc50dc737	 #-0x1.1b8e6e00p+11
	beq t0, a0, L1285
	ret
L1285:


	# Test 1287
	# b32- =0 i -1.000000P-126 +Zero -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1286
	ret
L1286:


	# Test 1288
	# b32- =0 i -0.7FFFFFP-126 +Zero -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1287
	ret
L1287:


	# Test 1289
	# b32- =0 i -0.766655P-126 +Zero -> -0.766655P-126 


	addi a1, a1, 1
	li t0, 0x80766655	 #-0x1.d9995400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80766655	 #-0x1.d9995400p-127
	beq t0, a0, L1288
	ret
L1288:


	# Test 1290
	# b32- =0 i -0.000001P-126 +Zero -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1289
	ret
L1289:


	# Test 1291
	# b32- =0 i -1.000000P0 +Zero -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1290
	ret
L1290:


	# Test 1292
	# b32- =0 i -Zero +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1291
	ret
L1291:


	# Test 1293
	# b32- =0 i +Zero +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1292
	ret
L1292:


	# Test 1294
	# b32- =0 i +1.000000P0 +Zero -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1293
	ret
L1293:


	# Test 1295
	# b32- =0 i +0.000001P-126 +Zero -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1294
	ret
L1294:


	# Test 1296
	# b32- =0 i +0.143EDEP-126 +Zero -> +0.143EDEP-126 


	addi a1, a1, 1
	li t0, 0x143ede	 #0x1.43ede000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x143ede	 #0x1.43ede000p-129
	beq t0, a0, L1295
	ret
L1295:


	# Test 1297
	# b32- =0 i +0.7FFFFFP-126 +Zero -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1296
	ret
L1296:


	# Test 1298
	# b32- =0 i +1.000000P-126 +Zero -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1297
	ret
L1297:


	# Test 1299
	# b32- =0 i +1.427614P-116 +Zero -> +1.427614P-116 


	addi a1, a1, 1
	li t0, 0x5c27614	 #0x1.84ec2800p-116
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5c27614	 #0x1.84ec2800p-116
	beq t0, a0, L1298
	ret
L1298:


	# Test 1300
	# b32- =0 i +1.7FFFFFP127 +Zero -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1299
	ret
L1299:


	# Test 1301
	# b32- =0 i +Inf +Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1300
	ret
L1300:


	# Test 1302
	# b32- =0 i -Inf +1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1301
	ret
L1301:


	# Test 1303
	# b32- =0 i -1.7FFFFFP127 +1.000000P0 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1302
	ret
L1302:


	# Test 1304
	# b32- =0 i -1.5C6BF2P57 +1.000000P0 -> -1.5C6BF2P57 x


	addi a1, a1, 1
	li t0, 0xdc5c6bf2	 #-0x1.b8d7e400p+57
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffdc5c6bf2	 #-0x1.b8d7e400p+57
	beq t0, a0, L1303
	ret
L1303:


	# Test 1305
	# b32- =0 i -1.000000P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1304
	ret
L1304:


	# Test 1306
	# b32- =0 i -0.7FFFFFP-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1305
	ret
L1305:


	# Test 1307
	# b32- =0 i -0.5A2329P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x805a2329	 #-0x1.688ca400p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1306
	ret
L1306:


	# Test 1308
	# b32- =0 i -0.000001P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1307
	ret
L1307:


	# Test 1309
	# b32- =0 i -1.000000P0 +1.000000P0 -> -1.000000P1 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0000000	 #-0x1.00000000p+1
	beq t0, a0, L1308
	ret
L1308:


	# Test 1310
	# b32- =0 i -Zero +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1309
	ret
L1309:


	# Test 1311
	# b32- =0 i +Zero +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1310
	ret
L1310:


	# Test 1312
	# b32- =0 i +1.000000P0 +1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1311
	ret
L1311:


	# Test 1313
	# b32- =0 i +0.000001P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1312
	ret
L1312:


	# Test 1314
	# b32- =0 i +0.136399P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x136399	 #0x1.36399000p-129
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1313
	ret
L1313:


	# Test 1315
	# b32- =0 i +0.7FFFFFP-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1314
	ret
L1314:


	# Test 1316
	# b32- =0 i +1.000000P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1315
	ret
L1315:


	# Test 1317
	# b32- =0 i +1.109AD0P-117 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x5109ad0	 #0x1.2135a000p-117
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1316
	ret
L1316:


	# Test 1318
	# b32- =0 i +1.7FFFFFP127 +1.000000P0 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1317
	ret
L1317:


	# Test 1319
	# b32- =0 i +Inf +1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1318
	ret
L1318:


	# Test 1320
	# b32- =0 i -Inf +0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1319
	ret
L1319:


	# Test 1321
	# b32- =0 i -1.7FFFFFP127 +0.000001P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1320
	ret
L1320:


	# Test 1322
	# b32- =0 i -1.2B50ADP104 +0.000001P-126 -> -1.2B50ADP104 x


	addi a1, a1, 1
	li t0, 0xf3ab50ad	 #-0x1.56a15a00p+104
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff3ab50ad	 #-0x1.56a15a00p+104
	beq t0, a0, L1321
	ret
L1321:


	# Test 1323
	# b32- =0 i -1.000000P-126 +0.000001P-126 -> -1.000001P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800001	 #-0x1.00000200p-126
	beq t0, a0, L1322
	ret
L1322:


	# Test 1324
	# b32- =0 i -0.7FFFFFP-126 +0.000001P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1323
	ret
L1323:


	# Test 1325
	# b32- =0 i -0.68C7E4P-126 +0.000001P-126 -> -0.68C7E5P-126 


	addi a1, a1, 1
	li t0, 0x8068c7e4	 #-0x1.a31f9000p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8068c7e5	 #-0x1.a31f9400p-127
	beq t0, a0, L1324
	ret
L1324:


	# Test 1326
	# b32- =0 i -0.000001P-126 +0.000001P-126 -> -0.000002P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000002	 #-0x1.00000000p-148
	beq t0, a0, L1325
	ret
L1325:


	# Test 1327
	# b32- =0 i -1.000000P0 +0.000001P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1326
	ret
L1326:


	# Test 1328
	# b32- =0 i -Zero +0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1327
	ret
L1327:


	# Test 1329
	# b32- =0 i +Zero +0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1328
	ret
L1328:


	# Test 1330
	# b32- =0 i +1.000000P0 +0.000001P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1329
	ret
L1329:


	# Test 1331
	# b32- =0 i +0.000001P-126 +0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1330
	ret
L1330:


	# Test 1332
	# b32- =0 i +0.098854P-126 +0.000001P-126 -> +0.098853P-126 


	addi a1, a1, 1
	li t0, 0x98854	 #0x1.310a8000p-130
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x98853	 #0x1.310a6000p-130
	beq t0, a0, L1331
	ret
L1331:


	# Test 1333
	# b32- =0 i +0.7FFFFFP-126 +0.000001P-126 -> +0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7ffffe	 #0x1.fffff800p-127
	beq t0, a0, L1332
	ret
L1332:


	# Test 1334
	# b32- =0 i +1.000000P-126 +0.000001P-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1333
	ret
L1333:


	# Test 1335
	# b32- =0 i +1.5F3F8BP-111 +0.000001P-126 -> +1.5F3F8BP-111 x


	addi a1, a1, 1
	li t0, 0x85f3f8b	 #0x1.be7f1600p-111
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x85f3f8b	 #0x1.be7f1600p-111
	beq t0, a0, L1334
	ret
L1334:


	# Test 1336
	# b32- =0 i +1.7FFFFFP127 +0.000001P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1335
	ret
L1335:


	# Test 1337
	# b32- =0 i +Inf +0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1336
	ret
L1336:


	# Test 1338
	# b32- =0 i -Inf +0.47A44EP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x47a44e	 #0x1.1e913800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1337
	ret
L1337:


	# Test 1339
	# b32- =0 i -1.7FFFFFP127 +0.26F350P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x26f350	 #0x1.379a8000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1338
	ret
L1338:


	# Test 1340
	# b32- =0 i -1.79F568P-42 +0.1BF664P-126 -> -1.79F568P-42 x


	addi a1, a1, 1
	li t0, 0xaaf9f568	 #-0x1.f3ead000p-42
	fmv.s.x f1, t0
	li t0, 0x1bf664	 #0x1.bf664000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffaaf9f568	 #-0x1.f3ead000p-42
	beq t0, a0, L1339
	ret
L1339:


	# Test 1341
	# b32- =0 i -1.000000P-126 +0.3CB21FP-126 -> -1.3CB21FP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3cb21f	 #0x1.e590f800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80bcb21f	 #-0x1.79643e00p-126
	beq t0, a0, L1340
	ret
L1340:


	# Test 1342
	# b32- =0 i -0.7FFFFFP-126 +0.0B54DBP-126 -> -1.0B54DAP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xb54db	 #0x1.6a9b6000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff808b54da	 #-0x1.16a9b400p-126
	beq t0, a0, L1341
	ret
L1341:


	# Test 1343
	# b32- =0 i -0.1BE46CP-126 +0.133335P-126 -> -0.2F17A1P-126 


	addi a1, a1, 1
	li t0, 0x801be46c	 #-0x1.be46c000p-129
	fmv.s.x f1, t0
	li t0, 0x133335	 #0x1.33335000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff802f17a1	 #-0x1.78bd0800p-128
	beq t0, a0, L1342
	ret
L1342:


	# Test 1344
	# b32- =0 i -0.000001P-126 +0.727FF0P-126 -> -0.727FF1P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x727ff0	 #0x1.c9ffc000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80727ff1	 #-0x1.c9ffc400p-127
	beq t0, a0, L1343
	ret
L1343:


	# Test 1345
	# b32- =0 i -1.000000P0 +0.3D5F58P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3d5f58	 #0x1.eafac000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1344
	ret
L1344:


	# Test 1346
	# b32- =0 i -Zero +0.0BC214P-126 -> -0.0BC214P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbc214	 #0x1.78428000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff800bc214	 #-0x1.78428000p-130
	beq t0, a0, L1345
	ret
L1345:


	# Test 1347
	# b32- =0 i +Zero +0.7EE17BP-126 -> -0.7EE17BP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7ee17b	 #0x1.fb85ec00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ee17b	 #-0x1.fb85ec00p-127
	beq t0, a0, L1346
	ret
L1346:


	# Test 1348
	# b32- =0 i +1.000000P0 +0.0600E3P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x600e3	 #0x1.8038c000p-131
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1347
	ret
L1347:


	# Test 1349
	# b32- =0 i +0.000001P-126 +0.490FE5P-126 -> -0.490FE4P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x490fe5	 #0x1.243f9400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80490fe4	 #-0x1.243f9000p-127
	beq t0, a0, L1348
	ret
L1348:


	# Test 1350
	# b32- =0 i +0.02ED0FP-126 +0.6152AAP-126 -> -0.5E659BP-126 


	addi a1, a1, 1
	li t0, 0x2ed0f	 #0x1.76878000p-132
	fmv.s.x f1, t0
	li t0, 0x6152aa	 #0x1.854aa800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff805e659b	 #-0x1.79966c00p-127
	beq t0, a0, L1349
	ret
L1349:


	# Test 1351
	# b32- =0 i +0.7FFFFFP-126 +0.340B5FP-126 -> +0.4BF4A0P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x340b5f	 #0x1.a05af800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4bf4a0	 #0x1.2fd28000p-127
	beq t0, a0, L1350
	ret
L1350:


	# Test 1352
	# b32- =0 i +1.000000P-126 +0.7F6AC7P-126 -> +0.009539P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f6ac7	 #0x1.fdab1c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x9539	 #0x1.2a720000p-134
	beq t0, a0, L1351
	ret
L1351:


	# Test 1353
	# b32- =0 i +1.421C13P21 +0.78CF7BP-126 -> +1.421C13P21 x


	addi a1, a1, 1
	li t0, 0x4a421c13	 #0x1.84382600p+21
	fmv.s.x f1, t0
	li t0, 0x78cf7b	 #0x1.e33dec00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4a421c13	 #0x1.84382600p+21
	beq t0, a0, L1352
	ret
L1352:


	# Test 1354
	# b32- =0 i +1.7FFFFFP127 +0.49D931P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x49d931	 #0x1.2764c400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1353
	ret
L1353:


	# Test 1355
	# b32- =0 i +Inf +0.34B898P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x34b898	 #0x1.a5c4c000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1354
	ret
L1354:


	# Test 1356
	# b32- =0 i -Inf +0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1355
	ret
L1355:


	# Test 1357
	# b32- =0 i -1.7FFFFFP127 +0.7FFFFFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1356
	ret
L1356:


	# Test 1358
	# b32- =0 i -1.489A24P-59 +0.7FFFFFP-126 -> -1.489A24P-59 x


	addi a1, a1, 1
	li t0, 0xa2489a24	 #-0x1.91344800p-59
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa2489a24	 #-0x1.91344800p-59
	beq t0, a0, L1357
	ret
L1357:


	# Test 1359
	# b32- =0 i -1.000000P-126 +0.7FFFFFP-126 -> -1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80ffffff	 #-0x1.fffffe00p-126
	beq t0, a0, L1358
	ret
L1358:


	# Test 1360
	# b32- =0 i -0.7FFFFFP-126 +0.7FFFFFP-126 -> -1.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80fffffe	 #-0x1.fffffc00p-126
	beq t0, a0, L1359
	ret
L1359:


	# Test 1361
	# b32- =0 i -0.0A0927P-126 +0.7FFFFFP-126 -> -1.0A0926P-126 


	addi a1, a1, 1
	li t0, 0x800a0927	 #-0x1.4124e000p-130
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff808a0926	 #-0x1.14124c00p-126
	beq t0, a0, L1360
	ret
L1360:


	# Test 1362
	# b32- =0 i -0.000001P-126 +0.7FFFFFP-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1361
	ret
L1361:


	# Test 1363
	# b32- =0 i -1.000000P0 +0.7FFFFFP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1362
	ret
L1362:


	# Test 1364
	# b32- =0 i -Zero +0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1363
	ret
L1363:


	# Test 1365
	# b32- =0 i +Zero +0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1364
	ret
L1364:


	# Test 1366
	# b32- =0 i +1.000000P0 +0.7FFFFFP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1365
	ret
L1365:


	# Test 1367
	# b32- =0 i +0.000001P-126 +0.7FFFFFP-126 -> -0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ffffe	 #-0x1.fffff800p-127
	beq t0, a0, L1366
	ret
L1366:


	# Test 1368
	# b32- =0 i +0.0C29E3P-126 +0.7FFFFFP-126 -> -0.73D61CP-126 


	addi a1, a1, 1
	li t0, 0xc29e3	 #0x1.853c6000p-130
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8073d61c	 #-0x1.cf587000p-127
	beq t0, a0, L1367
	ret
L1367:


	# Test 1369
	# b32- =0 i +0.7FFFFFP-126 +0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1368
	ret
L1368:


	# Test 1370
	# b32- =0 i +1.000000P-126 +0.7FFFFFP-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1369
	ret
L1369:


	# Test 1371
	# b32- =0 i +1.2618E7P3 +0.7FFFFFP-126 -> +1.2618E7P3 x


	addi a1, a1, 1
	li t0, 0x412618e7	 #0x1.4c31ce00p+3
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x412618e7	 #0x1.4c31ce00p+3
	beq t0, a0, L1370
	ret
L1370:


	# Test 1372
	# b32- =0 i +1.7FFFFFP127 +0.7FFFFFP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1371
	ret
L1371:


	# Test 1373
	# b32- =0 i +Inf +0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1372
	ret
L1372:


	# Test 1374
	# b32- =0 i -Inf +1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1373
	ret
L1373:


	# Test 1375
	# b32- =0 i -1.7FFFFFP127 +1.000000P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1374
	ret
L1374:


	# Test 1376
	# b32- =0 i -1.2CD6F7P-117 +1.000000P-126 -> -1.2D16F7P-117 


	addi a1, a1, 1
	li t0, 0x852cd6f7	 #-0x1.59adee00p-117
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff852d16f7	 #-0x1.5a2dee00p-117
	beq t0, a0, L1375
	ret
L1375:


	# Test 1377
	# b32- =0 i -1.000000P-126 +1.000000P-126 -> -1.000000P-125 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff81000000	 #-0x1.00000000p-125
	beq t0, a0, L1376
	ret
L1376:


	# Test 1378
	# b32- =0 i -0.7FFFFFP-126 +1.000000P-126 -> -1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80ffffff	 #-0x1.fffffe00p-126
	beq t0, a0, L1377
	ret
L1377:


	# Test 1379
	# b32- =0 i -0.006DE3P-126 +1.000000P-126 -> -1.006DE3P-126 


	addi a1, a1, 1
	li t0, 0x80006de3	 #-0x1.b78c0000p-135
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80806de3	 #-0x1.00dbc600p-126
	beq t0, a0, L1378
	ret
L1378:


	# Test 1380
	# b32- =0 i -0.000001P-126 +1.000000P-126 -> -1.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800001	 #-0x1.00000200p-126
	beq t0, a0, L1379
	ret
L1379:


	# Test 1381
	# b32- =0 i -1.000000P0 +1.000000P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1380
	ret
L1380:


	# Test 1382
	# b32- =0 i -Zero +1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1381
	ret
L1381:


	# Test 1383
	# b32- =0 i +Zero +1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1382
	ret
L1382:


	# Test 1384
	# b32- =0 i +1.000000P0 +1.000000P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1383
	ret
L1383:


	# Test 1385
	# b32- =0 i +0.000001P-126 +1.000000P-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1384
	ret
L1384:


	# Test 1386
	# b32- =0 i +0.00CE9EP-126 +1.000000P-126 -> -0.7F3162P-126 


	addi a1, a1, 1
	li t0, 0xce9e	 #0x1.9d3c0000p-134
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807f3162	 #-0x1.fcc58800p-127
	beq t0, a0, L1385
	ret
L1385:


	# Test 1387
	# b32- =0 i +0.7FFFFFP-126 +1.000000P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1386
	ret
L1386:


	# Test 1388
	# b32- =0 i +1.000000P-126 +1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1387
	ret
L1387:


	# Test 1389
	# b32- =0 i +1.74BDA2P49 +1.000000P-126 -> +1.74BDA2P49 x


	addi a1, a1, 1
	li t0, 0x5874bda2	 #0x1.e97b4400p+49
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5874bda2	 #0x1.e97b4400p+49
	beq t0, a0, L1388
	ret
L1388:


	# Test 1390
	# b32- =0 i +1.7FFFFFP127 +1.000000P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1389
	ret
L1389:


	# Test 1391
	# b32- =0 i +Inf +1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1390
	ret
L1390:


	# Test 1392
	# b32- =0 i -Inf +1.14A73AP-103 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xc14a73a	 #0x1.294e7400p-103
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1391
	ret
L1391:


	# Test 1393
	# b32- =0 i -1.7FFFFFP127 +1.1F86A1P-82 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x169f86a1	 #0x1.3f0d4200p-82
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1392
	ret
L1392:


	# Test 1394
	# b32- =0 i -1.7B7BB3P-119 +1.35FF70P90 -> -1.35FF70P90 x


	addi a1, a1, 1
	li t0, 0x847b7bb3	 #-0x1.f6f76600p-119
	fmv.s.x f1, t0
	li t0, 0x6cb5ff70	 #0x1.6bfee000p+90
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffecb5ff70	 #-0x1.6bfee000p+90
	beq t0, a0, L1393
	ret
L1393:


	# Test 1395
	# b32- =0 i -1.000000P-126 +1.69F50BP-35 -> -1.69F50BP-35 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x2e69f50b	 #0x1.d3ea1600p-35
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffae69f50b	 #-0x1.d3ea1600p-35
	beq t0, a0, L1394
	ret
L1394:


	# Test 1396
	# b32- =0 i -0.7FFFFFP-126 +1.74D473P-30 -> -1.74D473P-30 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x30f4d473	 #0x1.e9a8e600p-30
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb0f4d473	 #-0x1.e9a8e600p-30
	beq t0, a0, L1395
	ret
L1395:


	# Test 1397
	# b32- =0 i -0.04AAB6P-126 +1.4DFC40P20 -> -1.4DFC40P20 x


	addi a1, a1, 1
	li t0, 0x8004aab6	 #-0x1.2aad8000p-131
	fmv.s.x f1, t0
	li t0, 0x49cdfc40	 #0x1.9bf88000p+20
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc9cdfc40	 #-0x1.9bf88000p+20
	beq t0, a0, L1396
	ret
L1396:


	# Test 1398
	# b32- =0 i -0.000001P-126 +1.76D696P14 -> -1.76D696P14 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x46f6d696	 #0x1.edad2c00p+14
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc6f6d696	 #-0x1.edad2c00p+14
	beq t0, a0, L1397
	ret
L1397:


	# Test 1399
	# b32- =0 i -1.000000P0 +1.5F1EEFP-75 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1a5f1eef	 #0x1.be3dde00p-75
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1398
	ret
L1398:


	# Test 1400
	# b32- =0 i -Zero +1.6A7E57P-38 -> -1.6A7E57P-38 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x2cea7e57	 #0x1.d4fcae00p-38
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffacea7e57	 #-0x1.d4fcae00p-38
	beq t0, a0, L1399
	ret
L1399:


	# Test 1401
	# b32- =0 i +Zero +1.60A113P113 -> -1.60A113P113 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7860a113	 #0x1.c1422600p+113
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff860a113	 #-0x1.c1422600p+113
	beq t0, a0, L1400
	ret
L1400:


	# Test 1402
	# b32- =0 i +1.000000P0 +1.6B807AP6 -> -1.69807AP6 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x42eb807a	 #0x1.d700f400p+6
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc2e9807a	 #-0x1.d300f400p+6
	beq t0, a0, L1401
	ret
L1401:


	# Test 1403
	# b32- =0 i +0.000001P-126 +1.76DFE2P11 -> -1.76DFE2P11 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x4576dfe2	 #0x1.edbfc400p+11
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc576dfe2	 #-0x1.edbfc400p+11
	beq t0, a0, L1402
	ret
L1402:


	# Test 1404
	# b32- =0 i +0.71F35AP-126 +1.4B5BB6P86 -> -1.4B5BB6P86 x


	addi a1, a1, 1
	li t0, 0x71f35a	 #0x1.c7cd6800p-127
	fmv.s.x f1, t0
	li t0, 0x6acb5bb6	 #0x1.96b76c00p+86
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffeacb5bb6	 #-0x1.96b76c00p+86
	beq t0, a0, L1403
	ret
L1403:


	# Test 1405
	# b32- =0 i +0.7FFFFFP-126 +1.414E4BP27 -> -1.414E4BP27 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x4d414e4b	 #0x1.829c9600p+27
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcd414e4b	 #-0x1.829c9600p+27
	beq t0, a0, L1404
	ret
L1404:


	# Test 1406
	# b32- =0 i +1.000000P-126 +1.4C6DB3P32 -> -1.4C6DB3P32 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x4fcc6db3	 #0x1.98db6600p+32
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcfcc6db3	 #-0x1.98db6600p+32
	beq t0, a0, L1405
	ret
L1405:


	# Test 1407
	# b32- =0 i +1.2E4A45P-31 +1.0EEFC8P94 -> -1.0EEFC8P94 x


	addi a1, a1, 1
	li t0, 0x302e4a45	 #0x1.5c948a00p-31
	fmv.s.x f1, t0
	li t0, 0x6e8eefc8	 #0x1.1ddf9000p+94
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffee8eefc8	 #-0x1.1ddf9000p+94
	beq t0, a0, L1406
	ret
L1406:


	# Test 1408
	# b32- =0 i +1.7FFFFFP127 +1.4DAFD6P-52 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x25cdafd6	 #0x1.9b5fac00p-52
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1407
	ret
L1407:


	# Test 1409
	# b32- =0 i +Inf +1.213B84P-75 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1a213b84	 #0x1.42770800p-75
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1408
	ret
L1408:


	# Test 1410
	# b32- =0 i -Inf +1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1409
	ret
L1409:


	# Test 1411
	# b32- =0 i -1.7FFFFFP127 +1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1410
	ret
L1410:


	# Test 1412
	# b32- =0 i -1.5D983BP85 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xea5d983b	 #-0x1.bb307600p+85
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1411
	ret
L1411:


	# Test 1413
	# b32- =0 i -1.000000P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1412
	ret
L1412:


	# Test 1414
	# b32- =0 i -0.7FFFFFP-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1413
	ret
L1413:


	# Test 1415
	# b32- =0 i -0.5BCF72P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x805bcf72	 #-0x1.6f3dc800p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1414
	ret
L1414:


	# Test 1416
	# b32- =0 i -0.000001P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1415
	ret
L1415:


	# Test 1417
	# b32- =0 i -1.000000P0 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1416
	ret
L1416:


	# Test 1418
	# b32- =0 i -Zero +1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1417
	ret
L1417:


	# Test 1419
	# b32- =0 i +Zero +1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1418
	ret
L1418:


	# Test 1420
	# b32- =0 i +1.000000P0 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1419
	ret
L1419:


	# Test 1421
	# b32- =0 i +0.000001P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1420
	ret
L1420:


	# Test 1422
	# b32- =0 i +0.409815P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x409815	 #0x1.02605400p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1421
	ret
L1421:


	# Test 1423
	# b32- =0 i +0.7FFFFFP-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1422
	ret
L1422:


	# Test 1424
	# b32- =0 i +1.000000P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1423
	ret
L1423:


	# Test 1425
	# b32- =0 i +1.7D6F00P-49 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x277d6f00	 #0x1.fade0000p-49
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1424
	ret
L1424:


	# Test 1426
	# b32- =0 i +1.7FFFFFP127 +1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1425
	ret
L1425:


	# Test 1427
	# b32- =0 i +Inf +1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1426
	ret
L1426:


	# Test 1428
	# b32- =0 i -Inf +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1427
	ret
L1427:


	# Test 1429
	# b32- =0 i -1.7FFFFFP127 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1428
	ret
L1428:


	# Test 1430
	# b32- =0 i -1.2CBCF6P68 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xe1acbcf6	 #-0x1.5979ec00p+68
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1429
	ret
L1429:


	# Test 1431
	# b32- =0 i -1.000000P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1430
	ret
L1430:


	# Test 1432
	# b32- =0 i -0.7FFFFFP-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1431
	ret
L1431:


	# Test 1433
	# b32- =0 i -0.555C14P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80555c14	 #-0x1.55705000p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1432
	ret
L1432:


	# Test 1434
	# b32- =0 i -0.000001P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1433
	ret
L1433:


	# Test 1435
	# b32- =0 i -1.000000P0 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1434
	ret
L1434:


	# Test 1436
	# b32- =0 i -Zero +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1435
	ret
L1435:


	# Test 1437
	# b32- =0 i +Zero +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1436
	ret
L1436:


	# Test 1438
	# b32- =0 i +1.000000P0 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1437
	ret
L1437:


	# Test 1439
	# b32- =0 i +0.000001P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1438
	ret
L1438:


	# Test 1440
	# b32- =0 i +0.13349DP-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x13349d	 #0x1.3349d000p-129
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1439
	ret
L1439:


	# Test 1441
	# b32- =0 i +0.7FFFFFP-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1440
	ret
L1440:


	# Test 1442
	# b32- =0 i +1.000000P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1441
	ret
L1441:


	# Test 1443
	# b32- =0 i +1.4BD3BBP-66 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x1ecbd3bb	 #0x1.97a77600p-66
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1442
	ret
L1442:


	# Test 1444
	# b32- =0 i +1.7FFFFFP127 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1443
	ret
L1443:


	# Test 1445
	# b32- =0 -Inf -Inf -> q i


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1444
	ret
L1444:


	# Test 1446
	# b32- =0 -1.7FFFFFP127 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1445
	ret
L1445:


	# Test 1447
	# b32- =0 -1.2AE367P89 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xec2ae367	 #-0x1.55c6ce00p+89
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1446
	ret
L1446:


	# Test 1448
	# b32- =0 -1.000000P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1447
	ret
L1447:


	# Test 1449
	# b32- =0 -0.7FFFFFP-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1448
	ret
L1448:


	# Test 1450
	# b32- =0 -0.1C526BP-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x801c526b	 #-0x1.c526b000p-129
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1449
	ret
L1449:


	# Test 1451
	# b32- =0 -0.000001P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1450
	ret
L1450:


	# Test 1452
	# b32- =0 -1.000000P0 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1451
	ret
L1451:


	# Test 1453
	# b32- =0 -Zero -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1452
	ret
L1452:


	# Test 1454
	# b32- =0 +Zero -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1453
	ret
L1453:


	# Test 1455
	# b32- =0 +1.000000P0 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1454
	ret
L1454:


	# Test 1456
	# b32- =0 +0.000001P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1455
	ret
L1455:


	# Test 1457
	# b32- =0 +0.055B0EP-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x55b0e	 #0x1.56c38000p-131
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1456
	ret
L1456:


	# Test 1458
	# b32- =0 +0.7FFFFFP-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1457
	ret
L1457:


	# Test 1459
	# b32- =0 +1.000000P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1458
	ret
L1458:


	# Test 1460
	# b32- =0 +1.72CA12P-41 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x2b72ca12	 #0x1.e5942400p-41
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1459
	ret
L1459:


	# Test 1461
	# b32- =0 +1.7FFFFFP127 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1460
	ret
L1460:


	# Test 1462
	# b32- =0 +Inf -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1461
	ret
L1461:


	# Test 1463
	# b32- =0 q -Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1462
	ret
L1462:


	# Test 1464
	# b32- =0 q -Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1463
	ret
L1463:


	# Test 1465
	# b32- =0 -Inf -1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1464
	ret
L1464:


	# Test 1466
	# b32- =0 -1.7FFFFFP127 -1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1465
	ret
L1465:


	# Test 1467
	# b32- =0 -1.0D7FEFP-83 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x960d7fef	 #-0x1.1affde00p-83
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1466
	ret
L1466:


	# Test 1468
	# b32- =0 -1.000000P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1467
	ret
L1467:


	# Test 1469
	# b32- =0 -0.7FFFFFP-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1468
	ret
L1468:


	# Test 1470
	# b32- =0 -0.0AF726P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800af726	 #-0x1.5ee4c000p-130
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1469
	ret
L1469:


	# Test 1471
	# b32- =0 -0.000001P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1470
	ret
L1470:


	# Test 1472
	# b32- =0 -1.000000P0 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1471
	ret
L1471:


	# Test 1473
	# b32- =0 -Zero -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1472
	ret
L1472:


	# Test 1474
	# b32- =0 +Zero -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1473
	ret
L1473:


	# Test 1475
	# b32- =0 +1.000000P0 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1474
	ret
L1474:


	# Test 1476
	# b32- =0 +0.000001P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1475
	ret
L1475:


	# Test 1477
	# b32- =0 +0.6FBFC9P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x6fbfc9	 #0x1.beff2400p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1476
	ret
L1476:


	# Test 1478
	# b32- =0 +0.7FFFFFP-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1477
	ret
L1477:


	# Test 1479
	# b32- =0 +1.000000P-126 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1478
	ret
L1478:


	# Test 1480
	# b32- =0 +1.41EECDP-58 -1.7FFFFFP127 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x22c1eecd	 #0x1.83dd9a00p-58
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1479
	ret
L1479:


	# Test 1481
	# b32- =0 +1.7FFFFFP127 -1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1480
	ret
L1480:


	# Test 1482
	# b32- =0 +Inf -1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1481
	ret
L1481:


	# Test 1483
	# b32- =0 q -1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1482
	ret
L1482:


	# Test 1484
	# b32- =0 q -1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1483
	ret
L1483:


	# Test 1485
	# b32- =0 -Inf -1.652D92P105 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xf4652d92	 #-0x1.ca5b2400p+105
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1484
	ret
L1484:


	# Test 1486
	# b32- =0 -1.7FFFFFP127 -1.708CF9P-124 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x81f08cf9	 #-0x1.e119f200p-124
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1485
	ret
L1485:


	# Test 1487
	# b32- =0 -1.5BA4ABP-69 -1.7F28E4P90 -> +1.7F28E4P90 x


	addi a1, a1, 1
	li t0, 0x9d5ba4ab	 #-0x1.b7495600p-69
	fmv.s.x f1, t0
	li t0, 0xecff28e4	 #-0x1.fe51c800p+90
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6cff28e4	 #0x1.fe51c800p+90
	beq t0, a0, L1486
	ret
L1486:


	# Test 1488
	# b32- =0 -1.000000P-126 -1.4F780FP92 -> +1.4F780FP92 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xedcf780f	 #-0x1.9ef01e00p+92
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6dcf780f	 #0x1.9ef01e00p+92
	beq t0, a0, L1487
	ret
L1487:


	# Test 1489
	# b32- =0 -0.7FFFFFP-126 -1.45DACBP3 -> +1.45DACBP3 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xc145dacb	 #-0x1.8bb59600p+3
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4145dacb	 #0x1.8bb59600p+3
	beq t0, a0, L1488
	ret
L1488:


	# Test 1490
	# b32- =0 -0.03DBE1P-126 -1.077EFBP-12 -> +1.077EFBP-12 x


	addi a1, a1, 1
	li t0, 0x8003dbe1	 #-0x1.edf08000p-132
	fmv.s.x f1, t0
	li t0, 0xb9877efb	 #-0x1.0efdf600p-12
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x39877efb	 #0x1.0efdf600p-12
	beq t0, a0, L1489
	ret
L1489:


	# Test 1491
	# b32- =0 -0.000001P-126 -1.5C5999P13 -> +1.5C5999P13 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xc65c5999	 #-0x1.b8b33200p+13
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x465c5999	 #0x1.b8b33200p+13
	beq t0, a0, L1490
	ret
L1490:


	# Test 1492
	# b32- =0 -1.000000P0 -1.52FC55P52 -> +1.52FC55P52 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xd9d2fc55	 #-0x1.a5f8aa00p+52
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x59d2fc55	 #0x1.a5f8aa00p+52
	beq t0, a0, L1491
	ret
L1491:


	# Test 1493
	# b32- =0 -Zero -1.264803P-67 -> +1.264803P-67 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x9e264803	 #-0x1.4c900600p-67
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1e264803	 #0x1.4c900600p-67
	beq t0, a0, L1492
	ret
L1492:


	# Test 1494
	# b32- =0 +Zero -1.31276BP-30 -> +1.31276BP-30 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xb0b1276b	 #-0x1.624ed600p-30
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x30b1276b	 #0x1.624ed600p-30
	beq t0, a0, L1493
	ret
L1493:


	# Test 1495
	# b32- =0 +1.000000P0 -1.3C86D2P-25 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xb33c86d2	 #-0x1.790da400p-25
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1494
	ret
L1494:


	# Test 1496
	# b32- =0 +0.000001P-126 -1.33298EP-50 -> +1.33298EP-50 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xa6b3298e	 #-0x1.66531c00p-50
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x26b3298e	 #0x1.66531c00p-50
	beq t0, a0, L1495
	ret
L1495:


	# Test 1497
	# b32- =0 +0.27B46AP-126 -1.68ADE8P-55 -> +1.68ADE8P-55 x


	addi a1, a1, 1
	li t0, 0x27b46a	 #0x1.3da35000p-128
	fmv.s.x f1, t0
	li t0, 0xa468ade8	 #-0x1.d15bd000p-55
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2468ade8	 #0x1.d15bd000p-55
	beq t0, a0, L1496
	ret
L1496:


	# Test 1498
	# b32- =0 +0.7FFFFFP-126 -1.1194A3P-4 -> +1.1194A3P-4 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbd9194a3	 #-0x1.23294600p-4
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3d9194a3	 #0x1.23294600p-4
	beq t0, a0, L1497
	ret
L1497:


	# Test 1499
	# b32- =0 +1.000000P-126 -1.1D340BP-111 -> +1.1D350BP-111 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x881d340b	 #-0x1.3a681600p-111
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x81d350b	 #0x1.3a6a1600p-111
	beq t0, a0, L1498
	ret
L1498:


	# Test 1500
	# b32- =0 +1.101388P85 -1.1C9B41P-80 -> +1.101388P85 x


	addi a1, a1, 1
	li t0, 0x6a101388	 #0x1.20271000p+85
	fmv.s.x f1, t0
	li t0, 0x979c9b41	 #-0x1.39368200p-80
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6a101388	 #0x1.20271000p+85
	beq t0, a0, L1499
	ret
L1499:


	# Test 1501
	# b32- =0 +1.7FFFFFP127 -1.3332DAP-101 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x8d3332da	 #-0x1.6665b400p-101
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1500
	ret
L1500:


	# Test 1502
	# b32- =0 +Inf -1.077E87P-116 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x85877e87	 #-0x1.0efd0e00p-116
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1501
	ret
L1501:


	# Test 1503
	# b32- =0 q -1.257158P-55 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xa4257158	 #-0x1.4ae2b000p-55
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1502
	ret
L1502:


	# Test 1504
	# b32- =0 q -1.664D5EP-85 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x95664d5e	 #-0x1.cc9abc00p-85
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1503
	ret
L1503:


	# Test 1505
	# b32- =0 -Inf -1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1504
	ret
L1504:


	# Test 1506
	# b32- =0 -1.7FFFFFP127 -1.000000P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1505
	ret
L1505:


	# Test 1507
	# b32- =0 -1.2AC966P-86 -1.000000P-126 -> -1.2AC966P-86 x


	addi a1, a1, 1
	li t0, 0x94aac966	 #-0x1.5592cc00p-86
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff94aac966	 #-0x1.5592cc00p-86
	beq t0, a0, L1506
	ret
L1506:


	# Test 1508
	# b32- =0 -1.000000P-126 -1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1507
	ret
L1507:


	# Test 1509
	# b32- =0 -0.7FFFFFP-126 -1.000000P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1508
	ret
L1508:


	# Test 1510
	# b32- =0 -0.68009DP-126 -1.000000P-126 -> +0.17FF63P-126 


	addi a1, a1, 1
	li t0, 0x8068009d	 #-0x1.a0027400p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x17ff63	 #0x1.7ff63000p-129
	beq t0, a0, L1509
	ret
L1509:


	# Test 1511
	# b32- =0 -0.000001P-126 -1.000000P-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1510
	ret
L1510:


	# Test 1512
	# b32- =0 -1.000000P0 -1.000000P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1511
	ret
L1511:


	# Test 1513
	# b32- =0 -Zero -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1512
	ret
L1512:


	# Test 1514
	# b32- =0 +Zero -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1513
	ret
L1513:


	# Test 1515
	# b32- =0 +1.000000P0 -1.000000P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1514
	ret
L1514:


	# Test 1516
	# b32- =0 +0.000001P-126 -1.000000P-126 -> +1.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800001	 #0x1.00000200p-126
	beq t0, a0, L1515
	ret
L1515:


	# Test 1517
	# b32- =0 +0.36D925P-126 -1.000000P-126 -> +1.36D925P-126 


	addi a1, a1, 1
	li t0, 0x36d925	 #0x1.b6c92800p-128
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xb6d925	 #0x1.6db24a00p-126
	beq t0, a0, L1516
	ret
L1516:


	# Test 1518
	# b32- =0 +0.7FFFFFP-126 -1.000000P-126 -> +1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffff	 #0x1.fffffe00p-126
	beq t0, a0, L1517
	ret
L1517:


	# Test 1519
	# b32- =0 +1.000000P-126 -1.000000P-126 -> +1.000000P-125 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1000000	 #0x1.00000000p-125
	beq t0, a0, L1518
	ret
L1518:


	# Test 1520
	# b32- =0 +1.74105CP-94 -1.000000P-126 -> +1.74105CP-94 x


	addi a1, a1, 1
	li t0, 0x10f4105c	 #0x1.e820b800p-94
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x10f4105c	 #0x1.e820b800p-94
	beq t0, a0, L1519
	ret
L1519:


	# Test 1521
	# b32- =0 +1.7FFFFFP127 -1.000000P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1520
	ret
L1520:


	# Test 1522
	# b32- =0 +Inf -1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1521
	ret
L1521:


	# Test 1523
	# b32- =0 q -1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1522
	ret
L1522:


	# Test 1524
	# b32- =0 q -1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1523
	ret
L1523:


	# Test 1525
	# b32- =0 -Inf -0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1524
	ret
L1524:


	# Test 1526
	# b32- =0 -1.7FFFFFP127 -0.7FFFFFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1525
	ret
L1525:


	# Test 1527
	# b32- =0 -1.0E863AP-8 -0.7FFFFFP-126 -> -1.0E863AP-8 x


	addi a1, a1, 1
	li t0, 0xbb8e863a	 #-0x1.1d0c7400p-8
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbb8e863a	 #-0x1.1d0c7400p-8
	beq t0, a0, L1526
	ret
L1526:


	# Test 1528
	# b32- =0 -1.000000P-126 -0.7FFFFFP-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1527
	ret
L1527:


	# Test 1529
	# b32- =0 -0.7FFFFFP-126 -0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1528
	ret
L1528:


	# Test 1530
	# b32- =0 -0.00BD70P-126 -0.7FFFFFP-126 -> +0.7F428FP-126 


	addi a1, a1, 1
	li t0, 0x8000bd70	 #-0x1.7ae00000p-134
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f428f	 #0x1.fd0a3c00p-127
	beq t0, a0, L1529
	ret
L1529:


	# Test 1531
	# b32- =0 -0.000001P-126 -0.7FFFFFP-126 -> +0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7ffffe	 #0x1.fffff800p-127
	beq t0, a0, L1530
	ret
L1530:


	# Test 1532
	# b32- =0 -1.000000P0 -0.7FFFFFP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1531
	ret
L1531:


	# Test 1533
	# b32- =0 -Zero -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1532
	ret
L1532:


	# Test 1534
	# b32- =0 +Zero -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1533
	ret
L1533:


	# Test 1535
	# b32- =0 +1.000000P0 -0.7FFFFFP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1534
	ret
L1534:


	# Test 1536
	# b32- =0 +0.000001P-126 -0.7FFFFFP-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1535
	ret
L1535:


	# Test 1537
	# b32- =0 +0.257DE1P-126 -0.7FFFFFP-126 -> +1.257DE0P-126 


	addi a1, a1, 1
	li t0, 0x257de1	 #0x1.2bef0800p-128
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xa57de0	 #0x1.4afbc000p-126
	beq t0, a0, L1536
	ret
L1536:


	# Test 1538
	# b32- =0 +0.7FFFFFP-126 -0.7FFFFFP-126 -> +1.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffe	 #0x1.fffffc00p-126
	beq t0, a0, L1537
	ret
L1537:


	# Test 1539
	# b32- =0 +1.000000P-126 -0.7FFFFFP-126 -> +1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffff	 #0x1.fffffe00p-126
	beq t0, a0, L1538
	ret
L1538:


	# Test 1540
	# b32- =0 +1.42B517P17 -0.7FFFFFP-126 -> +1.42B517P17 x


	addi a1, a1, 1
	li t0, 0x4842b517	 #0x1.856a2e00p+17
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4842b517	 #0x1.856a2e00p+17
	beq t0, a0, L1539
	ret
L1539:


	# Test 1541
	# b32- =0 +1.7FFFFFP127 -0.7FFFFFP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1540
	ret
L1540:


	# Test 1542
	# b32- =0 +Inf -0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1541
	ret
L1541:


	# Test 1543
	# b32- =0 q -0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1542
	ret
L1542:


	# Test 1544
	# b32- =0 q -0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1543
	ret
L1543:


	# Test 1545
	# b32- =0 -Inf -0.476D2AP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80476d2a	 #-0x1.1db4a800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1544
	ret
L1544:


	# Test 1546
	# b32- =0 -1.7FFFFFP127 -0.1D4FE5P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x801d4fe5	 #-0x1.d4fe5000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1545
	ret
L1545:


	# Test 1547
	# b32- =0 -1.5D2AF5P-26 -0.1931EFP-126 -> -1.5D2AF5P-26 x


	addi a1, a1, 1
	li t0, 0xb2dd2af5	 #-0x1.ba55ea00p-26
	fmv.s.x f1, t0
	li t0, 0x801931ef	 #-0x1.931ef000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb2dd2af5	 #-0x1.ba55ea00p-26
	beq t0, a0, L1546
	ret
L1546:


	# Test 1548
	# b32- =0 -1.000000P-126 -0.13CEB4P-126 -> -0.6C314CP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x8013ceb4	 #-0x1.3ceb4000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff806c314c	 #-0x1.b0c53000p-127
	beq t0, a0, L1547
	ret
L1547:


	# Test 1549
	# b32- =0 -0.7FFFFFP-126 -0.52DDB7P-126 -> -0.2D2248P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x8052ddb7	 #-0x1.4b76dc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff802d2248	 #-0x1.69124000p-128
	beq t0, a0, L1548
	ret
L1548:


	# Test 1550
	# b32- =0 -0.5B622CP-126 -0.118806P-126 -> -0.49DA26P-126 


	addi a1, a1, 1
	li t0, 0x805b622c	 #-0x1.6d88b000p-127
	fmv.s.x f1, t0
	li t0, 0x80118806	 #-0x1.18806000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8049da26	 #-0x1.27689800p-127
	beq t0, a0, L1549
	ret
L1549:


	# Test 1551
	# b32- =0 -0.000001P-126 -0.691C86P-126 -> +0.691C85P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80691c86	 #-0x1.a4721800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x691c85	 #0x1.a4721400p-127
	beq t0, a0, L1550
	ret
L1550:


	# Test 1552
	# b32- =0 -1.000000P0 -0.143BEDP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80143bed	 #-0x1.43bed000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1551
	ret
L1551:


	# Test 1553
	# b32- =0 -Zero -0.6A9EA9P-126 -> +0.6A9EA9P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x806a9ea9	 #-0x1.aa7aa400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6a9ea9	 #0x1.aa7aa400p-127
	beq t0, a0, L1552
	ret
L1552:


	# Test 1554
	# b32- =0 +Zero -0.032702P-126 -> +0.032702P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80032702	 #-0x1.93810000p-132
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x32702	 #0x1.93810000p-132
	beq t0, a0, L1553
	ret
L1553:


	# Test 1555
	# b32- =0 +1.000000P0 -0.5E466AP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x805e466a	 #-0x1.7919a800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1554
	ret
L1554:


	# Test 1556
	# b32- =0 +0.000001P-126 -0.14A926P-126 -> +0.14A927P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x8014a926	 #-0x1.4a926000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x14a927	 #0x1.4a927000p-129
	beq t0, a0, L1555
	ret
L1555:


	# Test 1557
	# b32- =0 +0.13A29CP-126 -0.3E8E36P-126 -> +0.5230D2P-126 


	addi a1, a1, 1
	li t0, 0x13a29c	 #0x1.3a29c000p-129
	fmv.s.x f1, t0
	li t0, 0x803e8e36	 #-0x1.f471b000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5230d2	 #0x1.48c34800p-127
	beq t0, a0, L1556
	ret
L1556:


	# Test 1558
	# b32- =0 +0.7FFFFFP-126 -0.1AA7F4P-126 -> +1.1AA7F3P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x801aa7f4	 #-0x1.aa7f4000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x9aa7f3	 #0x1.354fe600p-126
	beq t0, a0, L1557
	ret
L1557:


	# Test 1559
	# b32- =0 +1.000000P-126 -0.3EF3A2P-126 -> +1.3EF3A2P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x803ef3a2	 #-0x1.f79d1000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xbef3a2	 #0x1.7de74400p-126
	beq t0, a0, L1558
	ret
L1558:


	# Test 1560
	# b32- =0 +1.11D9D3P64 -0.26E44DP-126 -> +1.11D9D3P64 x


	addi a1, a1, 1
	li t0, 0x5f91d9d3	 #0x1.23b3a600p+64
	fmv.s.x f1, t0
	li t0, 0x8026e44d	 #-0x1.37226800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5f91d9d3	 #0x1.23b3a600p+64
	beq t0, a0, L1559
	ret
L1559:


	# Test 1561
	# b32- =0 +1.7FFFFFP127 -0.4075C6P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x804075c6	 #-0x1.01d71800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1560
	ret
L1560:


	# Test 1562
	# b32- =0 +Inf -0.0B152DP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800b152d	 #-0x1.62a5a000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1561
	ret
L1561:


	# Test 1563
	# b32- =0 q -0.3EA11EP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x803ea11e	 #-0x1.f508f000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1562
	ret
L1562:


	# Test 1564
	# b32- =0 q -0.50D66AP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x8050d66a	 #-0x1.4359a800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1563
	ret
L1563:


	# Test 1565
	# b32- =0 -Inf -0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1564
	ret
L1564:


	# Test 1566
	# b32- =0 -1.7FFFFFP127 -0.000001P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1565
	ret
L1565:


	# Test 1567
	# b32- =0 -1.2C0FB0P-107 -0.000001P-126 -> -1.2C0FB0P-107 x


	addi a1, a1, 1
	li t0, 0x8a2c0fb0	 #-0x1.581f6000p-107
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8a2c0fb0	 #-0x1.581f6000p-107
	beq t0, a0, L1566
	ret
L1566:


	# Test 1568
	# b32- =0 -1.000000P-126 -0.000001P-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1567
	ret
L1567:


	# Test 1569
	# b32- =0 -0.7FFFFFP-126 -0.000001P-126 -> -0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ffffe	 #-0x1.fffff800p-127
	beq t0, a0, L1568
	ret
L1568:


	# Test 1570
	# b32- =0 -0.6986E7P-126 -0.000001P-126 -> -0.6986E6P-126 


	addi a1, a1, 1
	li t0, 0x806986e7	 #-0x1.a61b9c00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff806986e6	 #-0x1.a61b9800p-127
	beq t0, a0, L1569
	ret
L1569:


	# Test 1571
	# b32- =0 -0.000001P-126 -0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1570
	ret
L1570:


	# Test 1572
	# b32- =0 -1.000000P0 -0.000001P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1571
	ret
L1571:


	# Test 1573
	# b32- =0 -Zero -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1572
	ret
L1572:


	# Test 1574
	# b32- =0 +Zero -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1573
	ret
L1573:


	# Test 1575
	# b32- =0 +1.000000P0 -0.000001P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1574
	ret
L1574:


	# Test 1576
	# b32- =0 +0.000001P-126 -0.000001P-126 -> +0.000002P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2	 #0x1.00000000p-148
	beq t0, a0, L1575
	ret
L1575:


	# Test 1577
	# b32- =0 +0.0AC757P-126 -0.000001P-126 -> +0.0AC758P-126 


	addi a1, a1, 1
	li t0, 0xac757	 #0x1.58eae000p-130
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xac758	 #0x1.58eb0000p-130
	beq t0, a0, L1576
	ret
L1576:


	# Test 1578
	# b32- =0 +0.7FFFFFP-126 -0.000001P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1577
	ret
L1577:


	# Test 1579
	# b32- =0 +1.000000P-126 -0.000001P-126 -> +1.000001P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800001	 #0x1.00000200p-126
	beq t0, a0, L1578
	ret
L1578:


	# Test 1580
	# b32- =0 +1.607E8EP46 -0.000001P-126 -> +1.607E8EP46 x


	addi a1, a1, 1
	li t0, 0x56e07e8e	 #0x1.c0fd1c00p+46
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x56e07e8e	 #0x1.c0fd1c00p+46
	beq t0, a0, L1579
	ret
L1579:


	# Test 1581
	# b32- =0 +1.7FFFFFP127 -0.000001P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1580
	ret
L1580:


	# Test 1582
	# b32- =0 +Inf -0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1581
	ret
L1581:


	# Test 1583
	# b32- =0 q -0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1582
	ret
L1582:


	# Test 1584
	# b32- =0 q -0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1583
	ret
L1583:


	# Test 1585
	# b32- =0 -Inf -1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1584
	ret
L1584:


	# Test 1586
	# b32- =0 -1.7FFFFFP127 -1.000000P0 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1585
	ret
L1585:


	# Test 1587
	# b32- =0 -1.7AB46BP3 -1.000000P0 -> -1.6AB46BP3 


	addi a1, a1, 1
	li t0, 0xc17ab46b	 #-0x1.f568d600p+3
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc16ab46b	 #-0x1.d568d600p+3
	beq t0, a0, L1586
	ret
L1586:


	# Test 1588
	# b32- =0 -1.000000P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1587
	ret
L1587:


	# Test 1589
	# b32- =0 -0.7FFFFFP-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1588
	ret
L1588:


	# Test 1590
	# b32- =0 -0.786BA2P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80786ba2	 #-0x1.e1ae8800p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1589
	ret
L1589:


	# Test 1591
	# b32- =0 -0.000001P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1590
	ret
L1590:


	# Test 1592
	# b32- =0 -1.000000P0 -1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1591
	ret
L1591:


	# Test 1593
	# b32- =0 -Zero -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1592
	ret
L1592:


	# Test 1594
	# b32- =0 +Zero -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1593
	ret
L1593:


	# Test 1595
	# b32- =0 +1.000000P0 -1.000000P0 -> +1.000000P1 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x40000000	 #0x1.00000000p+1
	beq t0, a0, L1594
	ret
L1594:


	# Test 1596
	# b32- =0 +0.000001P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1595
	ret
L1595:


	# Test 1597
	# b32- =0 +0.16C42BP-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x16c42b	 #0x1.6c42b000p-129
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1596
	ret
L1596:


	# Test 1598
	# b32- =0 +0.7FFFFFP-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1597
	ret
L1597:


	# Test 1599
	# b32- =0 +1.000000P-126 -1.000000P0 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1598
	ret
L1598:


	# Test 1600
	# b32- =0 +1.2F2349P29 -1.000000P0 -> +1.2F2349P29 x


	addi a1, a1, 1
	li t0, 0x4e2f2349	 #0x1.5e469200p+29
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4e2f2349	 #0x1.5e469200p+29
	beq t0, a0, L1599
	ret
L1599:


	# Test 1601
	# b32- =0 +1.7FFFFFP127 -1.000000P0 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1600
	ret
L1600:


	# Test 1602
	# b32- =0 +Inf -1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1601
	ret
L1601:


	# Test 1603
	# b32- =0 q -1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1602
	ret
L1602:


	# Test 1604
	# b32- =0 q -1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1603
	ret
L1603:


	# Test 1605
	# b32- =0 -Inf -Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1604
	ret
L1604:


	# Test 1606
	# b32- =0 -1.7FFFFFP127 -Zero -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1605
	ret
L1605:


	# Test 1607
	# b32- =0 -1.5EF13FP49 -Zero -> -1.5EF13FP49 


	addi a1, a1, 1
	li t0, 0xd85ef13f	 #-0x1.bde27e00p+49
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd85ef13f	 #-0x1.bde27e00p+49
	beq t0, a0, L1606
	ret
L1606:


	# Test 1608
	# b32- =0 -1.000000P-126 -Zero -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1607
	ret
L1607:


	# Test 1609
	# b32- =0 -0.7FFFFFP-126 -Zero -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1608
	ret
L1608:


	# Test 1610
	# b32- =0 -0.47105EP-126 -Zero -> -0.47105EP-126 


	addi a1, a1, 1
	li t0, 0x8047105e	 #-0x1.1c417800p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8047105e	 #-0x1.1c417800p-127
	beq t0, a0, L1609
	ret
L1609:


	# Test 1611
	# b32- =0 -0.000001P-126 -Zero -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1610
	ret
L1610:


	# Test 1612
	# b32- =0 -1.000000P0 -Zero -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1611
	ret
L1611:


	# Test 1613
	# b32- =0 -Zero -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1612
	ret
L1612:


	# Test 1614
	# b32- =0 +Zero -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1613
	ret
L1613:


	# Test 1615
	# b32- =0 +1.000000P0 -Zero -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1614
	ret
L1614:


	# Test 1616
	# b32- =0 +0.000001P-126 -Zero -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1615
	ret
L1615:


	# Test 1617
	# b32- =0 +0.0D68E6P-126 -Zero -> +0.0D68E6P-126 


	addi a1, a1, 1
	li t0, 0xd68e6	 #0x1.ad1cc000p-130
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xd68e6	 #0x1.ad1cc000p-130
	beq t0, a0, L1616
	ret
L1616:


	# Test 1618
	# b32- =0 +0.7FFFFFP-126 -Zero -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1617
	ret
L1617:


	# Test 1619
	# b32- =0 +1.000000P-126 -Zero -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1618
	ret
L1618:


	# Test 1620
	# b32- =0 +1.7D8804P-53 -Zero -> +1.7D8804P-53 


	addi a1, a1, 1
	li t0, 0x257d8804	 #0x1.fb100800p-53
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x257d8804	 #0x1.fb100800p-53
	beq t0, a0, L1619
	ret
L1619:


	# Test 1621
	# b32- =0 +1.7FFFFFP127 -Zero -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1620
	ret
L1620:


	# Test 1622
	# b32- =0 +Inf -Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1621
	ret
L1621:


	# Test 1623
	# b32- =0 q -Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1622
	ret
L1622:


	# Test 1624
	# b32- =0 q -Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1623
	ret
L1623:


	# Test 1625
	# b32- =0 -Inf +Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1624
	ret
L1624:


	# Test 1626
	# b32- =0 -1.7FFFFFP127 +Zero -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1625
	ret
L1625:


	# Test 1627
	# b32- =0 -1.2D15FBP32 +Zero -> -1.2D15FBP32 


	addi a1, a1, 1
	li t0, 0xcfad15fb	 #-0x1.5a2bf600p+32
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcfad15fb	 #-0x1.5a2bf600p+32
	beq t0, a0, L1626
	ret
L1626:


	# Test 1628
	# b32- =0 -1.000000P-126 +Zero -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1627
	ret
L1627:


	# Test 1629
	# b32- =0 -0.7FFFFFP-126 +Zero -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1628
	ret
L1628:


	# Test 1630
	# b32- =0 -0.1F04FEP-126 +Zero -> -0.1F04FEP-126 


	addi a1, a1, 1
	li t0, 0x801f04fe	 #-0x1.f04fe000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff801f04fe	 #-0x1.f04fe000p-129
	beq t0, a0, L1629
	ret
L1629:


	# Test 1631
	# b32- =0 -0.000001P-126 +Zero -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1630
	ret
L1630:


	# Test 1632
	# b32- =0 -1.000000P0 +Zero -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1631
	ret
L1631:


	# Test 1633
	# b32- =0 -Zero +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1632
	ret
L1632:


	# Test 1634
	# b32- =0 +Zero +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1633
	ret
L1633:


	# Test 1635
	# b32- =0 +1.000000P0 +Zero -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1634
	ret
L1634:


	# Test 1636
	# b32- =0 +0.000001P-126 +Zero -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1635
	ret
L1635:


	# Test 1637
	# b32- =0 +0.040DA2P-126 +Zero -> +0.040DA2P-126 


	addi a1, a1, 1
	li t0, 0x40da2	 #0x1.03688000p-131
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x40da2	 #0x1.03688000p-131
	beq t0, a0, L1636
	ret
L1636:


	# Test 1638
	# b32- =0 +0.7FFFFFP-126 +Zero -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1637
	ret
L1637:


	# Test 1639
	# b32- =0 +1.000000P-126 +Zero -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1638
	ret
L1638:


	# Test 1640
	# b32- =0 +1.4C2CC0P-6 +Zero -> +1.4C2CC0P-6 


	addi a1, a1, 1
	li t0, 0x3ccc2cc0	 #0x1.98598000p-6
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3ccc2cc0	 #0x1.98598000p-6
	beq t0, a0, L1639
	ret
L1639:


	# Test 1641
	# b32- =0 +1.7FFFFFP127 +Zero -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1640
	ret
L1640:


	# Test 1642
	# b32- =0 +Inf +Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1641
	ret
L1641:


	# Test 1643
	# b32- =0 q +Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1642
	ret
L1642:


	# Test 1644
	# b32- =0 q +Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1643
	ret
L1643:


	# Test 1645
	# b32- =0 -Inf +1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1644
	ret
L1644:


	# Test 1646
	# b32- =0 -1.7FFFFFP127 +1.000000P0 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1645
	ret
L1645:


	# Test 1647
	# b32- =0 -1.7C3AB6P14 +1.000000P0 -> -1.7C3CB6P14 


	addi a1, a1, 1
	li t0, 0xc6fc3ab6	 #-0x1.f8756c00p+14
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc6fc3cb6	 #-0x1.f8796c00p+14
	beq t0, a0, L1646
	ret
L1646:


	# Test 1648
	# b32- =0 -1.000000P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1647
	ret
L1647:


	# Test 1649
	# b32- =0 -0.7FFFFFP-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1648
	ret
L1648:


	# Test 1650
	# b32- =0 -0.7891A1P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x807891a1	 #-0x1.e2468400p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1649
	ret
L1649:


	# Test 1651
	# b32- =0 -0.000001P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1650
	ret
L1650:


	# Test 1652
	# b32- =0 -1.000000P0 +1.000000P0 -> -1.000000P1 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0000000	 #-0x1.00000000p+1
	beq t0, a0, L1651
	ret
L1651:


	# Test 1653
	# b32- =0 -Zero +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1652
	ret
L1652:


	# Test 1654
	# b32- =0 +Zero +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1653
	ret
L1653:


	# Test 1655
	# b32- =0 +1.000000P0 +1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1654
	ret
L1654:


	# Test 1656
	# b32- =0 +0.000001P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1655
	ret
L1655:


	# Test 1657
	# b32- =0 +0.5D9A44P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x5d9a44	 #0x1.76691000p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1656
	ret
L1656:


	# Test 1658
	# b32- =0 +0.7FFFFFP-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1657
	ret
L1657:


	# Test 1659
	# b32- =0 +1.000000P-126 +1.000000P0 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1658
	ret
L1658:


	# Test 1660
	# b32- =0 +1.2F0948P14 +1.000000P0 -> +1.2F0748P14 


	addi a1, a1, 1
	li t0, 0x46af0948	 #0x1.5e129000p+14
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x46af0748	 #0x1.5e0e9000p+14
	beq t0, a0, L1659
	ret
L1659:


	# Test 1661
	# b32- =0 +1.7FFFFFP127 +1.000000P0 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1660
	ret
L1660:


	# Test 1662
	# b32- =0 +Inf +1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1661
	ret
L1661:


	# Test 1663
	# b32- =0 q +1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1662
	ret
L1662:


	# Test 1664
	# b32- =0 q +1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1663
	ret
L1663:


	# Test 1665
	# b32- =0 -Inf +0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1664
	ret
L1664:


	# Test 1666
	# b32- =0 -1.7FFFFFP127 +0.000001P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1665
	ret
L1665:


	# Test 1667
	# b32- =0 -1.358759P-2 +0.000001P-126 -> -1.358759P-2 x


	addi a1, a1, 1
	li t0, 0xbeb58759	 #-0x1.6b0eb200p-2
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbeb58759	 #-0x1.6b0eb200p-2
	beq t0, a0, L1666
	ret
L1666:


	# Test 1668
	# b32- =0 -1.000000P-126 +0.000001P-126 -> -1.000001P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800001	 #-0x1.00000200p-126
	beq t0, a0, L1667
	ret
L1667:


	# Test 1669
	# b32- =0 -0.7FFFFFP-126 +0.000001P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1668
	ret
L1668:


	# Test 1670
	# b32- =0 -0.47365CP-126 +0.000001P-126 -> -0.47365DP-126 


	addi a1, a1, 1
	li t0, 0x8047365c	 #-0x1.1cd97000p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8047365d	 #-0x1.1cd97400p-127
	beq t0, a0, L1669
	ret
L1669:


	# Test 1671
	# b32- =0 -0.000001P-126 +0.000001P-126 -> -0.000002P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000002	 #-0x1.00000000p-148
	beq t0, a0, L1670
	ret
L1670:


	# Test 1672
	# b32- =0 -1.000000P0 +0.000001P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1671
	ret
L1671:


	# Test 1673
	# b32- =0 -Zero +0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1672
	ret
L1672:


	# Test 1674
	# b32- =0 +Zero +0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1673
	ret
L1673:


	# Test 1675
	# b32- =0 +1.000000P0 +0.000001P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1674
	ret
L1674:


	# Test 1676
	# b32- =0 +0.000001P-126 +0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1675
	ret
L1675:


	# Test 1677
	# b32- =0 +0.415718P-126 +0.000001P-126 -> +0.415717P-126 


	addi a1, a1, 1
	li t0, 0x415718	 #0x1.055c6000p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x415717	 #0x1.055c5c00p-127
	beq t0, a0, L1676
	ret
L1676:


	# Test 1678
	# b32- =0 +0.7FFFFFP-126 +0.000001P-126 -> +0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7ffffe	 #0x1.fffff800p-127
	beq t0, a0, L1677
	ret
L1677:


	# Test 1679
	# b32- =0 +1.000000P-126 +0.000001P-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1678
	ret
L1678:


	# Test 1680
	# b32- =0 +1.7DAE03P60 +0.000001P-126 -> +1.7DAE03P60 x


	addi a1, a1, 1
	li t0, 0x5dfdae03	 #0x1.fb5c0600p+60
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5dfdae03	 #0x1.fb5c0600p+60
	beq t0, a0, L1679
	ret
L1679:


	# Test 1681
	# b32- =0 +1.7FFFFFP127 +0.000001P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1680
	ret
L1680:


	# Test 1682
	# b32- =0 +Inf +0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1681
	ret
L1681:


	# Test 1683
	# b32- =0 q +0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1682
	ret
L1682:


	# Test 1684
	# b32- =0 q +0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1683
	ret
L1683:


	# Test 1685
	# b32- =0 -Inf +0.38C699P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x38c699	 #0x1.c634c800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1684
	ret
L1684:


	# Test 1686
	# b32- =0 -1.7FFFFFP127 +0.5822ACP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x5822ac	 #0x1.608ab000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1685
	ret
L1685:


	# Test 1687
	# b32- =0 -1.042C14P-19 +0.3C3D4EP-126 -> -1.042C14P-19 x


	addi a1, a1, 1
	li t0, 0xb6042c14	 #-0x1.08582800p-19
	fmv.s.x f1, t0
	li t0, 0x3c3d4e	 #0x1.e1ea7000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb6042c14	 #-0x1.08582800p-19
	beq t0, a0, L1686
	ret
L1686:


	# Test 1688
	# b32- =0 -1.000000P-126 +0.629115P-126 -> -1.629115P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x629115	 #0x1.8a445400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80e29115	 #-0x1.c5222a00p-126
	beq t0, a0, L1687
	ret
L1687:


	# Test 1689
	# b32- =0 -0.7FFFFFP-126 +0.1DF07DP-126 -> -1.1DF07CP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1df07d	 #0x1.df07d000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff809df07c	 #-0x1.3be0f800p-126
	beq t0, a0, L1688
	ret
L1688:


	# Test 1690
	# b32- =0 -0.559B17P-126 +0.347A1FP-126 -> -1.0A1536P-126 


	addi a1, a1, 1
	li t0, 0x80559b17	 #-0x1.566c5c00p-127
	fmv.s.x f1, t0
	li t0, 0x347a1f	 #0x1.a3d0f800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff808a1536	 #-0x1.142a6c00p-126
	beq t0, a0, L1689
	ret
L1689:


	# Test 1691
	# b32- =0 -0.000001P-126 +0.2F72A0P-126 -> -0.2F72A1P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x2f72a0	 #0x1.7b950000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff802f72a1	 #-0x1.7b950800p-128
	beq t0, a0, L1690
	ret
L1690:


	# Test 1692
	# b32- =0 -1.000000P0 +0.433E4EP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x433e4e	 #0x1.0cf93800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1691
	ret
L1691:


	# Test 1693
	# b32- =0 -Zero +0.1E1DB6P-126 -> -0.1E1DB6P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1e1db6	 #0x1.e1db6000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff801e1db6	 #-0x1.e1db6000p-129
	beq t0, a0, L1692
	ret
L1692:


	# Test 1694
	# b32- =0 +Zero +0.597D1DP-126 -> -0.597D1DP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x597d1d	 #0x1.65f47400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80597d1d	 #-0x1.65f47400p-127
	beq t0, a0, L1693
	ret
L1693:


	# Test 1695
	# b32- =0 +1.000000P0 +0.2FDFD9P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x2fdfd9	 #0x1.7efec800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1694
	ret
L1694:


	# Test 1696
	# b32- =0 +0.000001P-126 +0.5ABF40P-126 -> -0.5ABF3FP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x5abf40	 #0x1.6afd0000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff805abf3f	 #-0x1.6afcfc00p-127
	beq t0, a0, L1695
	ret
L1695:


	# Test 1697
	# b32- =0 +0.503BD3P-126 +0.464253P-126 -> +0.09F980P-126 


	addi a1, a1, 1
	li t0, 0x503bd3	 #0x1.40ef4c00p-127
	fmv.s.x f1, t0
	li t0, 0x464253	 #0x1.19094c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x9f980	 #0x1.3f300000p-130
	beq t0, a0, L1696
	ret
L1696:


	# Test 1698
	# b32- =0 +0.7FFFFFP-126 +0.652DAAP-126 -> +0.1AD255P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x652daa	 #0x1.94b6a800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1ad255	 #0x1.ad255000p-129
	beq t0, a0, L1697
	ret
L1697:


	# Test 1699
	# b32- =0 +1.000000P-126 +0.300D12P-126 -> +0.4FF2EEP-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x300d12	 #0x1.80689000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4ff2ee	 #0x1.3fcbb800p-127
	beq t0, a0, L1698
	ret
L1698:


	# Test 1700
	# b32- =0 +1.61EAD7P42 +0.5DBF24P-126 -> +1.61EAD7P42 x


	addi a1, a1, 1
	li t0, 0x54e1ead7	 #0x1.c3d5ae00p+42
	fmv.s.x f1, t0
	li t0, 0x5dbf24	 #0x1.76fc9000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x54e1ead7	 #0x1.c3d5ae00p+42
	beq t0, a0, L1699
	ret
L1699:


	# Test 1701
	# b32- =0 +1.7FFFFFP127 +0.63F4D2P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x63f4d2	 #0x1.8fd34800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1700
	ret
L1700:


	# Test 1702
	# b32- =0 +Inf +0.5A178EP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x5a178e	 #0x1.685e3800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1701
	ret
L1701:


	# Test 1703
	# b32- =0 q +0.526C7DP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x526c7d	 #0x1.49b1f400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1702
	ret
L1702:


	# Test 1704
	# b32- =0 q +0.074883P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x74883	 #0x1.d220c000p-131
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1703
	ret
L1703:


	# Test 1705
	# b32- =0 -Inf +0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1704
	ret
L1704:


	# Test 1706
	# b32- =0 -1.7FFFFFP127 +0.7FFFFFP-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1705
	ret
L1705:


	# Test 1707
	# b32- =0 -1.6868E8P-38 +0.7FFFFFP-126 -> -1.6868E8P-38 x


	addi a1, a1, 1
	li t0, 0xace868e8	 #-0x1.d0d1d000p-38
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffface868e8	 #-0x1.d0d1d000p-38
	beq t0, a0, L1706
	ret
L1706:


	# Test 1708
	# b32- =0 -1.000000P-126 +0.7FFFFFP-126 -> -1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80ffffff	 #-0x1.fffffe00p-126
	beq t0, a0, L1707
	ret
L1707:


	# Test 1709
	# b32- =0 -0.7FFFFFP-126 +0.7FFFFFP-126 -> -1.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80fffffe	 #-0x1.fffffc00p-126
	beq t0, a0, L1708
	ret
L1708:


	# Test 1710
	# b32- =0 -0.64BFD3P-126 +0.7FFFFFP-126 -> -1.64BFD2P-126 


	addi a1, a1, 1
	li t0, 0x8064bfd3	 #-0x1.92ff4c00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80e4bfd2	 #-0x1.c97fa400p-126
	beq t0, a0, L1709
	ret
L1709:


	# Test 1711
	# b32- =0 -0.000001P-126 +0.7FFFFFP-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1710
	ret
L1710:


	# Test 1712
	# b32- =0 -1.000000P0 +0.7FFFFFP-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1711
	ret
L1711:


	# Test 1713
	# b32- =0 -Zero +0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1712
	ret
L1712:


	# Test 1714
	# b32- =0 +Zero +0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1713
	ret
L1713:


	# Test 1715
	# b32- =0 +1.000000P0 +0.7FFFFFP-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1714
	ret
L1714:


	# Test 1716
	# b32- =0 +0.000001P-126 +0.7FFFFFP-126 -> -0.7FFFFEP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807ffffe	 #-0x1.fffff800p-127
	beq t0, a0, L1715
	ret
L1715:


	# Test 1717
	# b32- =0 +0.5EE08FP-126 +0.7FFFFFP-126 -> -0.211F70P-126 


	addi a1, a1, 1
	li t0, 0x5ee08f	 #0x1.7b823c00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80211f70	 #-0x1.08fb8000p-128
	beq t0, a0, L1716
	ret
L1716:


	# Test 1718
	# b32- =0 +0.7FFFFFP-126 +0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1717
	ret
L1717:


	# Test 1719
	# b32- =0 +1.000000P-126 +0.7FFFFFP-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1718
	ret
L1718:


	# Test 1720
	# b32- =0 +1.30CF92P113 +0.7FFFFFP-126 -> +1.30CF92P113 x


	addi a1, a1, 1
	li t0, 0x7830cf92	 #0x1.619f2400p+113
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7830cf92	 #0x1.619f2400p+113
	beq t0, a0, L1719
	ret
L1719:


	# Test 1721
	# b32- =0 +1.7FFFFFP127 +0.7FFFFFP-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1720
	ret
L1720:


	# Test 1722
	# b32- =0 +Inf +0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1721
	ret
L1721:


	# Test 1723
	# b32- =0 q +0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1722
	ret
L1722:


	# Test 1724
	# b32- =0 q +0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1723
	ret
L1723:


	# Test 1725
	# b32- =0 -Inf +1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1724
	ret
L1724:


	# Test 1726
	# b32- =0 -1.7FFFFFP127 +1.000000P-126 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1725
	ret
L1725:


	# Test 1727
	# b32- =0 -1.4AC570P46 +1.000000P-126 -> -1.4AC570P46 x


	addi a1, a1, 1
	li t0, 0xd6cac570	 #-0x1.958ae000p+46
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd6cac570	 #-0x1.958ae000p+46
	beq t0, a0, L1726
	ret
L1726:


	# Test 1728
	# b32- =0 -1.000000P-126 +1.000000P-126 -> -1.000000P-125 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff81000000	 #-0x1.00000000p-125
	beq t0, a0, L1727
	ret
L1727:


	# Test 1729
	# b32- =0 -0.7FFFFFP-126 +1.000000P-126 -> -1.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80ffffff	 #-0x1.fffffe00p-126
	beq t0, a0, L1728
	ret
L1728:


	# Test 1730
	# b32- =0 -0.48FCA7P-126 +1.000000P-126 -> -1.48FCA7P-126 


	addi a1, a1, 1
	li t0, 0x8048fca7	 #-0x1.23f29c00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80c8fca7	 #-0x1.91f94e00p-126
	beq t0, a0, L1729
	ret
L1729:


	# Test 1731
	# b32- =0 -0.000001P-126 +1.000000P-126 -> -1.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800001	 #-0x1.00000200p-126
	beq t0, a0, L1730
	ret
L1730:


	# Test 1732
	# b32- =0 -1.000000P0 +1.000000P-126 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1731
	ret
L1731:


	# Test 1733
	# b32- =0 -Zero +1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1732
	ret
L1732:


	# Test 1734
	# b32- =0 +Zero +1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1733
	ret
L1733:


	# Test 1735
	# b32- =0 +1.000000P0 +1.000000P-126 -> +1.000000P0 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1734
	ret
L1734:


	# Test 1736
	# b32- =0 +0.000001P-126 +1.000000P-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1735
	ret
L1735:


	# Test 1737
	# b32- =0 +0.6D854AP-126 +1.000000P-126 -> -0.127AB6P-126 


	addi a1, a1, 1
	li t0, 0x6d854a	 #0x1.b6152800p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80127ab6	 #-0x1.27ab6000p-129
	beq t0, a0, L1736
	ret
L1736:


	# Test 1738
	# b32- =0 +0.7FFFFFP-126 +1.000000P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1737
	ret
L1737:


	# Test 1739
	# b32- =0 +1.000000P-126 +1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1738
	ret
L1738:


	# Test 1740
	# b32- =0 +1.7F744DP119 +1.000000P-126 -> +1.7F744DP119 x


	addi a1, a1, 1
	li t0, 0x7b7f744d	 #0x1.fee89a00p+119
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7b7f744d	 #0x1.fee89a00p+119
	beq t0, a0, L1739
	ret
L1739:


	# Test 1741
	# b32- =0 +1.7FFFFFP127 +1.000000P-126 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1740
	ret
L1740:


	# Test 1742
	# b32- =0 +Inf +1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1741
	ret
L1741:


	# Test 1743
	# b32- =0 q +1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1742
	ret
L1742:


	# Test 1744
	# b32- =0 q +1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1743
	ret
L1743:


	# Test 1745
	# b32- =0 -Inf +1.1A0630P-10 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3a9a0630	 #0x1.340c6000p-10
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1744
	ret
L1744:


	# Test 1746
	# b32- =0 -1.7FFFFFP127 +1.256598P59 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x5d256598	 #0x1.4acb3000p+59
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1745
	ret
L1745:


	# Test 1747
	# b32- =0 -1.19EA2BP29 +1.162D14P-43 -> -1.19EA2BP29 x


	addi a1, a1, 1
	li t0, 0xce19ea2b	 #-0x1.33d45600p+29
	fmv.s.x f1, t0
	li t0, 0x2a162d14	 #0x1.2c5a2800p-43
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffce19ea2b	 #-0x1.33d45600p+29
	beq t0, a0, L1746
	ret
L1746:


	# Test 1748
	# b32- =0 -1.000000P-126 +1.0410ADP-103 -> -1.0410AEP-103 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xc0410ad	 #0x1.08215a00p-103
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8c0410ae	 #-0x1.08215c00p-103
	beq t0, a0, L1747
	ret
L1747:


	# Test 1749
	# b32- =0 -0.7FFFFFP-126 +1.7AB369P-81 -> -1.7AB369P-81 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x177ab369	 #0x1.f566d200p-81
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff977ab369	 #-0x1.f566d200p-81
	beq t0, a0, L1748
	ret
L1748:


	# Test 1750
	# b32- =0 -0.572162P-126 +1.1E032BP-82 -> -1.1E032BP-82 x


	addi a1, a1, 1
	li t0, 0x80572162	 #-0x1.5c858800p-127
	fmv.s.x f1, t0
	li t0, 0x169e032b	 #0x1.3c065600p-82
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff969e032b	 #-0x1.3c065600p-82
	beq t0, a0, L1749
	ret
L1749:


	# Test 1751
	# b32- =0 -0.000001P-126 +1.10F238P-38 -> -1.10F238P-38 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x2c90f238	 #0x1.21e47000p-38
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffac90f238	 #-0x1.21e47000p-38
	beq t0, a0, L1750
	ret
L1750:


	# Test 1752
	# b32- =0 -1.000000P0 +1.0754F4P-95 -> -1.000000P0 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x100754f4	 #0x1.0ea9e800p-95
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1751
	ret
L1751:


	# Test 1753
	# b32- =0 -Zero +1.5B20A2P41 -> -1.5B20A2P41 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x545b20a2	 #0x1.b6414400p+41
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd45b20a2	 #-0x1.b6414400p+41
	beq t0, a0, L1752
	ret
L1752:


	# Test 1754
	# b32- =0 +Zero +1.660009P78 -> -1.660009P78 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x66e60009	 #0x1.cc001200p+78
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe6e60009	 #-0x1.cc001200p+78
	beq t0, a0, L1753
	ret
L1753:


	# Test 1755
	# b32- =0 +1.000000P0 +1.715F71P83 -> -1.715F71P83 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x69715f71	 #0x1.e2bee200p+83
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe9715f71	 #-0x1.e2bee200p+83
	beq t0, a0, L1754
	ret
L1754:


	# Test 1756
	# b32- =0 +0.000001P-126 +1.7CBED8P126 -> -1.7CBED8P126 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7efcbed8	 #0x1.f97db000p+126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffefcbed8	 #-0x1.f97db000p+126
	beq t0, a0, L1755
	ret
L1755:


	# Test 1757
	# b32- =0 +0.5061D2P-126 +1.2B895AP97 -> -1.2B895AP97 x


	addi a1, a1, 1
	li t0, 0x5061d2	 #0x1.41874800p-127
	fmv.s.x f1, t0
	li t0, 0x702b895a	 #0x1.5712b400p+97
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff02b895a	 #-0x1.5712b400p+97
	beq t0, a0, L1756
	ret
L1756:


	# Test 1758
	# b32- =0 +0.7FFFFFP-126 +1.5B69EDP38 -> -1.5B69EDP38 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x52db69ed	 #0x1.b6d3da00p+38
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd2db69ed	 #-0x1.b6d3da00p+38
	beq t0, a0, L1757
	ret
L1757:


	# Test 1759
	# b32- =0 +1.000000P-126 +1.51CCA9P109 -> -1.51CCA9P109 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7651cca9	 #0x1.a3995200p+109
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff651cca9	 #-0x1.a3995200p+109
	beq t0, a0, L1758
	ret
L1758:


	# Test 1760
	# b32- =0 +1.4D9909P-10 +1.33DF71P42 -> -1.33DF71P42 x


	addi a1, a1, 1
	li t0, 0x3acd9909	 #0x1.9b321200p-10
	fmv.s.x f1, t0
	li t0, 0x54b3df71	 #0x1.67bee200p+42
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd4b3df71	 #-0x1.67bee200p+42
	beq t0, a0, L1759
	ret
L1759:


	# Test 1761
	# b32- =0 +1.7FFFFFP127 +1.684B78P87 -> +1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x6b684b78	 #0x1.d096f000p+87
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1760
	ret
L1760:


	# Test 1762
	# b32- =0 +Inf +1.3BD726P64 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x5fbbd726	 #0x1.77ae4c00p+64
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1761
	ret
L1761:


	# Test 1763
	# b32- =0 q +1.3C3588P3 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x413c3588	 #0x1.786b1000p+3
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1762
	ret
L1762:


	# Test 1764
	# b32- =0 q +1.7D518EP-59 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x227d518e	 #0x1.faa31c00p-59
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1763
	ret
L1763:


	# Test 1765
	# b32- =0 -Inf +1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1764
	ret
L1764:


	# Test 1766
	# b32- =0 -1.7FFFFFP127 +1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1765
	ret
L1765:


	# Test 1767
	# b32- =0 -1.684EE6P75 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xe5684ee6	 #-0x1.d09dcc00p+75
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1766
	ret
L1766:


	# Test 1768
	# b32- =0 -1.000000P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1767
	ret
L1767:


	# Test 1769
	# b32- =0 -0.7FFFFFP-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1768
	ret
L1768:


	# Test 1770
	# b32- =0 -0.66461DP-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x8066461d	 #-0x1.99187400p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1769
	ret
L1769:


	# Test 1771
	# b32- =0 -0.000001P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1770
	ret
L1770:


	# Test 1772
	# b32- =0 -1.000000P0 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1771
	ret
L1771:


	# Test 1773
	# b32- =0 -Zero +1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1772
	ret
L1772:


	# Test 1774
	# b32- =0 +Zero +1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1773
	ret
L1773:


	# Test 1775
	# b32- =0 +1.000000P0 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1774
	ret
L1774:


	# Test 1776
	# b32- =0 +0.000001P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1775
	ret
L1775:


	# Test 1777
	# b32- =0 +0.041EA6P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x41ea6	 #0x1.07a98000p-131
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1776
	ret
L1776:


	# Test 1778
	# b32- =0 +0.7FFFFFP-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1777
	ret
L1777:


	# Test 1779
	# b32- =0 +1.000000P-126 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1778
	ret
L1778:


	# Test 1780
	# b32- =0 +1.1CBDC4P-27 +1.7FFFFFP127 -> -1.7FFFFFP127 x


	addi a1, a1, 1
	li t0, 0x321cbdc4	 #0x1.397b8800p-27
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1779
	ret
L1779:


	# Test 1781
	# b32- =0 +1.7FFFFFP127 +1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1780
	ret
L1780:


	# Test 1782
	# b32- =0 +Inf +1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1781
	ret
L1781:


	# Test 1783
	# b32- =0 q +1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1782
	ret
L1782:


	# Test 1784
	# b32- =0 q +1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1783
	ret
L1783:


	# Test 1785
	# b32- =0 -Inf +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1784
	ret
L1784:


	# Test 1786
	# b32- =0 -1.7FFFFFP127 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1785
	ret
L1785:


	# Test 1787
	# b32- =0 -1.4C0BBAP105 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xf44c0bba	 #-0x1.98177400p+105
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1786
	ret
L1786:


	# Test 1788
	# b32- =0 -1.000000P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1787
	ret
L1787:


	# Test 1789
	# b32- =0 -0.7FFFFFP-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1788
	ret
L1788:


	# Test 1790
	# b32- =0 -0.74EAD8P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x8074ead8	 #-0x1.d3ab6000p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1789
	ret
L1789:


	# Test 1791
	# b32- =0 -0.000001P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1790
	ret
L1790:


	# Test 1792
	# b32- =0 -1.000000P0 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1791
	ret
L1791:


	# Test 1793
	# b32- =0 -Zero +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1792
	ret
L1792:


	# Test 1794
	# b32- =0 +Zero +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1793
	ret
L1793:


	# Test 1795
	# b32- =0 +1.000000P0 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1794
	ret
L1794:


	# Test 1796
	# b32- =0 +0.000001P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1795
	ret
L1795:


	# Test 1797
	# b32- =0 +0.428361P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x428361	 #0x1.0a0d8400p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1796
	ret
L1796:


	# Test 1798
	# b32- =0 +0.7FFFFFP-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1797
	ret
L1797:


	# Test 1799
	# b32- =0 +1.000000P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1798
	ret
L1798:


	# Test 1800
	# b32- =0 +1.00FA98P-109 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x900fa98	 #0x1.01f53000p-109
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1799
	ret
L1799:


	# Test 1801
	# b32- =0 +1.7FFFFFP127 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1800
	ret
L1800:


	# Test 1802
	# b32- =0 +Inf +Inf -> q i


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1801
	ret
L1801:


	# Test 1803
	# b32- =0 q +Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1802
	ret
L1802:


	# Test 1804
	# b32- =0 q +Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1803
	ret
L1803:


	# Test 1805
	# b32- =0 -Inf q -> q 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1804
	ret
L1804:


	# Test 1806
	# b32- =0 -1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1805
	ret
L1805:


	# Test 1807
	# b32- =0 -1.1B3075P120 q -> q 


	addi a1, a1, 1
	li t0, 0xfb9b3075	 #-0x1.3660ea00p+120
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1806
	ret
L1806:


	# Test 1808
	# b32- =0 -1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1807
	ret
L1807:


	# Test 1809
	# b32- =0 -0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1808
	ret
L1808:


	# Test 1810
	# b32- =0 -0.58A7ACP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x8058a7ac	 #-0x1.629eb000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1809
	ret
L1809:


	# Test 1811
	# b32- =0 -0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1810
	ret
L1810:


	# Test 1812
	# b32- =0 -1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1811
	ret
L1811:


	# Test 1813
	# b32- =0 -Zero q -> q 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1812
	ret
L1812:


	# Test 1814
	# b32- =0 +Zero q -> q 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1813
	ret
L1813:


	# Test 1815
	# b32- =0 +1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1814
	ret
L1814:


	# Test 1816
	# b32- =0 +0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1815
	ret
L1815:


	# Test 1817
	# b32- =0 +0.51A81CP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x51a81c	 #0x1.46a07000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1816
	ret
L1816:


	# Test 1818
	# b32- =0 +0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1817
	ret
L1817:


	# Test 1819
	# b32- =0 +1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1818
	ret
L1818:


	# Test 1820
	# b32- =0 +1.4F5F53P-63 q -> q 


	addi a1, a1, 1
	li t0, 0x204f5f53	 #0x1.9ebea600p-63
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1819
	ret
L1819:


	# Test 1821
	# b32- =0 +1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1820
	ret
L1820:


	# Test 1822
	# b32- =0 +Inf q -> q 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1821
	ret
L1821:


	# Test 1823
	# b32- =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1822
	ret
L1822:


	# Test 1824
	# b32- =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1823
	ret
L1823:


	# Test 1825
	# b32- =0 -Inf q -> q 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1824
	ret
L1824:


	# Test 1826
	# b32- =0 -1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1825
	ret
L1825:


	# Test 1827
	# b32- =0 -1.69D531P-42 q -> q 


	addi a1, a1, 1
	li t0, 0xaae9d531	 #-0x1.d3aa6200p-42
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1826
	ret
L1826:


	# Test 1828
	# b32- =0 -1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1827
	ret
L1827:


	# Test 1829
	# b32- =0 -0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1828
	ret
L1828:


	# Test 1830
	# b32- =0 -0.674C67P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80674c67	 #-0x1.9d319c00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1829
	ret
L1829:


	# Test 1831
	# b32- =0 -0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1830
	ret
L1830:


	# Test 1832
	# b32- =0 -1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1831
	ret
L1831:


	# Test 1833
	# b32- =0 -Zero q -> q 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1832
	ret
L1832:


	# Test 1834
	# b32- =0 +Zero q -> q 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1833
	ret
L1833:


	# Test 1835
	# b32- =0 +1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1834
	ret
L1834:


	# Test 1836
	# b32- =0 +0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1835
	ret
L1835:


	# Test 1837
	# b32- =0 +0.604CD8P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x604cd8	 #0x1.81336000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1836
	ret
L1836:


	# Test 1838
	# b32- =0 +0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1837
	ret
L1837:


	# Test 1839
	# b32- =0 +1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1838
	ret
L1838:


	# Test 1840
	# b32- =0 +1.1E440EP-96 q -> q 


	addi a1, a1, 1
	li t0, 0xf9e440e	 #0x1.3c881c00p-96
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1839
	ret
L1839:


	# Test 1841
	# b32- =0 +1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1840
	ret
L1840:


	# Test 1842
	# b32- =0 +Inf q -> q 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1841
	ret
L1841:


	# Test 1843
	# b32- =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1842
	ret
L1842:


	# Test 1844
	# b32- =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fsub.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L1843
	ret
L1843:


	# Test 1845
	# b32* =0 i -Inf -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1844
	ret
L1844:


	# Test 1846
	# b32* =0 i -1.7FFFFFP127 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1845
	ret
L1845:


	# Test 1847
	# b32* =0 i -1.03DC38P2 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xc083dc38	 #-0x1.07b87000p+2
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1846
	ret
L1846:


	# Test 1848
	# b32* =0 i -1.000000P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1847
	ret
L1847:


	# Test 1849
	# b32* =0 i -0.7FFFFFP-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1848
	ret
L1848:


	# Test 1850
	# b32* =0 i -0.2C7B56P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x802c7b56	 #-0x1.63dab000p-128
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1849
	ret
L1849:


	# Test 1851
	# b32* =0 i -0.000001P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1850
	ret
L1850:


	# Test 1852
	# b32* =0 i -1.000000P0 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1851
	ret
L1851:


	# Test 1853
	# b32* =0 i +1.000000P0 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1852
	ret
L1852:


	# Test 1854
	# b32* =0 i +0.000001P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1853
	ret
L1853:


	# Test 1855
	# b32* =0 i +0.7AD3DFP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7ad3df	 #0x1.eb4f7c00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1854
	ret
L1854:


	# Test 1856
	# b32* =0 i +0.7FFFFFP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1855
	ret
L1855:


	# Test 1857
	# b32* =0 i +1.000000P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1856
	ret
L1856:


	# Test 1858
	# b32* =0 i +1.380B16P-37 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x2d380b16	 #0x1.70162c00p-37
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1857
	ret
L1857:


	# Test 1859
	# b32* =0 i +1.7FFFFFP127 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1858
	ret
L1858:


	# Test 1860
	# b32* =0 i +Inf -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1859
	ret
L1859:


	# Test 1861
	# b32* =0 i -Inf -1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1860
	ret
L1860:


	# Test 1862
	# b32* =0 i -1.7FFFFFP127 -1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1861
	ret
L1861:


	# Test 1863
	# b32* =0 i -1.5280F3P-112 -1.7FFFFFP127 -> +1.5280F2P16 x


	addi a1, a1, 1
	li t0, 0x87d280f3	 #-0x1.a501e600p-112
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x47d280f2	 #0x1.a501e400p+16
	beq t0, a0, L1862
	ret
L1862:


	# Test 1864
	# b32* =0 i -1.000000P-126 -1.7FFFFFP127 -> +1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407fffff	 #0x1.fffffe00p+1
	beq t0, a0, L1863
	ret
L1863:


	# Test 1865
	# b32* =0 i -0.7FFFFFP-126 -1.7FFFFFP127 -> +1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407ffffd	 #0x1.fffffa00p+1
	beq t0, a0, L1864
	ret
L1864:


	# Test 1866
	# b32* =0 i -0.30382AP-126 -1.7FFFFFP127 -> +1.40E0A7P0 x


	addi a1, a1, 1
	li t0, 0x8030382a	 #-0x1.81c15000p-128
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3fc0e0a7	 #0x1.81c14e00p+0
	beq t0, a0, L1865
	ret
L1865:


	# Test 1867
	# b32* =0 i -0.000001P-126 -1.7FFFFFP127 -> +1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34ffffff	 #0x1.fffffe00p-22
	beq t0, a0, L1866
	ret
L1866:


	# Test 1868
	# b32* =0 i -1.000000P0 -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1867
	ret
L1867:


	# Test 1869
	# b32* =0 i -Zero -1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1868
	ret
L1868:


	# Test 1870
	# b32* =0 i +Zero -1.7FFFFFP127 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1869
	ret
L1869:


	# Test 1871
	# b32* =0 i +1.000000P0 -1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1870
	ret
L1870:


	# Test 1872
	# b32* =0 i +0.000001P-126 -1.7FFFFFP127 -> -1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4ffffff	 #-0x1.fffffe00p-22
	beq t0, a0, L1871
	ret
L1871:


	# Test 1873
	# b32* =0 i +0.49789AP-126 -1.7FFFFFP127 -> -1.12F133P1 x


	addi a1, a1, 1
	li t0, 0x49789a	 #0x1.25e26800p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc012f133	 #-0x1.25e26600p+1
	beq t0, a0, L1872
	ret
L1872:


	# Test 1874
	# b32* =0 i +0.7FFFFFP-126 -1.7FFFFFP127 -> -1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07ffffd	 #-0x1.fffffa00p+1
	beq t0, a0, L1873
	ret
L1873:


	# Test 1875
	# b32* =0 i +1.000000P-126 -1.7FFFFFP127 -> -1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07fffff	 #-0x1.fffffe00p+1
	beq t0, a0, L1874
	ret
L1874:


	# Test 1876
	# b32* =0 i +1.06AFD1P-54 -1.7FFFFFP127 -> -1.06AFD0P74 x


	addi a1, a1, 1
	li t0, 0x2486afd1	 #0x1.0d5fa200p-54
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe486afd0	 #-0x1.0d5fa000p+74
	beq t0, a0, L1875
	ret
L1875:


	# Test 1877
	# b32* =0 i +1.7FFFFFP127 -1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1876
	ret
L1876:


	# Test 1878
	# b32* =0 i +Inf -1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1877
	ret
L1877:


	# Test 1879
	# b32* =0 i -Inf -1.555090P-65 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x9f555090	 #-0x1.aaa12000p-65
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1878
	ret
L1878:


	# Test 1880
	# b32* =0 i -1.7FFFFFP127 -1.60EFF7P-120 -> +1.60EFF6P8 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x83e0eff7	 #-0x1.c1dfee00p-120
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x43e0eff6	 #0x1.c1dfec00p+8
	beq t0, a0, L1879
	ret
L1879:


	# Test 1881
	# b32* =0 i -1.2165AFP-33 -1.5B6A7CP21 -> +1.0A551FP-11 x


	addi a1, a1, 1
	li t0, 0xaf2165af	 #-0x1.42cb5e00p-33
	fmv.s.x f1, t0
	li t0, 0xca5b6a7c	 #-0x1.b6d4f800p+21
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3a0a551f	 #0x1.14aa3e00p-11
	beq t0, a0, L1880
	ret
L1880:


	# Test 1882
	# b32* =0 i -1.000000P-126 -1.62321AP-48 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xa7e2321a	 #-0x1.c4643400p-48
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1881
	ret
L1881:


	# Test 1883
	# b32* =0 i -0.7FFFFFP-126 -1.6D5182P-43 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xaa6d5182	 #-0x1.daa30400p-43
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1882
	ret
L1882:


	# Test 1884
	# b32* =0 i -0.3F5CE5P-126 -1.638093P46 -> +1.613CC8P-81 x


	addi a1, a1, 1
	li t0, 0x803f5ce5	 #-0x1.fae72800p-128
	fmv.s.x f1, t0
	li t0, 0xd6e38093	 #-0x1.c7012600p+46
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x17613cc8	 #0x1.c2799000p-81
	beq t0, a0, L1883
	ret
L1883:


	# Test 1885
	# b32* =0 i -0.000001P-126 -1.4C3C97P-93 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x914c3c97	 #-0x1.98792e00p-93
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1884
	ret
L1884:


	# Test 1886
	# b32* =0 i -1.000000P0 -1.42DF53P42 -> +1.42DF53P42 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xd4c2df53	 #-0x1.85bea600p+42
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x54c2df53	 #0x1.85bea600p+42
	beq t0, a0, L1885
	ret
L1885:


	# Test 1887
	# b32* =0 i -Zero -1.4DBEBAP79 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xe74dbeba	 #-0x1.9b7d7400p+79
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1886
	ret
L1886:


	# Test 1888
	# b32* =0 i +Zero -1.589E22P100 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xf1d89e22	 #-0x1.b13c4400p+100
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1887
	ret
L1887:


	# Test 1889
	# b32* =0 i +1.000000P0 -1.2CE9D0P109 -> -1.2CE9D0P109 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xf62ce9d0	 #-0x1.59d3a000p+109
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff62ce9d0	 #-0x1.59d3a000p+109
	beq t0, a0, L1888
	ret
L1888:


	# Test 1890
	# b32* =0 i +0.000001P-126 -1.37C937P126 -> -1.37C937P-23 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xfeb7c937	 #-0x1.6f926e00p+126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb437c937	 #-0x1.6f926e00p-23
	beq t0, a0, L1889
	ret
L1889:


	# Test 1891
	# b32* =0 i +0.579D56P-126 -1.7086C3P-47 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x579d56	 #0x1.5e755800p-127
	fmv.s.x f1, t0
	li t0, 0xa87086c3	 #-0x1.e10d8600p-47
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1890
	ret
L1890:


	# Test 1892
	# b32* =0 i +0.7FFFFFP-126 -1.4E4806P108 -> -1.4E4804P-18 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xf5ce4806	 #-0x1.9c900c00p+108
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb6ce4804	 #-0x1.9c900800p-18
	beq t0, a0, L1891
	ret
L1891:


	# Test 1893
	# b32* =0 i +1.000000P-126 -1.0D1709P-73 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x9b0d1709	 #-0x1.1a2e1200p-73
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1892
	ret
L1892:


	# Test 1894
	# b32* =0 i +1.55D48CP120 -1.795CDAP-118 -> -1.50494BP3 x


	addi a1, a1, 1
	li t0, 0x7bd5d48c	 #0x1.aba91800p+120
	fmv.s.x f1, t0
	li t0, 0x84f95cda	 #-0x1.f2b9b400p-118
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc150494b	 #-0x1.a0929600p+3
	beq t0, a0, L1893
	ret
L1893:


	# Test 1895
	# b32* =0 i +1.7FFFFFP127 -1.2315D7P1 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xc02315d7	 #-0x1.462bae00p+1
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1894
	ret
L1894:


	# Test 1896
	# b32* =0 i +Inf -1.19B893P-88 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x9399b893	 #-0x1.33712600p-88
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1895
	ret
L1895:


	# Test 1897
	# b32* =0 i -Inf -1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1896
	ret
L1896:


	# Test 1898
	# b32* =0 i -1.7FFFFFP127 -1.000000P-126 -> +1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407fffff	 #0x1.fffffe00p+1
	beq t0, a0, L1897
	ret
L1897:


	# Test 1899
	# b32* =0 i -1.700A6AP-51 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0xa6700a6a	 #-0x1.e014d400p-51
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1898
	ret
L1898:


	# Test 1900
	# b32* =0 i -1.000000P-126 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1899
	ret
L1899:


	# Test 1901
	# b32* =0 i -0.7FFFFFP-126 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1900
	ret
L1900:


	# Test 1902
	# b32* =0 i -0.2DC1A1P-126 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x802dc1a1	 #-0x1.6e0d0800p-128
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1901
	ret
L1901:


	# Test 1903
	# b32* =0 i -0.000001P-126 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1902
	ret
L1902:


	# Test 1904
	# b32* =0 i -1.000000P0 -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1903
	ret
L1903:


	# Test 1905
	# b32* =0 i -Zero -1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1904
	ret
L1904:


	# Test 1906
	# b32* =0 i +Zero -1.000000P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1905
	ret
L1905:


	# Test 1907
	# b32* =0 i +1.000000P0 -1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1906
	ret
L1906:


	# Test 1908
	# b32* =0 i +0.000001P-126 -1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1907
	ret
L1907:


	# Test 1909
	# b32* =0 i +0.7B9A29P-126 -1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7b9a29	 #0x1.ee68a400p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1908
	ret
L1908:


	# Test 1910
	# b32* =0 i +0.7FFFFFP-126 -1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1909
	ret
L1909:


	# Test 1911
	# b32* =0 i +1.000000P-126 -1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1910
	ret
L1910:


	# Test 1912
	# b32* =0 i +1.247948P119 -1.000000P-126 -> -1.247948P-7 


	addi a1, a1, 1
	li t0, 0x7b247948	 #0x1.48f29000p+119
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbc247948	 #-0x1.48f29000p-7
	beq t0, a0, L1911
	ret
L1911:


	# Test 1913
	# b32* =0 i +1.7FFFFFP127 -1.000000P-126 -> -1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07fffff	 #-0x1.fffffe00p+1
	beq t0, a0, L1912
	ret
L1912:


	# Test 1914
	# b32* =0 i +Inf -1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1913
	ret
L1913:


	# Test 1915
	# b32* =0 i -Inf -0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1914
	ret
L1914:


	# Test 1916
	# b32* =0 i -1.7FFFFFP127 -0.7FFFFFP-126 -> +1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407ffffd	 #0x1.fffffa00p+1
	beq t0, a0, L1915
	ret
L1915:


	# Test 1917
	# b32* =0 i -1.3EAF25P92 -0.7FFFFFP-126 -> +1.3EAF24P-34 x


	addi a1, a1, 1
	li t0, 0xedbeaf25	 #-0x1.7d5e4a00p+92
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2ebeaf24	 #0x1.7d5e4800p-34
	beq t0, a0, L1916
	ret
L1916:


	# Test 1918
	# b32* =0 i -1.000000P-126 -0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1917
	ret
L1917:


	# Test 1919
	# b32* =0 i -0.7FFFFFP-126 -0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1918
	ret
L1918:


	# Test 1920
	# b32* =0 i -0.1C665CP-126 -0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x801c665c	 #-0x1.c665c000p-129
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1919
	ret
L1919:


	# Test 1921
	# b32* =0 i -0.000001P-126 -0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1920
	ret
L1920:


	# Test 1922
	# b32* =0 i -1.000000P0 -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1921
	ret
L1921:


	# Test 1923
	# b32* =0 i -Zero -0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1922
	ret
L1922:


	# Test 1924
	# b32* =0 i +Zero -0.7FFFFFP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1923
	ret
L1923:


	# Test 1925
	# b32* =0 i +1.000000P0 -0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1924
	ret
L1924:


	# Test 1926
	# b32* =0 i +0.000001P-126 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1925
	ret
L1925:


	# Test 1927
	# b32* =0 i +0.4ABEE5P-126 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x4abee5	 #0x1.2afb9400p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1926
	ret
L1926:


	# Test 1928
	# b32* =0 i +0.7FFFFFP-126 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1927
	ret
L1927:


	# Test 1929
	# b32* =0 i +1.000000P-126 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1928
	ret
L1928:


	# Test 1930
	# b32* =0 i +1.08761BP85 -0.7FFFFFP-126 -> -1.08761AP-41 x


	addi a1, a1, 1
	li t0, 0x6a08761b	 #0x1.10ec3600p+85
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffab08761a	 #-0x1.10ec3400p-41
	beq t0, a0, L1929
	ret
L1929:


	# Test 1931
	# b32* =0 i +1.7FFFFFP127 -0.7FFFFFP-126 -> -1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07ffffd	 #-0x1.fffffa00p+1
	beq t0, a0, L1930
	ret
L1930:


	# Test 1932
	# b32* =0 i +Inf -0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1931
	ret
L1931:


	# Test 1933
	# b32* =0 i -Inf -0.2E63E1P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x802e63e1	 #-0x1.731f0800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1932
	ret
L1932:


	# Test 1934
	# b32* =0 i -1.7FFFFFP127 -0.6D32E3P-126 -> +1.5A65C5P1 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x806d32e3	 #-0x1.b4cb8c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x405a65c5	 #0x1.b4cb8a00p+1
	beq t0, a0, L1933
	ret
L1933:


	# Test 1935
	# b32* =0 i -1.22EBF9P-22 -0.45F388P-126 -> +0.000001P-126 xu


	addi a1, a1, 1
	li t0, 0xb4a2ebf9	 #-0x1.45d7f200p-22
	fmv.s.x f1, t0
	li t0, 0x8045f388	 #-0x1.17ce2000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1934
	ret
L1934:


	# Test 1936
	# b32* =0 i -1.000000P-126 -0.43B1B2P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x8043b1b2	 #-0x1.0ec6c800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1935
	ret
L1935:


	# Test 1937
	# b32* =0 i -0.7FFFFFP-126 -0.3A546EP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x803a546e	 #-0x1.d2a37000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1936
	ret
L1936:


	# Test 1938
	# b32* =0 i -0.5F42E4P-126 -0.49079AP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x805f42e4	 #-0x1.7d0b9000p-127
	fmv.s.x f1, t0
	li t0, 0x8049079a	 #-0x1.241e6800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1937
	ret
L1937:


	# Test 1939
	# b32* =0 i -0.000001P-126 -0.0DFC2FP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800dfc2f	 #-0x1.bf85e000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1938
	ret
L1938:


	# Test 1940
	# b32* =0 i -1.000000P0 -0.789B96P-126 -> +0.789B96P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80789b96	 #-0x1.e26e5800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x789b96	 #0x1.e26e5800p-127
	beq t0, a0, L1939
	ret
L1939:


	# Test 1941
	# b32* =0 i -Zero -0.1F3E52P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x801f3e52	 #-0x1.f3e52000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1940
	ret
L1940:


	# Test 1942
	# b32* =0 i +Zero -0.7A5DBAP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807a5dba	 #-0x1.e976e800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1941
	ret
L1941:


	# Test 1943
	# b32* =0 i +1.000000P0 -0.15BD21P-126 -> -0.15BD21P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8015bd21	 #-0x1.5bd21000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8015bd21	 #-0x1.5bd21000p-129
	beq t0, a0, L1942
	ret
L1942:


	# Test 1944
	# b32* =0 i +0.000001P-126 -0.44CC23P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x8044cc23	 #-0x1.13308c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1943
	ret
L1943:


	# Test 1945
	# b32* =0 i +0.5963A0P-126 -0.3B4FCEP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x5963a0	 #0x1.658e8000p-127
	fmv.s.x f1, t0
	li t0, 0x803b4fce	 #-0x1.da7e7000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1944
	ret
L1944:


	# Test 1946
	# b32* =0 i +0.7FFFFFP-126 -0.5A8AF2P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x805a8af2	 #-0x1.6a2bc800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1945
	ret
L1945:


	# Test 1947
	# b32* =0 i +1.000000P-126 -0.166A5AP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80166a5a	 #-0x1.66a5a000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1946
	ret
L1946:


	# Test 1948
	# b32* =0 i +1.571AD7P67 -0.2325E5P-126 -> -1.6C4416P-61 x


	addi a1, a1, 1
	li t0, 0x61571ad7	 #0x1.ae35ae00p+67
	fmv.s.x f1, t0
	li t0, 0x802325e5	 #-0x1.192f2800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa16c4416	 #-0x1.d8882c00p-61
	beq t0, a0, L1947
	ret
L1947:


	# Test 1949
	# b32* =0 i +1.7FFFFFP127 -0.1018C4P-126 -> -1.00C61FP-1 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x801018c4	 #-0x1.018c4000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf00c61f	 #-0x1.018c3e00p-1
	beq t0, a0, L1948
	ret
L1948:


	# Test 1950
	# b32* =0 i +Inf -0.7B382BP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807b382b	 #-0x1.ece0ac00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1949
	ret
L1949:


	# Test 1951
	# b32* =0 i -Inf -0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1950
	ret
L1950:


	# Test 1952
	# b32* =0 i -1.7FFFFFP127 -0.000001P-126 -> +1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34ffffff	 #0x1.fffffe00p-22
	beq t0, a0, L1951
	ret
L1951:


	# Test 1953
	# b32* =0 i -1.7110B4P120 -0.000001P-126 -> +1.7110B4P-29 


	addi a1, a1, 1
	li t0, 0xfbf110b4	 #-0x1.e2216800p+120
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x317110b4	 #0x1.e2216800p-29
	beq t0, a0, L1952
	ret
L1952:


	# Test 1954
	# b32* =0 i -1.000000P-126 -0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1953
	ret
L1953:


	# Test 1955
	# b32* =0 i -0.7FFFFFP-126 -0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1954
	ret
L1954:


	# Test 1956
	# b32* =0 i -0.437FB8P-126 -0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80437fb8	 #-0x1.0dfee000p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1955
	ret
L1955:


	# Test 1957
	# b32* =0 i -0.000001P-126 -0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1956
	ret
L1956:


	# Test 1958
	# b32* =0 i -1.000000P0 -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1957
	ret
L1957:


	# Test 1959
	# b32* =0 i -Zero -0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1958
	ret
L1958:


	# Test 1960
	# b32* =0 i +Zero -0.000001P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1959
	ret
L1959:


	# Test 1961
	# b32* =0 i +1.000000P0 -0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1960
	ret
L1960:


	# Test 1962
	# b32* =0 i +0.000001P-126 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1961
	ret
L1961:


	# Test 1963
	# b32* =0 i +0.28085BP-126 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x28085b	 #0x1.4042d800p-128
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1962
	ret
L1962:


	# Test 1964
	# b32* =0 i +0.7FFFFFP-126 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1963
	ret
L1963:


	# Test 1965
	# b32* =0 i +1.000000P-126 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1964
	ret
L1964:


	# Test 1966
	# b32* =0 i +1.39F75FP-41 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x2b39f75f	 #0x1.73eebe00p-41
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1965
	ret
L1965:


	# Test 1967
	# b32* =0 i +1.7FFFFFP127 -0.000001P-126 -> -1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4ffffff	 #-0x1.fffffe00p-22
	beq t0, a0, L1966
	ret
L1966:


	# Test 1968
	# b32* =0 i +Inf -0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1967
	ret
L1967:


	# Test 1969
	# b32* =0 i -Inf -1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L1968
	ret
L1968:


	# Test 1970
	# b32* =0 i -1.7FFFFFP127 -1.000000P0 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L1969
	ret
L1969:


	# Test 1971
	# b32* =0 i -1.407570P7 -1.000000P0 -> +1.407570P7 


	addi a1, a1, 1
	li t0, 0xc3407570	 #-0x1.80eae000p+7
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x43407570	 #0x1.80eae000p+7
	beq t0, a0, L1970
	ret
L1970:


	# Test 1972
	# b32* =0 i -1.000000P-126 -1.000000P0 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L1971
	ret
L1971:


	# Test 1973
	# b32* =0 i -0.7FFFFFP-126 -1.000000P0 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L1972
	ret
L1972:


	# Test 1974
	# b32* =0 i -0.31A473P-126 -1.000000P0 -> +0.31A473P-126 


	addi a1, a1, 1
	li t0, 0x8031a473	 #-0x1.8d239800p-128
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x31a473	 #0x1.8d239800p-128
	beq t0, a0, L1973
	ret
L1973:


	# Test 1975
	# b32* =0 i -0.000001P-126 -1.000000P0 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L1974
	ret
L1974:


	# Test 1976
	# b32* =0 i -1.000000P0 -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L1975
	ret
L1975:


	# Test 1977
	# b32* =0 i -Zero -1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1976
	ret
L1976:


	# Test 1978
	# b32* =0 i +Zero -1.000000P0 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1977
	ret
L1977:


	# Test 1979
	# b32* =0 i +1.000000P0 -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L1978
	ret
L1978:


	# Test 1980
	# b32* =0 i +0.000001P-126 -1.000000P0 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L1979
	ret
L1979:


	# Test 1981
	# b32* =0 i +0.36AD17P-126 -1.000000P0 -> -0.36AD17P-126 


	addi a1, a1, 1
	li t0, 0x36ad17	 #0x1.b568b800p-128
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8036ad17	 #-0x1.b568b800p-128
	beq t0, a0, L1980
	ret
L1980:


	# Test 1982
	# b32* =0 i +0.7FFFFFP-126 -1.000000P0 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L1981
	ret
L1981:


	# Test 1983
	# b32* =0 i +1.000000P-126 -1.000000P0 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L1982
	ret
L1982:


	# Test 1984
	# b32* =0 i +1.081C1AP-58 -1.000000P0 -> -1.081C1AP-58 


	addi a1, a1, 1
	li t0, 0x22881c1a	 #0x1.10383400p-58
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa2881c1a	 #-0x1.10383400p-58
	beq t0, a0, L1983
	ret
L1983:


	# Test 1985
	# b32* =0 i +1.7FFFFFP127 -1.000000P0 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L1984
	ret
L1984:


	# Test 1986
	# b32* =0 i +Inf -1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L1985
	ret
L1985:


	# Test 1987
	# b32* =0 i -1.7FFFFFP127 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1986
	ret
L1986:


	# Test 1988
	# b32* =0 i -1.0E9A2BP54 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0xda8e9a2b	 #-0x1.1d345600p+54
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1987
	ret
L1987:


	# Test 1989
	# b32* =0 i -1.000000P-126 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1988
	ret
L1988:


	# Test 1990
	# b32* =0 i -0.7FFFFFP-126 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1989
	ret
L1989:


	# Test 1991
	# b32* =0 i -0.20C92EP-126 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x8020c92e	 #-0x1.06497000p-128
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1990
	ret
L1990:


	# Test 1992
	# b32* =0 i -0.000001P-126 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1991
	ret
L1991:


	# Test 1993
	# b32* =0 i -1.000000P0 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1992
	ret
L1992:


	# Test 1994
	# b32* =0 i -Zero -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L1993
	ret
L1993:


	# Test 1995
	# b32* =0 i +Zero -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1994
	ret
L1994:


	# Test 1996
	# b32* =0 i +1.000000P0 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1995
	ret
L1995:


	# Test 1997
	# b32* =0 i +0.000001P-126 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1996
	ret
L1996:


	# Test 1998
	# b32* =0 i +0.3AA9EAP-126 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x3aa9ea	 #0x1.d54f5000p-128
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1997
	ret
L1997:


	# Test 1999
	# b32* =0 i +0.7FFFFFP-126 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1998
	ret
L1998:


	# Test 2000
	# b32* =0 i +1.000000P-126 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L1999
	ret
L1999:


	# Test 2001
	# b32* =0 i +1.5740D5P-12 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x39d740d5	 #0x1.ae81aa00p-12
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2000
	ret
L2000:


	# Test 2002
	# b32* =0 i +1.7FFFFFP127 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2001
	ret
L2001:


	# Test 2003
	# b32* =0 i -1.7FFFFFP127 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2002
	ret
L2002:


	# Test 2004
	# b32* =0 i -1.068ECBP105 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0xf4068ecb	 #-0x1.0d1d9600p+105
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2003
	ret
L2003:


	# Test 2005
	# b32* =0 i -1.000000P-126 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2004
	ret
L2004:


	# Test 2006
	# b32* =0 i -0.7FFFFFP-126 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2005
	ret
L2005:


	# Test 2007
	# b32* =0 i -0.2F2DEAP-126 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x802f2dea	 #-0x1.796f5000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2006
	ret
L2006:


	# Test 2008
	# b32* =0 i -0.000001P-126 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2007
	ret
L2007:


	# Test 2009
	# b32* =0 i -1.000000P0 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2008
	ret
L2008:


	# Test 2010
	# b32* =0 i -Zero +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2009
	ret
L2009:


	# Test 2011
	# b32* =0 i +Zero +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2010
	ret
L2010:


	# Test 2012
	# b32* =0 i +1.000000P0 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2011
	ret
L2011:


	# Test 2013
	# b32* =0 i +0.000001P-126 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2012
	ret
L2012:


	# Test 2014
	# b32* =0 i +0.294EA6P-126 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x294ea6	 #0x1.4a753000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2013
	ret
L2013:


	# Test 2015
	# b32* =0 i +0.7FFFFFP-126 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2014
	ret
L2014:


	# Test 2016
	# b32* =0 i +1.000000P-126 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2015
	ret
L2015:


	# Test 2017
	# b32* =0 i +1.25A591P35 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x5125a591	 #0x1.4b4b2200p+35
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2016
	ret
L2016:


	# Test 2018
	# b32* =0 i +1.7FFFFFP127 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2017
	ret
L2017:


	# Test 2019
	# b32* =0 i -Inf +1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2018
	ret
L2018:


	# Test 2020
	# b32* =0 i -1.7FFFFFP127 +1.000000P0 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L2019
	ret
L2019:


	# Test 2021
	# b32* =0 i -1.553387P-9 +1.000000P0 -> -1.553387P-9 


	addi a1, a1, 1
	li t0, 0xbb553387	 #-0x1.aa670e00p-9
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbb553387	 #-0x1.aa670e00p-9
	beq t0, a0, L2020
	ret
L2020:


	# Test 2022
	# b32* =0 i -1.000000P-126 +1.000000P0 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L2021
	ret
L2021:


	# Test 2023
	# b32* =0 i -0.7FFFFFP-126 +1.000000P0 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L2022
	ret
L2022:


	# Test 2024
	# b32* =0 i -0.3DD2A5P-126 +1.000000P0 -> -0.3DD2A5P-126 


	addi a1, a1, 1
	li t0, 0x803dd2a5	 #-0x1.ee952800p-128
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff803dd2a5	 #-0x1.ee952800p-128
	beq t0, a0, L2023
	ret
L2023:


	# Test 2025
	# b32* =0 i -0.000001P-126 +1.000000P0 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L2024
	ret
L2024:


	# Test 2026
	# b32* =0 i -1.000000P0 +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2025
	ret
L2025:


	# Test 2027
	# b32* =0 i -Zero +1.000000P0 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2026
	ret
L2026:


	# Test 2028
	# b32* =0 i +Zero +1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2027
	ret
L2027:


	# Test 2029
	# b32* =0 i +1.000000P0 +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2028
	ret
L2028:


	# Test 2030
	# b32* =0 i +0.000001P-126 +1.000000P0 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L2029
	ret
L2029:


	# Test 2031
	# b32* =0 i +0.4C2B2EP-126 +1.000000P0 -> +0.4C2B2EP-126 


	addi a1, a1, 1
	li t0, 0x4c2b2e	 #0x1.30acb800p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4c2b2e	 #0x1.30acb800p-127
	beq t0, a0, L2030
	ret
L2030:


	# Test 2032
	# b32* =0 i +0.7FFFFFP-126 +1.000000P0 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L2031
	ret
L2031:


	# Test 2033
	# b32* =0 i +1.000000P-126 +1.000000P0 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L2032
	ret
L2032:


	# Test 2034
	# b32* =0 i +1.74CA4CP17 +1.000000P0 -> +1.74CA4CP17 


	addi a1, a1, 1
	li t0, 0x4874ca4c	 #0x1.e9949800p+17
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4874ca4c	 #0x1.e9949800p+17
	beq t0, a0, L2033
	ret
L2033:


	# Test 2035
	# b32* =0 i +1.7FFFFFP127 +1.000000P0 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L2034
	ret
L2034:


	# Test 2036
	# b32* =0 i +Inf +1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2035
	ret
L2035:


	# Test 2037
	# b32* =0 i -Inf +0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2036
	ret
L2036:


	# Test 2038
	# b32* =0 i -1.7FFFFFP127 +0.000001P-126 -> -1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4ffffff	 #-0x1.fffffe00p-22
	beq t0, a0, L2037
	ret
L2037:


	# Test 2039
	# b32* =0 i -1.245842P-26 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0xb2a45842	 #-0x1.48b08400p-26
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2038
	ret
L2038:


	# Test 2040
	# b32* =0 i -1.000000P-126 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2039
	ret
L2039:


	# Test 2041
	# b32* =0 i -0.7FFFFFP-126 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2040
	ret
L2040:


	# Test 2042
	# b32* =0 i -0.1CF760P-126 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x801cf760	 #-0x1.cf760000p-129
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2041
	ret
L2041:


	# Test 2043
	# b32* =0 i -0.000001P-126 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2042
	ret
L2042:


	# Test 2044
	# b32* =0 i -1.000000P0 +0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L2043
	ret
L2043:


	# Test 2045
	# b32* =0 i -Zero +0.000001P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2044
	ret
L2044:


	# Test 2046
	# b32* =0 i +Zero +0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2045
	ret
L2045:


	# Test 2047
	# b32* =0 i +1.000000P0 +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L2046
	ret
L2046:


	# Test 2048
	# b32* =0 i +0.000001P-126 +0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2047
	ret
L2047:


	# Test 2049
	# b32* =0 i +0.25B7D0P-126 +0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x25b7d0	 #0x1.2dbe8000p-128
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2048
	ret
L2048:


	# Test 2050
	# b32* =0 i +0.7FFFFFP-126 +0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2049
	ret
L2049:


	# Test 2051
	# b32* =0 i +1.000000P-126 +0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2050
	ret
L2050:


	# Test 2052
	# b32* =0 i +1.436F07P64 +0.000001P-126 -> +1.436F07P-85 


	addi a1, a1, 1
	li t0, 0x5fc36f07	 #0x1.86de0e00p+64
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x15436f07	 #0x1.86de0e00p-85
	beq t0, a0, L2051
	ret
L2051:


	# Test 2053
	# b32* =0 i +1.7FFFFFP127 +0.000001P-126 -> +1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34ffffff	 #0x1.fffffe00p-22
	beq t0, a0, L2052
	ret
L2052:


	# Test 2054
	# b32* =0 i +Inf +0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2053
	ret
L2053:


	# Test 2055
	# b32* =0 i -Inf +0.7D2642P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7d2642	 #0x1.f4990800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2054
	ret
L2054:


	# Test 2056
	# b32* =0 i -1.7FFFFFP127 +0.1845A9P-126 -> -1.422D47P-1 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1845a9	 #0x1.845a9000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf422d47	 #-0x1.845a8e00p-1
	beq t0, a0, L2055
	ret
L2055:


	# Test 2057
	# b32* =0 i -1.5DA4E5P21 +0.1865A1P-126 -> -1.28FB97P-107 x


	addi a1, a1, 1
	li t0, 0xca5da4e5	 #-0x1.bb49ca00p+21
	fmv.s.x f1, t0
	li t0, 0x1865a1	 #0x1.865a1000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8a28fb97	 #-0x1.51f72e00p-107
	beq t0, a0, L2056
	ret
L2056:


	# Test 2058
	# b32* =0 i -1.000000P-126 +0.2987CDP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x2987cd	 #0x1.4c3e6800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2057
	ret
L2057:


	# Test 2059
	# b32* =0 i -0.7FFFFFP-126 +0.5DD37BP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x5dd37b	 #0x1.774dec00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2058
	ret
L2058:


	# Test 2060
	# b32* =0 i -0.1B1C1CP-126 +0.10FBB8P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x801b1c1c	 #-0x1.b1c1c000p-129
	fmv.s.x f1, t0
	li t0, 0x10fbb8	 #0x1.0fbb8000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2059
	ret
L2059:


	# Test 2061
	# b32* =0 i -0.000001P-126 +0.5F559EP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x5f559e	 #0x1.7d567800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2060
	ret
L2060:


	# Test 2062
	# b32* =0 i -1.000000P0 +0.2A7505P-126 -> -0.2A7505P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x2a7505	 #0x1.53a82800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff802a7505	 #-0x1.53a82800p-128
	beq t0, a0, L2061
	ret
L2061:


	# Test 2063
	# b32* =0 i -Zero +0.7E00B3P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7e00b3	 #0x1.f802cc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2062
	ret
L2062:


	# Test 2064
	# b32* =0 i +Zero +0.19601BP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x19601b	 #0x1.9601b000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2063
	ret
L2063:


	# Test 2065
	# b32* =0 i +1.000000P0 +0.7FC2D7P-126 -> +0.7FC2D7P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc2d7	 #0x1.ff0b5c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc2d7	 #0x1.ff0b5c00p-127
	beq t0, a0, L2064
	ret
L2064:


	# Test 2066
	# b32* =0 i +0.000001P-126 +0.2AE23EP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x2ae23e	 #0x1.5711f000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2065
	ret
L2065:


	# Test 2067
	# b32* =0 i +0.345C8CP-126 +0.5D81E7P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x345c8c	 #0x1.a2e46000p-128
	fmv.s.x f1, t0
	li t0, 0x5d81e7	 #0x1.76079c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2066
	ret
L2066:


	# Test 2068
	# b32* =0 i +0.7FFFFFP-126 +0.19CD53P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x19cd53	 #0x1.9cd53000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2067
	ret
L2067:


	# Test 2069
	# b32* =0 i +1.000000P-126 +0.74ECBBP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x74ecbb	 #0x1.d3b2ec00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2068
	ret
L2068:


	# Test 2070
	# b32* =0 i +1.1193C3P-81 +0.6657FEP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x171193c3	 #0x1.23278600p-81
	fmv.s.x f1, t0
	li t0, 0x6657fe	 #0x1.995ff800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2069
	ret
L2069:


	# Test 2071
	# b32* =0 i +1.7FFFFFP127 +0.4B6B8AP-126 -> +1.16D713P1 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x4b6b8a	 #0x1.2dae2800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4016d713	 #0x1.2dae2600p+1
	beq t0, a0, L2070
	ret
L2070:


	# Test 2072
	# b32* =0 i +Inf +0.21CE46P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x21ce46	 #0x1.0e723000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2071
	ret
L2071:


	# Test 2073
	# b32* =0 i -Inf +0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2072
	ret
L2072:


	# Test 2074
	# b32* =0 i -1.7FFFFFP127 +0.7FFFFFP-126 -> -1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07ffffd	 #-0x1.fffffa00p+1
	beq t0, a0, L2073
	ret
L2073:


	# Test 2075
	# b32* =0 i -1.2C49A0P4 +0.7FFFFFP-126 -> -1.2C499FP-122 x


	addi a1, a1, 1
	li t0, 0xc1ac49a0	 #-0x1.58934000p+4
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff82ac499f	 #-0x1.58933e00p-122
	beq t0, a0, L2074
	ret
L2074:


	# Test 2076
	# b32* =0 i -1.000000P-126 +0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2075
	ret
L2075:


	# Test 2077
	# b32* =0 i -0.7FFFFFP-126 +0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2076
	ret
L2076:


	# Test 2078
	# b32* =0 i -0.0A40D7P-126 +0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800a40d7	 #-0x1.481ae000p-130
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2077
	ret
L2077:


	# Test 2079
	# b32* =0 i -0.000001P-126 +0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2078
	ret
L2078:


	# Test 2080
	# b32* =0 i -1.000000P0 +0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L2079
	ret
L2079:


	# Test 2081
	# b32* =0 i -Zero +0.7FFFFFP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2080
	ret
L2080:


	# Test 2082
	# b32* =0 i +Zero +0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2081
	ret
L2081:


	# Test 2083
	# b32* =0 i +1.000000P0 +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L2082
	ret
L2082:


	# Test 2084
	# b32* =0 i +0.000001P-126 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2083
	ret
L2083:


	# Test 2085
	# b32* =0 i +0.385960P-126 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x385960	 #0x1.c2cb0000p-128
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2084
	ret
L2084:


	# Test 2086
	# b32* =0 i +0.7FFFFFP-126 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2085
	ret
L2085:


	# Test 2087
	# b32* =0 i +1.000000P-126 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2086
	ret
L2086:


	# Test 2088
	# b32* =0 i +1.60B87EP-67 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1e60b87e	 #0x1.c170fc00p-67
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2087
	ret
L2087:


	# Test 2089
	# b32* =0 i +1.7FFFFFP127 +0.7FFFFFP-126 -> +1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407ffffd	 #0x1.fffffa00p+1
	beq t0, a0, L2088
	ret
L2088:


	# Test 2090
	# b32* =0 i +Inf +0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2089
	ret
L2089:


	# Test 2091
	# b32* =0 i -Inf +1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2090
	ret
L2090:


	# Test 2092
	# b32* =0 i -1.7FFFFFP127 +1.000000P-126 -> -1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07fffff	 #-0x1.fffffe00p+1
	beq t0, a0, L2091
	ret
L2091:


	# Test 2093
	# b32* =0 i -1.100674P50 +1.000000P-126 -> -1.100674P-76 


	addi a1, a1, 1
	li t0, 0xd8900674	 #-0x1.200ce800p+50
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff99900674	 #-0x1.200ce800p-76
	beq t0, a0, L2092
	ret
L2092:


	# Test 2094
	# b32* =0 i -1.000000P-126 +1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2093
	ret
L2093:


	# Test 2095
	# b32* =0 i -0.7FFFFFP-126 +1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2094
	ret
L2094:


	# Test 2096
	# b32* =0 i -0.02A592P-126 +1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x8002a592	 #-0x1.52c90000p-132
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2095
	ret
L2095:


	# Test 2097
	# b32* =0 i -0.000001P-126 +1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2096
	ret
L2096:


	# Test 2098
	# b32* =0 i -1.000000P0 +1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L2097
	ret
L2097:


	# Test 2099
	# b32* =0 i -Zero +1.000000P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2098
	ret
L2098:


	# Test 2100
	# b32* =0 i +Zero +1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2099
	ret
L2099:


	# Test 2101
	# b32* =0 i +1.000000P0 +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L2100
	ret
L2100:


	# Test 2102
	# b32* =0 i +0.000001P-126 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2101
	ret
L2101:


	# Test 2103
	# b32* =0 i +0.26FE1BP-126 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x26fe1b	 #0x1.37f0d800p-128
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2102
	ret
L2102:


	# Test 2104
	# b32* =0 i +0.7FFFFFP-126 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2103
	ret
L2103:


	# Test 2105
	# b32* =0 i +1.000000P-126 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2104
	ret
L2104:


	# Test 2106
	# b32* =0 i +1.44F552P-53 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x2544f552	 #0x1.89eaa400p-53
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2105
	ret
L2105:


	# Test 2107
	# b32* =0 i +1.7FFFFFP127 +1.000000P-126 -> +1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407fffff	 #0x1.fffffe00p+1
	beq t0, a0, L2106
	ret
L2106:


	# Test 2108
	# b32* =0 i +Inf +1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2107
	ret
L2107:


	# Test 2109
	# b32* =0 i -Inf +1.0A692EP108 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x758a692e	 #0x1.14d25c00p+108
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2108
	ret
L2108:


	# Test 2110
	# b32* =0 i -1.7FFFFFP127 +1.154896P49 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x58154896	 #0x1.2a912c00p+49
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2109
	ret
L2109:


	# Test 2111
	# b32* =0 i -1.5F2B2FP32 +1.726EACP-49 -> -1.53574DP-16 x


	addi a1, a1, 1
	li t0, 0xcfdf2b2f	 #-0x1.be565e00p+32
	fmv.s.x f1, t0
	li t0, 0x27726eac	 #0x1.e4dd5800p-49
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb7d3574d	 #-0x1.a6ae9a00p-16
	beq t0, a0, L2110
	ret
L2110:


	# Test 2112
	# b32* =0 i -1.000000P-126 +1.16CAB9P29 -> -1.16CAB9P-97 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x4e16cab9	 #0x1.2d957200p+29
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8f16cab9	 #-0x1.2d957200p-97
	beq t0, a0, L2111
	ret
L2111:


	# Test 2113
	# b32* =0 i -0.7FFFFFP-126 +1.21EA20P34 -> -1.21EA1FP-92 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x50a1ea20	 #0x1.43d44000p+34
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff91a1ea1f	 #-0x1.43d43e00p-92
	beq t0, a0, L2112
	ret
L2112:


	# Test 2114
	# b32* =0 i -0.0CE266P-126 +1.7AC4C3P-112 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800ce266	 #-0x1.9c4cc000p-130
	fmv.s.x f1, t0
	li t0, 0x7fac4c3	 #0x1.f5898600p-112
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2113
	ret
L2113:


	# Test 2115
	# b32* =0 i -0.000001P-126 +1.009536P16 -> -0.01012AP-126 xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x47809536	 #0x1.012a6c00p+16
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8001012a	 #-0x1.012a0000p-133
	beq t0, a0, L2114
	ret
L2114:


	# Test 2116
	# b32* =0 i -1.000000P0 +1.0C749DP21 -> -1.0C749DP21 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x4a0c749d	 #0x1.18e93a00p+21
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffca0c749d	 #-0x1.18e93a00p+21
	beq t0, a0, L2115
	ret
L2115:


	# Test 2117
	# b32* =0 i -Zero +1.171405P26 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x4c971405	 #0x1.2e280a00p+26
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2116
	ret
L2116:


	# Test 2118
	# b32* =0 i +Zero +1.22336CP119 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7b22336c	 #0x1.4466d800p+119
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2117
	ret
L2117:


	# Test 2119
	# b32* =0 i +1.000000P0 +1.61426EP-23 -> +1.61426EP-23 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3461426e	 #0x1.c284dc00p-23
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3461426e	 #0x1.c284dc00p-23
	beq t0, a0, L2118
	ret
L2118:


	# Test 2120
	# b32* =0 i +0.000001P-126 +1.6C21D6P46 -> +1.6C21D6P-103 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x56ec21d6	 #0x1.d843ac00p+46
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xc6c21d6	 #0x1.d843ac00p-103
	beq t0, a0, L2119
	ret
L2119:


	# Test 2121
	# b32* =0 i +0.35A2D6P-126 +1.07CAF3P-52 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x35a2d6	 #0x1.ad16b000p-128
	fmv.s.x f1, t0
	li t0, 0x2587caf3	 #0x1.0f95e600p-52
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2120
	ret
L2120:


	# Test 2122
	# b32* =0 i +0.7FFFFFP-126 +1.02E0A5P-71 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1c02e0a5	 #0x1.05c14a00p-71
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2121
	ret
L2121:


	# Test 2123
	# b32* =0 i +1.000000P-126 +1.41AFA7P99 -> +1.41AFA7P-27 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7141afa7	 #0x1.835f4e00p+99
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3241afa7	 #0x1.835f4e00p-27
	beq t0, a0, L2122
	ret
L2122:


	# Test 2124
	# b32* =0 i +1.135A0DP-118 +1.10210AP125 -> +1.25EB57P7 x


	addi a1, a1, 1
	li t0, 0x4935a0d	 #0x1.26b41a00p-118
	fmv.s.x f1, t0
	li t0, 0x7e10210a	 #0x1.20421400p+125
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4325eb57	 #0x1.4bd6ae00p+7
	beq t0, a0, L2123
	ret
L2123:


	# Test 2125
	# b32* =0 i +1.7FFFFFP127 +1.586E76P-99 -> +1.586E75P29 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xe586e76	 #0x1.b0dcec00p-99
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4e586e75	 #0x1.b0dcea00p+29
	beq t0, a0, L2124
	ret
L2124:


	# Test 2126
	# b32* =0 i +Inf +1.4E1132P52 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x59ce1132	 #0x1.9c226400p+52
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2125
	ret
L2125:


	# Test 2127
	# b32* =0 i -Inf +1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2126
	ret
L2126:


	# Test 2128
	# b32* =0 i -1.7FFFFFP127 +1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2127
	ret
L2127:


	# Test 2129
	# b32* =0 i -1.2DCFEAP15 +1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xc72dcfea	 #-0x1.5b9fd400p+15
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2128
	ret
L2128:


	# Test 2130
	# b32* =0 i -1.000000P-126 +1.7FFFFFP127 -> -1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07fffff	 #-0x1.fffffe00p+1
	beq t0, a0, L2129
	ret
L2129:


	# Test 2131
	# b32* =0 i -0.7FFFFFP-126 +1.7FFFFFP127 -> -1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07ffffd	 #-0x1.fffffa00p+1
	beq t0, a0, L2130
	ret
L2130:


	# Test 2132
	# b32* =0 i -0.6B0721P-126 +1.7FFFFFP127 -> -1.560E41P1 x


	addi a1, a1, 1
	li t0, 0x806b0721	 #-0x1.ac1c8400p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0560e41	 #-0x1.ac1c8200p+1
	beq t0, a0, L2131
	ret
L2131:


	# Test 2133
	# b32* =0 i -0.000001P-126 +1.7FFFFFP127 -> -1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4ffffff	 #-0x1.fffffe00p-22
	beq t0, a0, L2132
	ret
L2132:


	# Test 2134
	# b32* =0 i -1.000000P0 +1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L2133
	ret
L2133:


	# Test 2135
	# b32* =0 i -Zero +1.7FFFFFP127 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2134
	ret
L2134:


	# Test 2136
	# b32* =0 i +Zero +1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2135
	ret
L2135:


	# Test 2137
	# b32* =0 i +1.000000P0 +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L2136
	ret
L2136:


	# Test 2138
	# b32* =0 i +0.000001P-126 +1.7FFFFFP127 -> +1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34ffffff	 #0x1.fffffe00p-22
	beq t0, a0, L2137
	ret
L2137:


	# Test 2139
	# b32* =0 i +0.244791P-126 +1.7FFFFFP127 -> +1.111E43P0 x


	addi a1, a1, 1
	li t0, 0x244791	 #0x1.223c8800p-128
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f911e43	 #0x1.223c8600p+0
	beq t0, a0, L2138
	ret
L2138:


	# Test 2140
	# b32* =0 i +0.7FFFFFP-126 +1.7FFFFFP127 -> +1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407ffffd	 #0x1.fffffa00p+1
	beq t0, a0, L2139
	ret
L2139:


	# Test 2141
	# b32* =0 i +1.000000P-126 +1.7FFFFFP127 -> +1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407fffff	 #0x1.fffffe00p+1
	beq t0, a0, L2140
	ret
L2140:


	# Test 2142
	# b32* =0 i +1.627EC8P-104 +1.7FFFFFP127 -> +1.627EC7P24 x


	addi a1, a1, 1
	li t0, 0xbe27ec8	 #0x1.c4fd9000p-104
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4be27ec7	 #0x1.c4fd8e00p+24
	beq t0, a0, L2141
	ret
L2141:


	# Test 2143
	# b32* =0 i +1.7FFFFFP127 +1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2142
	ret
L2142:


	# Test 2144
	# b32* =0 i +Inf +1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2143
	ret
L2143:


	# Test 2145
	# b32* =0 i -Inf +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2144
	ret
L2144:


	# Test 2146
	# b32* =0 i -1.7FFFFFP127 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2145
	ret
L2145:


	# Test 2147
	# b32* =0 i -1.7C74A6P-67 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x9e7c74a6	 #-0x1.f8e94c00p-67
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2146
	ret
L2146:


	# Test 2148
	# b32* =0 i -1.000000P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2147
	ret
L2147:


	# Test 2149
	# b32* =0 i -0.7FFFFFP-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2148
	ret
L2148:


	# Test 2150
	# b32* =0 i -0.2E63A9P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x802e63a9	 #-0x1.731d4800p-128
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2149
	ret
L2149:


	# Test 2151
	# b32* =0 i -0.000001P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2150
	ret
L2150:


	# Test 2152
	# b32* =0 i -1.000000P0 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2151
	ret
L2151:


	# Test 2153
	# b32* =0 i +1.000000P0 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2152
	ret
L2152:


	# Test 2154
	# b32* =0 i +0.000001P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2153
	ret
L2153:


	# Test 2155
	# b32* =0 i +0.280465P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x280465	 #0x1.40232800p-128
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2154
	ret
L2154:


	# Test 2156
	# b32* =0 i +0.7FFFFFP-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2155
	ret
L2155:


	# Test 2157
	# b32* =0 i +1.000000P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2156
	ret
L2156:


	# Test 2158
	# b32* =0 i +1.30A383P-105 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xb30a383	 #0x1.61470600p-105
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2157
	ret
L2157:


	# Test 2159
	# b32* =0 i +1.7FFFFFP127 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2158
	ret
L2158:


	# Test 2160
	# b32* =0 i +Inf +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2159
	ret
L2159:


	# Test 2161
	# b32* =0 -Inf -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2160
	ret
L2160:


	# Test 2162
	# b32* =0 -1.7FFFFFP127 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2161
	ret
L2161:


	# Test 2163
	# b32* =0 -1.23AAFCP23 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xcb23aafc	 #-0x1.4755f800p+23
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2162
	ret
L2162:


	# Test 2164
	# b32* =0 -1.000000P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2163
	ret
L2163:


	# Test 2165
	# b32* =0 -0.7FFFFFP-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2164
	ret
L2164:


	# Test 2166
	# b32* =0 -0.216233P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80216233	 #-0x1.0b119800p-128
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2165
	ret
L2165:


	# Test 2167
	# b32* =0 -0.000001P-126 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2166
	ret
L2166:


	# Test 2168
	# b32* =0 -1.000000P0 -Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2167
	ret
L2167:


	# Test 2169
	# b32* =0 -Zero -Inf -> q i


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2168
	ret
L2168:


	# Test 2170
	# b32* =0 +Zero -Inf -> q i


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2169
	ret
L2169:


	# Test 2171
	# b32* =0 +1.000000P0 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2170
	ret
L2170:


	# Test 2172
	# b32* =0 +0.000001P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2171
	ret
L2171:


	# Test 2173
	# b32* =0 +0.5A22A3P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x5a22a3	 #0x1.688a8c00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2172
	ret
L2172:


	# Test 2174
	# b32* =0 +0.7FFFFFP-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2173
	ret
L2173:


	# Test 2175
	# b32* =0 +1.000000P-126 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2174
	ret
L2174:


	# Test 2176
	# b32* =0 +1.57D9DAP-120 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x3d7d9da	 #0x1.afb3b400p-120
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2175
	ret
L2175:


	# Test 2177
	# b32* =0 +1.7FFFFFP127 -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2176
	ret
L2176:


	# Test 2178
	# b32* =0 +Inf -Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2177
	ret
L2177:


	# Test 2179
	# b32* =0 q -Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2178
	ret
L2178:


	# Test 2180
	# b32* =0 q -Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2179
	ret
L2179:


	# Test 2181
	# b32* =0 -Inf -1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2180
	ret
L2180:


	# Test 2182
	# b32* =0 -1.7FFFFFP127 -1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2181
	ret
L2181:


	# Test 2183
	# b32* =0 -1.724FB7P-91 -1.7FFFFFP127 -> +1.724FB6P37 x


	addi a1, a1, 1
	li t0, 0x92724fb7	 #-0x1.e49f6e00p-91
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x52724fb6	 #0x1.e49f6c00p+37
	beq t0, a0, L2182
	ret
L2182:


	# Test 2184
	# b32* =0 -1.000000P-126 -1.7FFFFFP127 -> +1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407fffff	 #0x1.fffffe00p+1
	beq t0, a0, L2183
	ret
L2183:


	# Test 2185
	# b32* =0 -0.7FFFFFP-126 -1.7FFFFFP127 -> +1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407ffffd	 #0x1.fffffa00p+1
	beq t0, a0, L2184
	ret
L2184:


	# Test 2186
	# b32* =0 -0.1F86EEP-126 -1.7FFFFFP127 -> +1.7C376FP-1 x


	addi a1, a1, 1
	li t0, 0x801f86ee	 #-0x1.f86ee000p-129
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f7c376f	 #0x1.f86ede00p-1
	beq t0, a0, L2185
	ret
L2185:


	# Test 2187
	# b32* =0 -0.000001P-126 -1.7FFFFFP127 -> +1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34ffffff	 #0x1.fffffe00p-22
	beq t0, a0, L2186
	ret
L2186:


	# Test 2188
	# b32* =0 -1.000000P0 -1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L2187
	ret
L2187:


	# Test 2189
	# b32* =0 -Zero -1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2188
	ret
L2188:


	# Test 2190
	# b32* =0 +Zero -1.7FFFFFP127 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2189
	ret
L2189:


	# Test 2191
	# b32* =0 +1.000000P0 -1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L2190
	ret
L2190:


	# Test 2192
	# b32* =0 +0.000001P-126 -1.7FFFFFP127 -> -1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4ffffff	 #-0x1.fffffe00p-22
	beq t0, a0, L2191
	ret
L2191:


	# Test 2193
	# b32* =0 +0.28C75EP-126 -1.7FFFFFP127 -> -1.231D77P0 x


	addi a1, a1, 1
	li t0, 0x28c75e	 #0x1.463af000p-128
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbfa31d77	 #-0x1.463aee00p+0
	beq t0, a0, L2192
	ret
L2192:


	# Test 2194
	# b32* =0 +0.7FFFFFP-126 -1.7FFFFFP127 -> -1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07ffffd	 #-0x1.fffffa00p+1
	beq t0, a0, L2193
	ret
L2193:


	# Test 2195
	# b32* =0 +1.000000P-126 -1.7FFFFFP127 -> -1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07fffff	 #-0x1.fffffe00p+1
	beq t0, a0, L2194
	ret
L2194:


	# Test 2196
	# b32* =0 +1.26FE95P-113 -1.7FFFFFP127 -> -1.26FE94P15 x


	addi a1, a1, 1
	li t0, 0x726fe95	 #0x1.4dfd2a00p-113
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc726fe94	 #-0x1.4dfd2800p+15
	beq t0, a0, L2195
	ret
L2195:


	# Test 2197
	# b32* =0 +1.7FFFFFP127 -1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2196
	ret
L2196:


	# Test 2198
	# b32* =0 +Inf -1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2197
	ret
L2197:


	# Test 2199
	# b32* =0 q -1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2198
	ret
L2198:


	# Test 2200
	# b32* =0 q -1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2199
	ret
L2199:


	# Test 2201
	# b32* =0 -Inf -1.273FEBP103 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xf3273feb	 #-0x1.4e7fd600p+103
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2200
	ret
L2200:


	# Test 2202
	# b32* =0 -1.7FFFFFP127 -1.7A8B99P111 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xf77a8b99	 #-0x1.f5173200p+111
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2201
	ret
L2201:


	# Test 2203
	# b32* =0 -1.40B473P-12 -1.005A25P126 -> +1.413C2AP114 x


	addi a1, a1, 1
	li t0, 0xb9c0b473	 #-0x1.8168e600p-12
	fmv.s.x f1, t0
	li t0, 0xfe805a25	 #-0x1.00b44a00p+126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x78c13c2a	 #0x1.82785400p+114
	beq t0, a0, L2202
	ret
L2202:


	# Test 2204
	# b32* =0 -1.000000P-126 -1.7C4DBCP-37 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xad7c4dbc	 #-0x1.f89b7800p-37
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2203
	ret
L2203:


	# Test 2205
	# b32* =0 -0.7FFFFFP-126 -1.07ED24P33 -> +1.07ED23P-93 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xd007ed24	 #-0x1.0fda4800p+33
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1107ed23	 #0x1.0fda4600p-93
	beq t0, a0, L2204
	ret
L2204:


	# Test 2206
	# b32* =0 -0.52E376P-126 -1.1856F6P-36 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x8052e376	 #-0x1.4b8dd800p-127
	fmv.s.x f1, t0
	li t0, 0xad9856f6	 #-0x1.30adec00p-36
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2205
	ret
L2205:


	# Test 2207
	# b32* =0 -0.000001P-126 -1.519B8DP-16 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xb7d19b8d	 #-0x1.a3371a00p-16
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2206
	ret
L2206:


	# Test 2208
	# b32* =0 -1.000000P0 -1.5CBAF5P-11 -> +1.5CBAF5P-11 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xba5cbaf5	 #-0x1.b975ea00p-11
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3a5cbaf5	 #0x1.b975ea00p-11
	beq t0, a0, L2207
	ret
L2207:


	# Test 2209
	# b32* =0 -Zero -1.685A5CP-70 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x9ce85a5c	 #-0x1.d0b4b800p-70
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2208
	ret
L2208:


	# Test 2210
	# b32* =0 +Zero -1.5E7D18P65 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xe05e7d18	 #-0x1.bcfa3000p+65
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2209
	ret
L2209:


	# Test 2211
	# b32* =0 +1.000000P0 -1.3248C6P10 -> -1.3248C6P10 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xc4b248c6	 #-0x1.64918c00p+10
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc4b248c6	 #-0x1.64918c00p+10
	beq t0, a0, L2210
	ret
L2210:


	# Test 2212
	# b32* =0 +0.000001P-126 -1.3D282EP79 -> -1.3D282EP-70 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xe73d282e	 #-0x1.7a505c00p+79
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff9cbd282e	 #-0x1.7a505c00p-70
	beq t0, a0, L2211
	ret
L2211:


	# Test 2213
	# b32* =0 +0.4C8432P-126 -1.6A1F2AP-48 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x4c8432	 #0x1.3210c800p-127
	fmv.s.x f1, t0
	li t0, 0xa7ea1f2a	 #-0x1.d43e5400p-48
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2212
	ret
L2212:


	# Test 2214
	# b32* =0 +0.7FFFFFP-126 -1.3EAA51P-125 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x813eaa51	 #-0x1.7d54a200p-125
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2213
	ret
L2213:


	# Test 2215
	# b32* =0 +1.000000P-126 -1.5EC664P30 -> -1.5EC664P-96 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xcedec664	 #-0x1.bd8cc800p+30
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8fdec664	 #-0x1.bd8cc800p-96
	beq t0, a0, L2214
	ret
L2214:


	# Test 2216
	# b32* =0 +1.600B38P110 -1.49A3C4P4 -> -1.307822P115 x


	addi a1, a1, 1
	li t0, 0x76e00b38	 #0x1.c0167000p+110
	fmv.s.x f1, t0
	li t0, 0xc1c9a3c4	 #-0x1.93478800p+4
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff9307822	 #-0x1.60f04400p+115
	beq t0, a0, L2215
	ret
L2215:


	# Test 2217
	# b32* =0 +1.7FFFFFP127 -1.28F4CEP110 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xf6a8f4ce	 #-0x1.51e99c00p+110
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2216
	ret
L2216:


	# Test 2218
	# b32* =0 +Inf -1.345435P-125 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x81345435	 #-0x1.68a86a00p-125
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2217
	ret
L2217:


	# Test 2219
	# b32* =0 q -1.61E095P-2 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbee1e095	 #-0x1.c3c12a00p-2
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2218
	ret
L2218:


	# Test 2220
	# b32* =0 q -1.776559P-13 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xb9776559	 #-0x1.eecab200p-13
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2219
	ret
L2219:


	# Test 2221
	# b32* =0 -Inf -1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2220
	ret
L2220:


	# Test 2222
	# b32* =0 -1.7FFFFFP127 -1.000000P-126 -> +1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407fffff	 #0x1.fffffe00p+1
	beq t0, a0, L2221
	ret
L2221:


	# Test 2223
	# b32* =0 -1.0FD92EP-29 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0xb10fd92e	 #-0x1.1fb25c00p-29
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2222
	ret
L2222:


	# Test 2224
	# b32* =0 -1.000000P-126 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2223
	ret
L2223:


	# Test 2225
	# b32* =0 -0.7FFFFFP-126 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2224
	ret
L2224:


	# Test 2226
	# b32* =0 -0.214832P-126 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80214832	 #-0x1.0a419000p-128
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2225
	ret
L2225:


	# Test 2227
	# b32* =0 -0.000001P-126 -1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2226
	ret
L2226:


	# Test 2228
	# b32* =0 -1.000000P0 -1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L2227
	ret
L2227:


	# Test 2229
	# b32* =0 -Zero -1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2228
	ret
L2228:


	# Test 2230
	# b32* =0 +Zero -1.000000P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2229
	ret
L2229:


	# Test 2231
	# b32* =0 +1.000000P0 -1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L2230
	ret
L2230:


	# Test 2232
	# b32* =0 +0.000001P-126 -1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2231
	ret
L2231:


	# Test 2233
	# b32* =0 +0.3BE8EDP-126 -1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x3be8ed	 #0x1.df476800p-128
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2232
	ret
L2232:


	# Test 2234
	# b32* =0 +0.7FFFFFP-126 -1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2233
	ret
L2233:


	# Test 2235
	# b32* =0 +1.000000P-126 -1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2234
	ret
L2234:


	# Test 2236
	# b32* =0 +1.587FD8P33 -1.000000P-126 -> -1.587FD8P-93 


	addi a1, a1, 1
	li t0, 0x50587fd8	 #0x1.b0ffb000p+33
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff91587fd8	 #-0x1.b0ffb000p-93
	beq t0, a0, L2235
	ret
L2235:


	# Test 2237
	# b32* =0 +1.7FFFFFP127 -1.000000P-126 -> -1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07fffff	 #-0x1.fffffe00p+1
	beq t0, a0, L2236
	ret
L2236:


	# Test 2238
	# b32* =0 +Inf -1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2237
	ret
L2237:


	# Test 2239
	# b32* =0 q -1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2238
	ret
L2238:


	# Test 2240
	# b32* =0 q -1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2239
	ret
L2239:


	# Test 2241
	# b32* =0 -Inf -0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2240
	ret
L2240:


	# Test 2242
	# b32* =0 -1.7FFFFFP127 -0.7FFFFFP-126 -> +1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407ffffd	 #0x1.fffffa00p+1
	beq t0, a0, L2241
	ret
L2241:


	# Test 2243
	# b32* =0 -1.739602P16 -0.7FFFFFP-126 -> +1.739600P-110 x


	addi a1, a1, 1
	li t0, 0xc7f39602	 #-0x1.e72c0400p+16
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x8f39600	 #0x1.e72c0000p-110
	beq t0, a0, L2242
	ret
L2242:


	# Test 2244
	# b32* =0 -1.000000P-126 -0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2243
	ret
L2243:


	# Test 2245
	# b32* =0 -0.7FFFFFP-126 -0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2244
	ret
L2244:


	# Test 2246
	# b32* =0 -0.3AD4D4P-126 -0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x803ad4d4	 #-0x1.d6a6a000p-128
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2245
	ret
L2245:


	# Test 2247
	# b32* =0 -0.000001P-126 -0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2246
	ret
L2246:


	# Test 2248
	# b32* =0 -1.000000P0 -0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L2247
	ret
L2247:


	# Test 2249
	# b32* =0 -Zero -0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2248
	ret
L2248:


	# Test 2250
	# b32* =0 +Zero -0.7FFFFFP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2249
	ret
L2249:


	# Test 2251
	# b32* =0 +1.000000P0 -0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L2250
	ret
L2250:


	# Test 2252
	# b32* =0 +0.000001P-126 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2251
	ret
L2251:


	# Test 2253
	# b32* =0 +0.357590P-126 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x357590	 #0x1.abac8000p-128
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2252
	ret
L2252:


	# Test 2254
	# b32* =0 +0.7FFFFFP-126 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2253
	ret
L2253:


	# Test 2255
	# b32* =0 +1.000000P-126 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2254
	ret
L2254:


	# Test 2256
	# b32* =0 +1.26E494P-48 -0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x27a6e494	 #0x1.4dc92800p-48
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2255
	ret
L2255:


	# Test 2257
	# b32* =0 +1.7FFFFFP127 -0.7FFFFFP-126 -> -1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07ffffd	 #-0x1.fffffa00p+1
	beq t0, a0, L2256
	ret
L2256:


	# Test 2258
	# b32* =0 +Inf -0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2257
	ret
L2257:


	# Test 2259
	# b32* =0 q -0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2258
	ret
L2258:


	# Test 2260
	# b32* =0 q -0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2259
	ret
L2259:


	# Test 2261
	# b32* =0 -Inf -0.73C2D7P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x8073c2d7	 #-0x1.cf0b5c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2260
	ret
L2260:


	# Test 2262
	# b32* =0 -1.7FFFFFP127 -0.03623FP-126 -> +1.588FBFP-4 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x8003623f	 #-0x1.b11f8000p-132
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3dd88fbf	 #0x1.b11f7e00p-4
	beq t0, a0, L2261
	ret
L2261:


	# Test 2263
	# b32* =0 -1.2D22A4P96 -0.163A72P-126 -> +1.708811P-33 x


	addi a1, a1, 1
	li t0, 0xefad22a4	 #-0x1.5a454800p+96
	fmv.s.x f1, t0
	li t0, 0x80163a72	 #-0x1.63a72000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2f708811	 #0x1.e1102200p-33
	beq t0, a0, L2262
	ret
L2262:


	# Test 2264
	# b32* =0 -1.000000P-126 -0.0110A8P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800110a8	 #-0x1.10a80000p-133
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2263
	ret
L2263:


	# Test 2265
	# b32* =0 -0.7FFFFFP-126 -0.347010P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80347010	 #-0x1.a3808000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2264
	ret
L2264:


	# Test 2266
	# b32* =0 -0.3E91A8P-126 -0.1E7743P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x803e91a8	 #-0x1.f48d4000p-128
	fmv.s.x f1, t0
	li t0, 0x801e7743	 #-0x1.e7743000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2265
	ret
L2265:


	# Test 2267
	# b32* =0 -0.000001P-126 -0.55F233P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x8055f233	 #-0x1.57c8cc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2266
	ret
L2266:


	# Test 2268
	# b32* =0 -1.000000P0 -0.69BDE1P-126 -> +0.69BDE1P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8069bde1	 #-0x1.a6f78400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x69bde1	 #0x1.a6f78400p-127
	beq t0, a0, L2267
	ret
L2267:


	# Test 2269
	# b32* =0 -Zero -0.349D49P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80349d49	 #-0x1.a4ea4800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2268
	ret
L2268:


	# Test 2270
	# b32* =0 +Zero -0.54F95BP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8054f95b	 #-0x1.53e56c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2269
	ret
L2269:


	# Test 2271
	# b32* =0 +1.000000P0 -0.2B5C17P-126 -> -0.2B5C17P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x802b5c17	 #-0x1.5ae0b800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff802b5c17	 #-0x1.5ae0b800p-128
	beq t0, a0, L2270
	ret
L2270:


	# Test 2272
	# b32* =0 +0.000001P-126 -0.5EE7C5P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x805ee7c5	 #-0x1.7b9f1400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2271
	ret
L2271:


	# Test 2273
	# b32* =0 +0.139A4BP-126 -0.2B96B9P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x139a4b	 #0x1.39a4b000p-129
	fmv.s.x f1, t0
	li t0, 0x802b96b9	 #-0x1.5cb5c800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2272
	ret
L2272:


	# Test 2274
	# b32* =0 +0.7FFFFFP-126 -0.60E9E9P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x8060e9e9	 #-0x1.83a7a400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2273
	ret
L2273:


	# Test 2275
	# b32* =0 +1.000000P-126 -0.2BC950P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x802bc950	 #-0x1.5e4a8000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2274
	ret
L2274:


	# Test 2276
	# b32* =0 +1.75094FP-2 -0.23D38AP-126 -> -0.112560P-126 xu


	addi a1, a1, 1
	li t0, 0x3ef5094f	 #0x1.ea129e00p-2
	fmv.s.x f1, t0
	li t0, 0x8023d38a	 #-0x1.1e9c5000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80112560	 #-0x1.12560000p-129
	beq t0, a0, L2275
	ret
L2275:


	# Test 2277
	# b32* =0 +1.7FFFFFP127 -0.2AB465P-126 -> -1.2AD193P0 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x802ab465	 #-0x1.55a32800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbfaad193	 #-0x1.55a32600p+0
	beq t0, a0, L2276
	ret
L2276:


	# Test 2278
	# b32* =0 +Inf -0.409721P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80409721	 #-0x1.025c8400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2277
	ret
L2277:


	# Test 2279
	# b32* =0 q -0.2BA9A1P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x802ba9a1	 #-0x1.5d4d0800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2278
	ret
L2278:


	# Test 2280
	# b32* =0 q -0.4D45A7P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x804d45a7	 #-0x1.35169c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2279
	ret
L2279:


	# Test 2281
	# b32* =0 -Inf -0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2280
	ret
L2280:


	# Test 2282
	# b32* =0 -1.7FFFFFP127 -0.000001P-126 -> +1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34ffffff	 #0x1.fffffe00p-22
	beq t0, a0, L2281
	ret
L2281:


	# Test 2283
	# b32* =0 -1.7BC760P78 -0.000001P-126 -> +1.7BC760P-71 


	addi a1, a1, 1
	li t0, 0xe6fbc760	 #-0x1.f78ec000p+78
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1c7bc760	 #0x1.f78ec000p-71
	beq t0, a0, L2282
	ret
L2282:


	# Test 2284
	# b32* =0 -1.000000P-126 -0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2283
	ret
L2283:


	# Test 2285
	# b32* =0 -0.7FFFFFP-126 -0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2284
	ret
L2284:


	# Test 2286
	# b32* =0 -0.2DB663P-126 -0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x802db663	 #-0x1.6db31800p-128
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2285
	ret
L2285:


	# Test 2287
	# b32* =0 -0.000001P-126 -0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2286
	ret
L2286:


	# Test 2288
	# b32* =0 -1.000000P0 -0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L2287
	ret
L2287:


	# Test 2289
	# b32* =0 -Zero -0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2288
	ret
L2288:


	# Test 2290
	# b32* =0 +Zero -0.000001P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2289
	ret
L2289:


	# Test 2291
	# b32* =0 +1.000000P0 -0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L2290
	ret
L2290:


	# Test 2292
	# b32* =0 +0.000001P-126 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2291
	ret
L2291:


	# Test 2293
	# b32* =0 +0.123F07P-126 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x123f07	 #0x1.23f07000p-129
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2292
	ret
L2292:


	# Test 2294
	# b32* =0 +0.7FFFFFP-126 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2293
	ret
L2293:


	# Test 2295
	# b32* =0 +1.000000P-126 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2294
	ret
L2294:


	# Test 2296
	# b32* =0 +1.442E0AP-19 -0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x36442e0a	 #0x1.885c1400p-19
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2295
	ret
L2295:


	# Test 2297
	# b32* =0 +1.7FFFFFP127 -0.000001P-126 -> -1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4ffffff	 #-0x1.fffffe00p-22
	beq t0, a0, L2296
	ret
L2296:


	# Test 2298
	# b32* =0 +Inf -0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2297
	ret
L2297:


	# Test 2299
	# b32* =0 q -0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2298
	ret
L2298:


	# Test 2300
	# b32* =0 q -0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2299
	ret
L2299:


	# Test 2301
	# b32* =0 -Inf -1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2300
	ret
L2300:


	# Test 2302
	# b32* =0 -1.7FFFFFP127 -1.000000P0 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L2301
	ret
L2301:


	# Test 2303
	# b32* =0 -1.5EE3E8P-94 -1.000000P0 -> +1.5EE3E8P-94 


	addi a1, a1, 1
	li t0, 0x90dee3e8	 #-0x1.bdc7d000p-94
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x10dee3e8	 #0x1.bdc7d000p-94
	beq t0, a0, L2302
	ret
L2302:


	# Test 2304
	# b32* =0 -1.000000P-126 -1.000000P0 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L2303
	ret
L2303:


	# Test 2305
	# b32* =0 -0.7FFFFFP-126 -1.000000P0 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L2304
	ret
L2304:


	# Test 2306
	# b32* =0 -0.1C5B1FP-126 -1.000000P0 -> +0.1C5B1FP-126 


	addi a1, a1, 1
	li t0, 0x801c5b1f	 #-0x1.c5b1f000p-129
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1c5b1f	 #0x1.c5b1f000p-129
	beq t0, a0, L2305
	ret
L2305:


	# Test 2307
	# b32* =0 -0.000001P-126 -1.000000P0 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L2306
	ret
L2306:


	# Test 2308
	# b32* =0 -1.000000P0 -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2307
	ret
L2307:


	# Test 2309
	# b32* =0 -Zero -1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2308
	ret
L2308:


	# Test 2310
	# b32* =0 +Zero -1.000000P0 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2309
	ret
L2309:


	# Test 2311
	# b32* =0 +1.000000P0 -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2310
	ret
L2310:


	# Test 2312
	# b32* =0 +0.000001P-126 -1.000000P0 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L2311
	ret
L2311:


	# Test 2313
	# b32* =0 +0.167BDBP-126 -1.000000P0 -> -0.167BDBP-126 


	addi a1, a1, 1
	li t0, 0x167bdb	 #0x1.67bdb000p-129
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80167bdb	 #-0x1.67bdb000p-129
	beq t0, a0, L2312
	ret
L2312:


	# Test 2314
	# b32* =0 +0.7FFFFFP-126 -1.000000P0 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L2313
	ret
L2313:


	# Test 2315
	# b32* =0 +1.000000P-126 -1.000000P0 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L2314
	ret
L2314:


	# Test 2316
	# b32* =0 +1.12D2C6P92 -1.000000P0 -> -1.12D2C6P92 


	addi a1, a1, 1
	li t0, 0x6d92d2c6	 #0x1.25a58c00p+92
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffed92d2c6	 #-0x1.25a58c00p+92
	beq t0, a0, L2315
	ret
L2315:


	# Test 2317
	# b32* =0 +1.7FFFFFP127 -1.000000P0 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L2316
	ret
L2316:


	# Test 2318
	# b32* =0 +Inf -1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2317
	ret
L2317:


	# Test 2319
	# b32* =0 q -1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2318
	ret
L2318:


	# Test 2320
	# b32* =0 q -1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2319
	ret
L2319:


	# Test 2321
	# b32* =0 -Inf -Zero -> q i


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2320
	ret
L2320:


	# Test 2322
	# b32* =0 -1.7FFFFFP127 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2321
	ret
L2321:


	# Test 2323
	# b32* =0 -1.4220BCP-16 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0xb7c220bc	 #-0x1.84417800p-16
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2322
	ret
L2322:


	# Test 2324
	# b32* =0 -1.000000P-126 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2323
	ret
L2323:


	# Test 2325
	# b32* =0 -0.7FFFFFP-126 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2324
	ret
L2324:


	# Test 2326
	# b32* =0 -0.0ABFDAP-126 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x800abfda	 #-0x1.57fb4000p-130
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2325
	ret
L2325:


	# Test 2327
	# b32* =0 -0.000001P-126 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2326
	ret
L2326:


	# Test 2328
	# b32* =0 -1.000000P0 -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2327
	ret
L2327:


	# Test 2329
	# b32* =0 -Zero -Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2328
	ret
L2328:


	# Test 2330
	# b32* =0 +Zero -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2329
	ret
L2329:


	# Test 2331
	# b32* =0 +1.000000P0 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2330
	ret
L2330:


	# Test 2332
	# b32* =0 +0.000001P-126 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2331
	ret
L2331:


	# Test 2333
	# b32* =0 +0.391863P-126 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x391863	 #0x1.c8c31800p-128
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2332
	ret
L2332:


	# Test 2334
	# b32* =0 +0.7FFFFFP-126 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2333
	ret
L2333:


	# Test 2335
	# b32* =0 +1.000000P-126 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2334
	ret
L2334:


	# Test 2336
	# b32* =0 +1.768F99P-55 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x24768f99	 #0x1.ed1f3200p-55
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2335
	ret
L2335:


	# Test 2337
	# b32* =0 +1.7FFFFFP127 -Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2336
	ret
L2336:


	# Test 2338
	# b32* =0 +Inf -Zero -> q i


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2337
	ret
L2337:


	# Test 2339
	# b32* =0 q -Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2338
	ret
L2338:


	# Test 2340
	# b32* =0 q -Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2339
	ret
L2339:


	# Test 2341
	# b32* =0 -Inf +Zero -> q i


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2340
	ret
L2340:


	# Test 2342
	# b32* =0 -1.7FFFFFP127 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2341
	ret
L2341:


	# Test 2343
	# b32* =0 -1.114577P-33 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0xaf114577	 #-0x1.228aee00p-33
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2342
	ret
L2342:


	# Test 2344
	# b32* =0 -1.000000P-126 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2343
	ret
L2343:


	# Test 2345
	# b32* =0 -0.7FFFFFP-126 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2344
	ret
L2344:


	# Test 2346
	# b32* =0 -0.1EFCAEP-126 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x801efcae	 #-0x1.efcae000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2345
	ret
L2345:


	# Test 2347
	# b32* =0 -0.000001P-126 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2346
	ret
L2346:


	# Test 2348
	# b32* =0 -1.000000P0 +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2347
	ret
L2347:


	# Test 2349
	# b32* =0 -Zero +Zero -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2348
	ret
L2348:


	# Test 2350
	# b32* =0 +Zero +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2349
	ret
L2349:


	# Test 2351
	# b32* =0 +1.000000P0 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2350
	ret
L2350:


	# Test 2352
	# b32* =0 +0.000001P-126 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2351
	ret
L2351:


	# Test 2353
	# b32* =0 +0.27BD1EP-126 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x27bd1e	 #0x1.3de8f000p-128
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2352
	ret
L2352:


	# Test 2354
	# b32* =0 +0.7FFFFFP-126 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2353
	ret
L2353:


	# Test 2355
	# b32* =0 +1.000000P-126 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2354
	ret
L2354:


	# Test 2356
	# b32* =0 +1.45F455P56 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x5bc5f455	 #0x1.8be8aa00p+56
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2355
	ret
L2355:


	# Test 2357
	# b32* =0 +1.7FFFFFP127 +Zero -> +Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2356
	ret
L2356:


	# Test 2358
	# b32* =0 +Inf +Zero -> q i


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2357
	ret
L2357:


	# Test 2359
	# b32* =0 q +Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2358
	ret
L2358:


	# Test 2360
	# b32* =0 q +Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2359
	ret
L2359:


	# Test 2361
	# b32* =0 -Inf +1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2360
	ret
L2360:


	# Test 2362
	# b32* =0 -1.7FFFFFP127 +1.000000P0 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L2361
	ret
L2361:


	# Test 2363
	# b32* =0 -1.5FEA32P-51 +1.000000P0 -> -1.5FEA32P-51 


	addi a1, a1, 1
	li t0, 0xa65fea32	 #-0x1.bfd46400p-51
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffa65fea32	 #-0x1.bfd46400p-51
	beq t0, a0, L2362
	ret
L2362:


	# Test 2364
	# b32* =0 -1.000000P-126 +1.000000P0 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L2363
	ret
L2363:


	# Test 2365
	# b32* =0 -0.7FFFFFP-126 +1.000000P0 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L2364
	ret
L2364:


	# Test 2366
	# b32* =0 -0.0DA169P-126 +1.000000P0 -> -0.0DA169P-126 


	addi a1, a1, 1
	li t0, 0x800da169	 #-0x1.b42d2000p-130
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff800da169	 #-0x1.b42d2000p-130
	beq t0, a0, L2365
	ret
L2365:


	# Test 2367
	# b32* =0 -0.000001P-126 +1.000000P0 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L2366
	ret
L2366:


	# Test 2368
	# b32* =0 -1.000000P0 +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2367
	ret
L2367:


	# Test 2369
	# b32* =0 -Zero +1.000000P0 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2368
	ret
L2368:


	# Test 2370
	# b32* =0 +Zero +1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2369
	ret
L2369:


	# Test 2371
	# b32* =0 +1.000000P0 +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2370
	ret
L2370:


	# Test 2372
	# b32* =0 +0.000001P-126 +1.000000P0 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L2371
	ret
L2371:


	# Test 2373
	# b32* =0 +0.36E1D9P-126 +1.000000P0 -> +0.36E1D9P-126 


	addi a1, a1, 1
	li t0, 0x36e1d9	 #0x1.b70ec800p-128
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x36e1d9	 #0x1.b70ec800p-128
	beq t0, a0, L2372
	ret
L2372:


	# Test 2374
	# b32* =0 +0.7FFFFFP-126 +1.000000P0 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L2373
	ret
L2373:


	# Test 2375
	# b32* =0 +1.000000P-126 +1.000000P0 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L2374
	ret
L2374:


	# Test 2376
	# b32* =0 +1.141910P39 +1.000000P0 -> +1.141910P39 


	addi a1, a1, 1
	li t0, 0x53141910	 #0x1.28322000p+39
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x53141910	 #0x1.28322000p+39
	beq t0, a0, L2375
	ret
L2375:


	# Test 2377
	# b32* =0 +1.7FFFFFP127 +1.000000P0 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L2376
	ret
L2376:


	# Test 2378
	# b32* =0 +Inf +1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2377
	ret
L2377:


	# Test 2379
	# b32* =0 q +1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2378
	ret
L2378:


	# Test 2380
	# b32* =0 q +1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2379
	ret
L2379:


	# Test 2381
	# b32* =0 -Inf +0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2380
	ret
L2380:


	# Test 2382
	# b32* =0 -1.7FFFFFP127 +0.000001P-126 -> -1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4ffffff	 #-0x1.fffffe00p-22
	beq t0, a0, L2381
	ret
L2381:


	# Test 2383
	# b32* =0 -1.2E8EEDP-4 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0xbdae8eed	 #-0x1.5d1dda00p-4
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2382
	ret
L2382:


	# Test 2384
	# b32* =0 -1.000000P-126 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2383
	ret
L2383:


	# Test 2385
	# b32* =0 -0.7FFFFFP-126 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2384
	ret
L2384:


	# Test 2386
	# b32* =0 -0.024624P-126 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80024624	 #-0x1.23120000p-132
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2385
	ret
L2385:


	# Test 2387
	# b32* =0 -0.000001P-126 +0.000001P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2386
	ret
L2386:


	# Test 2388
	# b32* =0 -1.000000P0 +0.000001P-126 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L2387
	ret
L2387:


	# Test 2389
	# b32* =0 -Zero +0.000001P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2388
	ret
L2388:


	# Test 2390
	# b32* =0 +Zero +0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2389
	ret
L2389:


	# Test 2391
	# b32* =0 +1.000000P0 +0.000001P-126 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L2390
	ret
L2390:


	# Test 2392
	# b32* =0 +0.000001P-126 +0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2391
	ret
L2391:


	# Test 2393
	# b32* =0 +0.3A1EADP-126 +0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x3a1ead	 #0x1.d0f56800p-128
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2392
	ret
L2392:


	# Test 2394
	# b32* =0 +0.7FFFFFP-126 +0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2393
	ret
L2393:


	# Test 2395
	# b32* =0 +1.000000P-126 +0.000001P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2394
	ret
L2394:


	# Test 2396
	# b32* =0 +1.62BDCBP21 +0.000001P-126 -> +0.38AF73P-126 xu


	addi a1, a1, 1
	li t0, 0x4a62bdcb	 #0x1.c57b9600p+21
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x38af73	 #0x1.c57b9800p-128
	beq t0, a0, L2395
	ret
L2395:


	# Test 2397
	# b32* =0 +1.7FFFFFP127 +0.000001P-126 -> +1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34ffffff	 #0x1.fffffe00p-22
	beq t0, a0, L2396
	ret
L2396:


	# Test 2398
	# b32* =0 +Inf +0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2397
	ret
L2397:


	# Test 2399
	# b32* =0 q +0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2398
	ret
L2398:


	# Test 2400
	# b32* =0 q +0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2399
	ret
L2399:


	# Test 2401
	# b32* =0 -Inf +0.428538P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x428538	 #0x1.0a14e000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2400
	ret
L2400:


	# Test 2402
	# b32* =0 -1.7FFFFFP127 +0.2E24A0P-126 -> -1.38927FP0 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x2e24a0	 #0x1.71250000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbfb8927f	 #-0x1.7124fe00p+0
	beq t0, a0, L2401
	ret
L2401:


	# Test 2403
	# b32* =0 -1.7D73A9P42 +0.2D554AP-126 -> -1.338715P-85 x


	addi a1, a1, 1
	li t0, 0xd4fd73a9	 #-0x1.fae75200p+42
	fmv.s.x f1, t0
	li t0, 0x2d554a	 #0x1.6aaa5000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff95338715	 #-0x1.670e2a00p-85
	beq t0, a0, L2402
	ret
L2402:


	# Test 2404
	# b32* =0 -1.000000P-126 +0.24236FP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x24236f	 #0x1.211b7800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2403
	ret
L2403:


	# Test 2405
	# b32* =0 -0.7FFFFFP-126 +0.1A862AP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1a862a	 #0x1.a862a000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2404
	ret
L2404:


	# Test 2406
	# b32* =0 -0.7AEAE0P-126 +0.35EB61P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807aeae0	 #-0x1.ebab8000p-127
	fmv.s.x f1, t0
	li t0, 0x35eb61	 #0x1.af5b0800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2405
	ret
L2405:


	# Test 2407
	# b32* =0 -0.000001P-126 +0.79F140P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x79f140	 #0x1.e7c50000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2406
	ret
L2406:


	# Test 2408
	# b32* =0 -1.000000P0 +0.24D0A7P-126 -> -0.24D0A7P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x24d0a7	 #0x1.26853800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8024d0a7	 #-0x1.26853800p-128
	beq t0, a0, L2407
	ret
L2407:


	# Test 2409
	# b32* =0 -Zero +0.0B7363P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xb7363	 #0x1.6e6c6000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2408
	ret
L2408:


	# Test 2410
	# b32* =0 +Zero +0.2F3F11P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x2f3f11	 #0x1.79f88800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2409
	ret
L2409:


	# Test 2411
	# b32* =0 +1.000000P0 +0.5A5E79P-126 -> +0.5A5E79P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x5a5e79	 #0x1.6979e400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5a5e79	 #0x1.6979e400p-127
	beq t0, a0, L2410
	ret
L2410:


	# Test 2412
	# b32* =0 +0.000001P-126 +0.50C134P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x50c134	 #0x1.4304d000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2411
	ret
L2411:


	# Test 2413
	# b32* =0 +0.294368P-126 +0.571A4FP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x294368	 #0x1.4a1b4000p-128
	fmv.s.x f1, t0
	li t0, 0x571a4f	 #0x1.5c693c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2412
	ret
L2412:


	# Test 2414
	# b32* =0 +0.7FFFFFP-126 +0.66C003P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x66c003	 #0x1.9b000c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2413
	ret
L2413:


	# Test 2415
	# b32* =0 +1.000000P-126 +0.7ACBB1P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7acbb1	 #0x1.eb2ec400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2414
	ret
L2414:


	# Test 2416
	# b32* =0 +1.46FA9FP99 +0.5F7066P-126 -> +1.145CBCP-27 x


	addi a1, a1, 1
	li t0, 0x7146fa9f	 #0x1.8df53e00p+99
	fmv.s.x f1, t0
	li t0, 0x5f7066	 #0x1.7dc19800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x32145cbc	 #0x1.28b97800p-27
	beq t0, a0, L2415
	ret
L2415:


	# Test 2417
	# b32* =0 +1.7FFFFFP127 +0.50CA80P-126 -> +1.2194FFP1 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x50ca80	 #0x1.432a0000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x402194ff	 #0x1.4329fe00p+1
	beq t0, a0, L2416
	ret
L2416:


	# Test 2418
	# b32* =0 +Inf +0.3C69E7P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3c69e7	 #0x1.e34f3800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2417
	ret
L2417:


	# Test 2419
	# b32* =0 q +0.628478P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x628478	 #0x1.8a11e000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2418
	ret
L2418:


	# Test 2420
	# b32* =0 q +0.0CF9C4P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xcf9c4	 #0x1.9f388000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2419
	ret
L2419:


	# Test 2421
	# b32* =0 -Inf +0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2420
	ret
L2420:


	# Test 2422
	# b32* =0 -1.7FFFFFP127 +0.7FFFFFP-126 -> -1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07ffffd	 #-0x1.fffffa00p+1
	beq t0, a0, L2421
	ret
L2421:


	# Test 2423
	# b32* =0 -1.61307DP24 +0.7FFFFFP-126 -> -1.61307BP-102 x


	addi a1, a1, 1
	li t0, 0xcbe1307d	 #-0x1.c260fa00p+24
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8ce1307b	 #-0x1.c260f600p-102
	beq t0, a0, L2422
	ret
L2422:


	# Test 2424
	# b32* =0 -1.000000P-126 +0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2423
	ret
L2423:


	# Test 2425
	# b32* =0 -0.7FFFFFP-126 +0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2424
	ret
L2424:


	# Test 2426
	# b32* =0 -0.49CF9BP-126 +0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x8049cf9b	 #-0x1.273e6c00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2425
	ret
L2425:


	# Test 2427
	# b32* =0 -0.000001P-126 +0.7FFFFFP-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2426
	ret
L2426:


	# Test 2428
	# b32* =0 -1.000000P0 +0.7FFFFFP-126 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L2427
	ret
L2427:


	# Test 2429
	# b32* =0 -Zero +0.7FFFFFP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2428
	ret
L2428:


	# Test 2430
	# b32* =0 +Zero +0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2429
	ret
L2429:


	# Test 2431
	# b32* =0 +1.000000P0 +0.7FFFFFP-126 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L2430
	ret
L2430:


	# Test 2432
	# b32* =0 +0.000001P-126 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2431
	ret
L2431:


	# Test 2433
	# b32* =0 +0.37A824P-126 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x37a824	 #0x1.bd412000p-128
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2432
	ret
L2432:


	# Test 2434
	# b32* =0 +0.7FFFFFP-126 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2433
	ret
L2433:


	# Test 2435
	# b32* =0 +1.000000P-126 +0.7FFFFFP-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2434
	ret
L2434:


	# Test 2436
	# b32* =0 +1.159F5AP82 +0.7FFFFFP-126 -> +1.159F59P-44 x


	addi a1, a1, 1
	li t0, 0x68959f5a	 #0x1.2b3eb400p+82
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x29959f59	 #0x1.2b3eb200p-44
	beq t0, a0, L2435
	ret
L2435:


	# Test 2437
	# b32* =0 +1.7FFFFFP127 +0.7FFFFFP-126 -> +1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407ffffd	 #0x1.fffffa00p+1
	beq t0, a0, L2436
	ret
L2436:


	# Test 2438
	# b32* =0 +Inf +0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2437
	ret
L2437:


	# Test 2439
	# b32* =0 q +0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2438
	ret
L2438:


	# Test 2440
	# b32* =0 q +0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2439
	ret
L2439:


	# Test 2441
	# b32* =0 -Inf +1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2440
	ret
L2440:


	# Test 2442
	# b32* =0 -1.7FFFFFP127 +1.000000P-126 -> -1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07fffff	 #-0x1.fffffe00p+1
	beq t0, a0, L2441
	ret
L2441:


	# Test 2443
	# b32* =0 -1.2FD538P7 +1.000000P-126 -> -1.2FD538P-119 


	addi a1, a1, 1
	li t0, 0xc32fd538	 #-0x1.5faa7000p+7
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff842fd538	 #-0x1.5faa7000p-119
	beq t0, a0, L2442
	ret
L2442:


	# Test 2444
	# b32* =0 -1.000000P-126 +1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2443
	ret
L2443:


	# Test 2445
	# b32* =0 -0.7FFFFFP-126 +1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2444
	ret
L2444:


	# Test 2446
	# b32* =0 -0.6D8C6FP-126 +1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x806d8c6f	 #-0x1.b631bc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2445
	ret
L2445:


	# Test 2447
	# b32* =0 -0.000001P-126 +1.000000P-126 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2446
	ret
L2446:


	# Test 2448
	# b32* =0 -1.000000P0 +1.000000P-126 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L2447
	ret
L2447:


	# Test 2449
	# b32* =0 -Zero +1.000000P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2448
	ret
L2448:


	# Test 2450
	# b32* =0 +Zero +1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2449
	ret
L2449:


	# Test 2451
	# b32* =0 +1.000000P0 +1.000000P-126 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L2450
	ret
L2450:


	# Test 2452
	# b32* =0 +0.000001P-126 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2451
	ret
L2451:


	# Test 2453
	# b32* =0 +0.16CCDFP-126 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x16ccdf	 #0x1.6ccdf000p-129
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2452
	ret
L2452:


	# Test 2454
	# b32* =0 +0.7FFFFFP-126 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2453
	ret
L2453:


	# Test 2455
	# b32* =0 +1.000000P-126 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2454
	ret
L2454:


	# Test 2456
	# b32* =0 +1.640416P-64 +1.000000P-126 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1fe40416	 #0x1.c8082c00p-64
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2455
	ret
L2455:


	# Test 2457
	# b32* =0 +1.7FFFFFP127 +1.000000P-126 -> +1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407fffff	 #0x1.fffffe00p+1
	beq t0, a0, L2456
	ret
L2456:


	# Test 2458
	# b32* =0 +Inf +1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2457
	ret
L2457:


	# Test 2459
	# b32* =0 q +1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2458
	ret
L2458:


	# Test 2460
	# b32* =0 q +1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2459
	ret
L2459:


	# Test 2461
	# b32* =0 -Inf +1.5B988AP-45 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x295b988a	 #0x1.b7311400p-45
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2460
	ret
L2460:


	# Test 2462
	# b32* =0 -1.7FFFFFP127 +1.2FA437P-68 -> -1.2FA436P60 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1dafa437	 #0x1.5f486e00p-68
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffddafa436	 #-0x1.5f486c00p+60
	beq t0, a0, L2461
	ret
L2461:


	# Test 2463
	# b32* =0 -1.7EF9F3P53 +1.17DE55P-68 -> -1.1742E0P-14 x


	addi a1, a1, 1
	li t0, 0xda7ef9f3	 #-0x1.fdf3e600p+53
	fmv.s.x f1, t0
	li t0, 0x1d97de55	 #0x1.2fbcaa00p-68
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb89742e0	 #-0x1.2e85c000p-14
	beq t0, a0, L2462
	ret
L2462:


	# Test 2464
	# b32* =0 -1.000000P-126 +1.31665BP40 -> -1.31665BP-86 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x53b1665b	 #0x1.62ccb600p+40
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff94b1665b	 #-0x1.62ccb600p-86
	beq t0, a0, L2463
	ret
L2463:


	# Test 2465
	# b32* =0 -0.7FFFFFP-126 +1.3C05C2P-19 -> -0.000018P-126 xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x363c05c2	 #0x1.780b8400p-19
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000018	 #-0x1.80000000p-145
	beq t0, a0, L2464
	ret
L2464:


	# Test 2466
	# b32* =0 -0.1068F7P-126 +1.2F1B26P86 -> -1.3397E6P-43 x


	addi a1, a1, 1
	li t0, 0x801068f7	 #-0x1.068f7000p-129
	fmv.s.x f1, t0
	li t0, 0x6aaf1b26	 #0x1.5e364c00p+86
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffaa3397e6	 #-0x1.672fcc00p-43
	beq t0, a0, L2465
	ret
L2465:


	# Test 2467
	# b32* =0 -0.000001P-126 +1.06B42CP61 -> -1.06B42CP-88 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x5e06b42c	 #0x1.0d685800p+61
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff9386b42c	 #-0x1.0d685800p-88
	beq t0, a0, L2466
	ret
L2466:


	# Test 2468
	# b32* =0 -1.000000P0 +1.11D393P2 -> -1.11D393P2 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x4091d393	 #0x1.23a72600p+2
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc091d393	 #-0x1.23a72600p+2
	beq t0, a0, L2467
	ret
L2467:


	# Test 2469
	# b32* =0 -Zero +1.1CF2FBP103 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x731cf2fb	 #0x1.39e5f600p+103
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2468
	ret
L2468:


	# Test 2470
	# b32* =0 +Zero +1.1355B7P-18 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x369355b7	 #0x1.26ab6e00p-18
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2469
	ret
L2469:


	# Test 2471
	# b32* =0 +1.000000P0 +1.66A165P-74 -> +1.66A165P-74 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1ae6a165	 #0x1.cd42ca00p-74
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1ae6a165	 #0x1.cd42ca00p-74
	beq t0, a0, L2470
	ret
L2470:


	# Test 2472
	# b32* =0 +0.000001P-126 +1.06BD78P90 -> +1.06BD78P-59 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x6c86bd78	 #0x1.0d7af000p+90
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2206bd78	 #0x1.0d7af000p-59
	beq t0, a0, L2471
	ret
L2471:


	# Test 2473
	# b32* =0 +0.15719AP-126 +1.2CBA9CP24 -> +1.677F51P-105 x


	addi a1, a1, 1
	li t0, 0x15719a	 #0x1.5719a000p-129
	fmv.s.x f1, t0
	li t0, 0x4bacba9c	 #0x1.59753800p+24
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xb677f51	 #0x1.cefea200p-105
	beq t0, a0, L2472
	ret
L2472:


	# Test 2474
	# b32* =0 +0.7FFFFFP-126 +1.083F9BP-122 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x2883f9b	 #0x1.107f3600p-122
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2473
	ret
L2473:


	# Test 2475
	# b32* =0 +1.000000P-126 +1.139F02P-101 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xd139f02	 #0x1.273e0400p-101
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2474
	ret
L2474:


	# Test 2476
	# b32* =0 +1.1D90B8P-48 +1.6167F4P-66 -> +1.0ABC2DP-113 x


	addi a1, a1, 1
	li t0, 0x279d90b8	 #0x1.3b217000p-48
	fmv.s.x f1, t0
	li t0, 0x1ee167f4	 #0x1.c2cfe800p-66
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x70abc2d	 #0x1.15785a00p-113
	beq t0, a0, L2475
	ret
L2475:


	# Test 2477
	# b32* =0 +1.7FFFFFP127 +1.5DCD6CP-6 -> +1.5DCD6BP122 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3cddcd6c	 #0x1.bb9ad800p-6
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7cddcd6b	 #0x1.bb9ad600p+122
	beq t0, a0, L2476
	ret
L2476:


	# Test 2478
	# b32* =0 +Inf +1.68ACD4P-121 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x368acd4	 #0x1.d159a800p-121
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2477
	ret
L2477:


	# Test 2479
	# b32* =0 q +1.78A4C5P-8 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3bf8a4c5	 #0x1.f1498a00p-8
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2478
	ret
L2478:


	# Test 2480
	# b32* =0 q +1.0E698AP-124 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x18e698a	 #0x1.1cd31400p-124
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2479
	ret
L2479:


	# Test 2481
	# b32* =0 -Inf +1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2480
	ret
L2480:


	# Test 2482
	# b32* =0 -1.7FFFFFP127 +1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2481
	ret
L2481:


	# Test 2483
	# b32* =0 -1.4D1EAEP36 +1.7FFFFFP127 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xd1cd1eae	 #-0x1.9a3d5c00p+36
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2482
	ret
L2482:


	# Test 2484
	# b32* =0 -1.000000P-126 +1.7FFFFFP127 -> -1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07fffff	 #-0x1.fffffe00p+1
	beq t0, a0, L2483
	ret
L2483:


	# Test 2485
	# b32* =0 -0.7FFFFFP-126 +1.7FFFFFP127 -> -1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc07ffffd	 #-0x1.fffffa00p+1
	beq t0, a0, L2484
	ret
L2484:


	# Test 2486
	# b32* =0 -0.05F599P-126 +1.7FFFFFP127 -> -1.3EB31FP-3 x


	addi a1, a1, 1
	li t0, 0x8005f599	 #-0x1.7d664000p-131
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbe3eb31f	 #-0x1.7d663e00p-3
	beq t0, a0, L2485
	ret
L2485:


	# Test 2487
	# b32* =0 -0.000001P-126 +1.7FFFFFP127 -> -1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4ffffff	 #-0x1.fffffe00p-22
	beq t0, a0, L2486
	ret
L2486:


	# Test 2488
	# b32* =0 -1.000000P0 +1.7FFFFFP127 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L2487
	ret
L2487:


	# Test 2489
	# b32* =0 -Zero +1.7FFFFFP127 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2488
	ret
L2488:


	# Test 2490
	# b32* =0 +Zero +1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2489
	ret
L2489:


	# Test 2491
	# b32* =0 +1.000000P0 +1.7FFFFFP127 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L2490
	ret
L2490:


	# Test 2492
	# b32* =0 +0.000001P-126 +1.7FFFFFP127 -> +1.7FFFFFP-22 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34ffffff	 #0x1.fffffe00p-22
	beq t0, a0, L2491
	ret
L2491:


	# Test 2493
	# b32* =0 +0.041655P-126 +1.7FFFFFP127 -> +1.02CA9FP-3 x


	addi a1, a1, 1
	li t0, 0x41655	 #0x1.05954000p-131
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3e02ca9f	 #0x1.05953e00p-3
	beq t0, a0, L2492
	ret
L2492:


	# Test 2494
	# b32* =0 +0.7FFFFFP-126 +1.7FFFFFP127 -> +1.7FFFFDP1 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407ffffd	 #0x1.fffffa00p+1
	beq t0, a0, L2493
	ret
L2493:


	# Test 2495
	# b32* =0 +1.000000P-126 +1.7FFFFFP127 -> +1.7FFFFFP1 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x407fffff	 #0x1.fffffe00p+1
	beq t0, a0, L2494
	ret
L2494:


	# Test 2496
	# b32* =0 +1.006D40P36 +1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x51806d40	 #0x1.00da8000p+36
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2495
	ret
L2495:


	# Test 2497
	# b32* =0 +1.7FFFFFP127 +1.7FFFFFP127 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2496
	ret
L2496:


	# Test 2498
	# b32* =0 +Inf +1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2497
	ret
L2497:


	# Test 2499
	# b32* =0 q +1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2498
	ret
L2498:


	# Test 2500
	# b32* =0 q +1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2499
	ret
L2499:


	# Test 2501
	# b32* =0 -Inf +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2500
	ret
L2500:


	# Test 2502
	# b32* =0 -1.7FFFFFP127 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2501
	ret
L2501:


	# Test 2503
	# b32* =0 -1.1C036AP-45 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xa91c036a	 #-0x1.3806d400p-45
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2502
	ret
L2502:


	# Test 2504
	# b32* =0 -1.000000P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2503
	ret
L2503:


	# Test 2505
	# b32* =0 -0.7FFFFFP-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2504
	ret
L2504:


	# Test 2506
	# b32* =0 -0.589A55P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80589a55	 #-0x1.62695400p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2505
	ret
L2505:


	# Test 2507
	# b32* =0 -0.000001P-126 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2506
	ret
L2506:


	# Test 2508
	# b32* =0 -1.000000P0 +Inf -> -Inf 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2507
	ret
L2507:


	# Test 2509
	# b32* =0 -Zero +Inf -> q i


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2508
	ret
L2508:


	# Test 2510
	# b32* =0 +Zero +Inf -> q i


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2509
	ret
L2509:


	# Test 2511
	# b32* =0 +1.000000P0 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2510
	ret
L2510:


	# Test 2512
	# b32* =0 +0.000001P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2511
	ret
L2511:


	# Test 2513
	# b32* =0 +0.52BB11P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x52bb11	 #0x1.4aec4400p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2512
	ret
L2512:


	# Test 2514
	# b32* =0 +0.7FFFFFP-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2513
	ret
L2513:


	# Test 2515
	# b32* =0 +1.000000P-126 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2514
	ret
L2514:


	# Test 2516
	# b32* =0 +1.642A14P17 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x48642a14	 #0x1.c8542800p+17
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2515
	ret
L2515:


	# Test 2517
	# b32* =0 +1.7FFFFFP127 +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2516
	ret
L2516:


	# Test 2518
	# b32* =0 +Inf +Inf -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2517
	ret
L2517:


	# Test 2519
	# b32* =0 q +Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2518
	ret
L2518:


	# Test 2520
	# b32* =0 q +Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2519
	ret
L2519:


	# Test 2521
	# b32* =0 -Inf q -> q 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2520
	ret
L2520:


	# Test 2522
	# b32* =0 -1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2521
	ret
L2521:


	# Test 2523
	# b32* =0 -1.6AA825P-63 q -> q 


	addi a1, a1, 1
	li t0, 0xa06aa825	 #-0x1.d5504a00p-63
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2522
	ret
L2522:


	# Test 2524
	# b32* =0 -1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2523
	ret
L2523:


	# Test 2525
	# b32* =0 -0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2524
	ret
L2524:


	# Test 2526
	# b32* =0 -0.677F10P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80677f10	 #-0x1.9dfc4000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2525
	ret
L2525:


	# Test 2527
	# b32* =0 -0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2526
	ret
L2526:


	# Test 2528
	# b32* =0 -1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2527
	ret
L2527:


	# Test 2529
	# b32* =0 -Zero q -> q 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2528
	ret
L2528:


	# Test 2530
	# b32* =0 +Zero q -> q 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2529
	ret
L2529:


	# Test 2531
	# b32* =0 +1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2530
	ret
L2530:


	# Test 2532
	# b32* =0 +0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2531
	ret
L2531:


	# Test 2533
	# b32* =0 +0.611FCCP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x611fcc	 #0x1.847f3000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2532
	ret
L2532:


	# Test 2534
	# b32* =0 +0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2533
	ret
L2533:


	# Test 2535
	# b32* =0 +1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2534
	ret
L2534:


	# Test 2536
	# b32* =0 +1.334ECFP64 q -> q 


	addi a1, a1, 1
	li t0, 0x5fb34ecf	 #0x1.669d9e00p+64
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2535
	ret
L2535:


	# Test 2537
	# b32* =0 +1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2536
	ret
L2536:


	# Test 2538
	# b32* =0 +Inf q -> q 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2537
	ret
L2537:


	# Test 2539
	# b32* =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2538
	ret
L2538:


	# Test 2540
	# b32* =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2539
	ret
L2539:


	# Test 2541
	# b32* =0 -Inf q -> q 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2540
	ret
L2540:


	# Test 2542
	# b32* =0 -1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2541
	ret
L2541:


	# Test 2543
	# b32* =0 -1.394CE0P-16 q -> q 


	addi a1, a1, 1
	li t0, 0xb7b94ce0	 #-0x1.7299c000p-16
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2542
	ret
L2542:


	# Test 2544
	# b32* =0 -1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2543
	ret
L2543:


	# Test 2545
	# b32* =0 -0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2544
	ret
L2544:


	# Test 2546
	# b32* =0 -0.4B3BE4P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x804b3be4	 #-0x1.2cef9000p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2545
	ret
L2545:


	# Test 2547
	# b32* =0 -0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2546
	ret
L2546:


	# Test 2548
	# b32* =0 -1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2547
	ret
L2547:


	# Test 2549
	# b32* =0 -Zero q -> q 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2548
	ret
L2548:


	# Test 2550
	# b32* =0 +Zero q -> q 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2549
	ret
L2549:


	# Test 2551
	# b32* =0 +1.000000P0 q -> q 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2550
	ret
L2550:


	# Test 2552
	# b32* =0 +0.000001P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2551
	ret
L2551:


	# Test 2553
	# b32* =0 +0.704487P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x704487	 #0x1.c1121c00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2552
	ret
L2552:


	# Test 2554
	# b32* =0 +0.7FFFFFP-126 q -> q 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2553
	ret
L2553:


	# Test 2555
	# b32* =0 +1.000000P-126 q -> q 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2554
	ret
L2554:


	# Test 2556
	# b32* =0 +1.01B38BP119 q -> q 


	addi a1, a1, 1
	li t0, 0x7b01b38b	 #0x1.03671600p+119
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2555
	ret
L2555:


	# Test 2557
	# b32* =0 +1.7FFFFFP127 q -> q 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2556
	ret
L2556:


	# Test 2558
	# b32* =0 +Inf q -> q 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2557
	ret
L2557:


	# Test 2559
	# b32* =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2558
	ret
L2558:


	# Test 2560
	# b32* =0 q q -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fc00000	 #nan
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fmul.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2559
	ret
L2559:


	# Test 2561
	# b32/ =0 i -1.7FFFFFP127 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2560
	ret
L2560:


	# Test 2562
	# b32/ =0 i -1.3830E4P-74 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x9ab830e4	 #-0x1.7061c800p-74
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2561
	ret
L2561:


	# Test 2563
	# b32/ =0 i -1.000000P-126 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2562
	ret
L2562:


	# Test 2564
	# b32/ =0 i -0.7FFFFFP-126 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2563
	ret
L2563:


	# Test 2565
	# b32/ =0 i -0.4B77EAP-126 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x804b77ea	 #-0x1.2ddfa800p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2564
	ret
L2564:


	# Test 2566
	# b32/ =0 i -0.000001P-126 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2565
	ret
L2565:


	# Test 2567
	# b32/ =0 i -1.000000P0 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2566
	ret
L2566:


	# Test 2568
	# b32/ =0 i -Zero -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2567
	ret
L2567:


	# Test 2569
	# b32/ =0 i +Zero -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2568
	ret
L2568:


	# Test 2570
	# b32/ =0 i +1.000000P0 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2569
	ret
L2569:


	# Test 2571
	# b32/ =0 i +0.000001P-126 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2570
	ret
L2570:


	# Test 2572
	# b32/ =0 i +0.19D072P-126 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x19d072	 #0x1.9d072000p-129
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2571
	ret
L2571:


	# Test 2573
	# b32/ =0 i +0.7FFFFFP-126 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2572
	ret
L2572:


	# Test 2574
	# b32/ =0 i +1.000000P-126 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2573
	ret
L2573:


	# Test 2575
	# b32/ =0 i +1.5707A9P48 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x57d707a9	 #0x1.ae0f5200p+48
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2574
	ret
L2574:


	# Test 2576
	# b32/ =0 i +1.7FFFFFP127 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2575
	ret
L2575:


	# Test 2577
	# b32/ =0 i -Inf -1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2576
	ret
L2576:


	# Test 2578
	# b32/ =0 i -1.7FFFFFP127 -1.7FFFFFP127 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2577
	ret
L2577:


	# Test 2579
	# b32/ =0 i -1.06D59FP-91 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x9206d59f	 #-0x1.0dab3e00p-91
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2578
	ret
L2578:


	# Test 2580
	# b32/ =0 i -1.000000P-126 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2579
	ret
L2579:


	# Test 2581
	# b32/ =0 i -0.7FFFFFP-126 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2580
	ret
L2580:


	# Test 2582
	# b32/ =0 i -0.6F34BEP-126 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x806f34be	 #-0x1.bcd2f800p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2581
	ret
L2581:


	# Test 2583
	# b32/ =0 i -0.000001P-126 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2582
	ret
L2582:


	# Test 2584
	# b32/ =0 i -1.000000P0 -1.7FFFFFP127 -> +0.200000P-126 xu


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x200000	 #0x1.00000000p-128
	beq t0, a0, L2583
	ret
L2583:


	# Test 2585
	# b32/ =0 i -Zero -1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2584
	ret
L2584:


	# Test 2586
	# b32/ =0 i +Zero -1.7FFFFFP127 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2585
	ret
L2585:


	# Test 2587
	# b32/ =0 i +1.000000P0 -1.7FFFFFP127 -> -0.200000P-126 xu


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80200000	 #-0x1.00000000p-128
	beq t0, a0, L2586
	ret
L2586:


	# Test 2588
	# b32/ =0 i +0.000001P-126 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2587
	ret
L2587:


	# Test 2589
	# b32/ =0 i +0.18352EP-126 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x18352e	 #0x1.8352e000p-129
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2588
	ret
L2588:


	# Test 2590
	# b32/ =0 i +0.7FFFFFP-126 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2589
	ret
L2589:


	# Test 2591
	# b32/ =0 i +1.000000P-126 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2590
	ret
L2590:


	# Test 2592
	# b32/ =0 i +1.262C64P-65 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1f262c64	 #0x1.4c58c800p-65
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2591
	ret
L2591:


	# Test 2593
	# b32/ =0 i +1.7FFFFFP127 -1.7FFFFFP127 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2592
	ret
L2592:


	# Test 2594
	# b32/ =0 i +Inf -1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2593
	ret
L2593:


	# Test 2595
	# b32/ =0 i -Inf -1.1ACF4CP60 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xdd9acf4c	 #-0x1.359e9800p+60
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2594
	ret
L2594:


	# Test 2596
	# b32/ =0 i -1.7FFFFFP127 -1.6E5AFAP-28 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xb1ee5afa	 #-0x1.dcb5f400p-28
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2595
	ret
L2595:


	# Test 2597
	# b32/ =0 i -1.406242P52 -1.33CE3FP-61 -> +1.08F443P113 x


	addi a1, a1, 1
	li t0, 0xd9c06242	 #-0x1.80c48400p+52
	fmv.s.x f1, t0
	li t0, 0xa133ce3f	 #-0x1.679c7e00p-61
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7808f443	 #0x1.11e88600p+113
	beq t0, a0, L2596
	ret
L2596:


	# Test 2598
	# b32/ =0 i -1.000000P-126 -1.6F9D1DP-48 -> +1.08C0E1P-79 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xa7ef9d1d	 #-0x1.df3a3a00p-48
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1808c0e1	 #0x1.1181c200p-79
	beq t0, a0, L2597
	ret
L2597:


	# Test 2599
	# b32/ =0 i -0.7FFFFFP-126 -1.7B3C84P-75 -> +1.026D4DP-52 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x9a7b3c84	 #-0x1.f6790800p-75
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x25826d4d	 #0x1.04da9a00p-52
	beq t0, a0, L2598
	ret
L2598:


	# Test 2600
	# b32/ =0 i -0.125146P-126 -1.4B8B10P109 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80125146	 #-0x1.25146000p-129
	fmv.s.x f1, t0
	li t0, 0xf64b8b10	 #-0x1.97162000p+109
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2599
	ret
L2599:


	# Test 2601
	# b32/ =0 i -0.000001P-126 -1.452AEEP37 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xd2452aee	 #-0x1.8a55dc00p+37
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2600
	ret
L2600:


	# Test 2602
	# b32/ =0 i -1.000000P0 -1.500A56P74 -> +1.1D8205P-75 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xe4d00a56	 #-0x1.a014ac00p+74
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1a1d8205	 #0x1.3b040a00p-75
	beq t0, a0, L2601
	ret
L2601:


	# Test 2603
	# b32/ =0 i -Zero -1.5BE9BDP111 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xf75be9bd	 #-0x1.b7d37a00p+111
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2602
	ret
L2602:


	# Test 2604
	# b32/ =0 i +Zero -1.66C924P52 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xd9e6c924	 #-0x1.cd924800p+52
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2603
	ret
L2603:


	# Test 2605
	# b32/ =0 i +1.000000P0 -1.3A14D2P121 -> -1.301855P-122 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xfc3a14d2	 #-0x1.7429a400p+121
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff82b01855	 #-0x1.6030aa00p-122
	beq t0, a0, L2604
	ret
L2604:


	# Test 2606
	# b32/ =0 i +0.000001P-126 -1.45F43AP34 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xd0c5f43a	 #-0x1.8be87400p+34
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2605
	ret
L2605:


	# Test 2607
	# b32/ =0 i +0.0F59E9P-126 -1.492A86P63 -> -Zero xu


	addi a1, a1, 1
	li t0, 0xf59e9	 #0x1.eb3d2000p-130
	fmv.s.x f1, t0
	li t0, 0xdf492a86	 #-0x1.92550c00p+63
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2606
	ret
L2606:


	# Test 2608
	# b32/ =0 i +0.7FFFFFP-126 -1.47365DP-114 -> -1.247CDEP-13 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x86c7365d	 #-0x1.8e6cba00p-114
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb9247cde	 #-0x1.48f9bc00p-13
	beq t0, a0, L2607
	ret
L2607:


	# Test 2609
	# b32/ =0 i +1.000000P-126 -1.5215C5P19 -> -0.00000AP-126 xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xc95215c5	 #-0x1.a42b8a00p+19
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8000000a	 #-0x1.40000000p-146
	beq t0, a0, L2608
	ret
L2608:


	# Test 2610
	# b32/ =0 i +1.749120P-51 -1.51809DP88 -> -0.0004ABP-126 xu


	addi a1, a1, 1
	li t0, 0x26749120	 #0x1.e9224000p-51
	fmv.s.x f1, t0
	li t0, 0xebd1809d	 #-0x1.a3013a00p+88
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff800004ab	 #-0x1.2ac00000p-139
	beq t0, a0, L2609
	ret
L2609:


	# Test 2611
	# b32/ =0 i +1.7FFFFFP127 -1.3140DAP1 -> -1.38DD88P126 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xc03140da	 #-0x1.6281b400p+1
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffeb8dd88	 #-0x1.71bb1000p+126
	beq t0, a0, L2610
	ret
L2610:


	# Test 2612
	# b32/ =0 i +Inf -1.27E396P-24 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xb3a7e396	 #-0x1.4fc72c00p-24
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2611
	ret
L2611:


	# Test 2613
	# b32/ =0 i -Inf -1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2612
	ret
L2612:


	# Test 2614
	# b32/ =0 i -1.7FFFFFP127 -1.000000P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2613
	ret
L2613:


	# Test 2615
	# b32/ =0 i -1.0F06FDP35 -1.000000P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xd10f06fd	 #-0x1.1e0dfa00p+35
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2614
	ret
L2614:


	# Test 2616
	# b32/ =0 i -1.000000P-126 -1.000000P-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2615
	ret
L2615:


	# Test 2617
	# b32/ =0 i -0.7FFFFFP-126 -1.000000P-126 -> +1.7FFFFEP-1 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f7ffffe	 #0x1.fffffc00p-1
	beq t0, a0, L2616
	ret
L2616:


	# Test 2618
	# b32/ =0 i -0.10F601P-126 -1.000000P-126 -> +1.07B008P-3 


	addi a1, a1, 1
	li t0, 0x8010f601	 #-0x1.0f601000p-129
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3e07b008	 #0x1.0f601000p-3
	beq t0, a0, L2617
	ret
L2617:


	# Test 2619
	# b32/ =0 i -0.000001P-126 -1.000000P-126 -> +1.000000P-23 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34000000	 #0x1.00000000p-23
	beq t0, a0, L2618
	ret
L2618:


	# Test 2620
	# b32/ =0 i -1.000000P0 -1.000000P-126 -> +1.000000P126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e800000	 #0x1.00000000p+126
	beq t0, a0, L2619
	ret
L2619:


	# Test 2621
	# b32/ =0 i -Zero -1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2620
	ret
L2620:


	# Test 2622
	# b32/ =0 i +Zero -1.000000P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2621
	ret
L2621:


	# Test 2623
	# b32/ =0 i +1.000000P0 -1.000000P-126 -> -1.000000P126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe800000	 #-0x1.00000000p+126
	beq t0, a0, L2622
	ret
L2622:


	# Test 2624
	# b32/ =0 i +0.000001P-126 -1.000000P-126 -> -1.000000P-23 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4000000	 #-0x1.00000000p-23
	beq t0, a0, L2623
	ret
L2623:


	# Test 2625
	# b32/ =0 i +0.1B16BDP-126 -1.000000P-126 -> -1.58B5E8P-3 


	addi a1, a1, 1
	li t0, 0x1b16bd	 #0x1.b16bd000p-129
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbe58b5e8	 #-0x1.b16bd000p-3
	beq t0, a0, L2624
	ret
L2624:


	# Test 2626
	# b32/ =0 i +0.7FFFFFP-126 -1.000000P-126 -> -1.7FFFFEP-1 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf7ffffe	 #-0x1.fffffc00p-1
	beq t0, a0, L2625
	ret
L2625:


	# Test 2627
	# b32/ =0 i +1.000000P-126 -1.000000P-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2626
	ret
L2626:


	# Test 2628
	# b32/ =0 i +1.576DA8P125 -1.000000P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7e576da8	 #0x1.aedb5000p+125
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2627
	ret
L2627:


	# Test 2629
	# b32/ =0 i +1.7FFFFFP127 -1.000000P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2628
	ret
L2628:


	# Test 2630
	# b32/ =0 i +Inf -1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2629
	ret
L2629:


	# Test 2631
	# b32/ =0 i -Inf -0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2630
	ret
L2630:


	# Test 2632
	# b32/ =0 i -1.7FFFFFP127 -0.7FFFFFP-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2631
	ret
L2631:


	# Test 2633
	# b32/ =0 i -1.5DABB9P-123 -0.7FFFFFP-126 -> +1.5DABBBP3 x


	addi a1, a1, 1
	li t0, 0x825dabb9	 #-0x1.bb577200p-123
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x415dabbb	 #0x1.bb577600p+3
	beq t0, a0, L2632
	ret
L2632:


	# Test 2634
	# b32/ =0 i -1.000000P-126 -0.7FFFFFP-126 -> +1.000001P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800001	 #0x1.00000200p+0
	beq t0, a0, L2633
	ret
L2633:


	# Test 2635
	# b32/ =0 i -0.7FFFFFP-126 -0.7FFFFFP-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2634
	ret
L2634:


	# Test 2636
	# b32/ =0 i -0.0F9ABCP-126 -0.7FFFFFP-126 -> +1.79ABC2P-4 x


	addi a1, a1, 1
	li t0, 0x800f9abc	 #-0x1.f3578000p-130
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3df9abc2	 #0x1.f3578400p-4
	beq t0, a0, L2635
	ret
L2635:


	# Test 2637
	# b32/ =0 i -0.000001P-126 -0.7FFFFFP-126 -> +1.000001P-23 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34000001	 #0x1.00000200p-23
	beq t0, a0, L2636
	ret
L2636:


	# Test 2638
	# b32/ =0 i -1.000000P0 -0.7FFFFFP-126 -> +1.000001P126 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e800001	 #0x1.00000200p+126
	beq t0, a0, L2637
	ret
L2637:


	# Test 2639
	# b32/ =0 i -Zero -0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2638
	ret
L2638:


	# Test 2640
	# b32/ =0 i +Zero -0.7FFFFFP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2639
	ret
L2639:


	# Test 2641
	# b32/ =0 i +1.000000P0 -0.7FFFFFP-126 -> -1.000001P126 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe800001	 #-0x1.00000200p+126
	beq t0, a0, L2640
	ret
L2640:


	# Test 2642
	# b32/ =0 i +0.000001P-126 -0.7FFFFFP-126 -> -1.000001P-23 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4000001	 #-0x1.00000200p-23
	beq t0, a0, L2641
	ret
L2641:


	# Test 2643
	# b32/ =0 i +0.09BB78P-126 -0.7FFFFFP-126 -> -1.1BB781P-4 x


	addi a1, a1, 1
	li t0, 0x9bb78	 #0x1.376f0000p-130
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbd9bb781	 #-0x1.376f0200p-4
	beq t0, a0, L2642
	ret
L2642:


	# Test 2644
	# b32/ =0 i +0.7FFFFFP-126 -0.7FFFFFP-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2643
	ret
L2643:


	# Test 2645
	# b32/ =0 i +1.000000P-126 -0.7FFFFFP-126 -> -1.000001P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800001	 #-0x1.00000200p+0
	beq t0, a0, L2644
	ret
L2644:


	# Test 2646
	# b32/ =0 i +1.3B2A7CP-81 -0.7FFFFFP-126 -> -1.3B2A7DP45 x


	addi a1, a1, 1
	li t0, 0x173b2a7c	 #0x1.7654f800p-81
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd63b2a7d	 #-0x1.7654fa00p+45
	beq t0, a0, L2645
	ret
L2645:


	# Test 2647
	# b32/ =0 i +1.7FFFFFP127 -0.7FFFFFP-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2646
	ret
L2646:


	# Test 2648
	# b32/ =0 i +Inf -0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2647
	ret
L2647:


	# Test 2649
	# b32/ =0 i -Inf -0.675238P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80675238	 #-0x1.9d48e000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2648
	ret
L2648:


	# Test 2650
	# b32/ =0 i -1.7FFFFFP127 -0.12B19FP-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x8012b19f	 #-0x1.2b19f000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2649
	ret
L2649:


	# Test 2651
	# b32/ =0 i -1.41A88CP111 -0.5E174BP-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xf741a88c	 #-0x1.83511800p+111
	fmv.s.x f1, t0
	li t0, 0x805e174b	 #-0x1.785d2c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2650
	ret
L2650:


	# Test 2652
	# b32/ =0 i -1.000000P-126 -0.1CA009P-126 -> +1.0F174DP2 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x801ca009	 #-0x1.ca009000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x408f174d	 #0x1.1e2e9a00p+2
	beq t0, a0, L2651
	ret
L2651:


	# Test 2653
	# b32/ =0 i -0.7FFFFFP-126 -0.47FF71P-126 -> +1.638FFBP0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x8047ff71	 #-0x1.1ffdc400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3fe38ffb	 #0x1.c71ff600p+0
	beq t0, a0, L2652
	ret
L2652:


	# Test 2654
	# b32/ =0 i -0.00BF77P-126 -0.61EB5DP-126 -> +1.7A484CP-8 x


	addi a1, a1, 1
	li t0, 0x8000bf77	 #-0x1.7eee0000p-134
	fmv.s.x f1, t0
	li t0, 0x8061eb5d	 #-0x1.87ad7400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3bfa484c	 #0x1.f4909800p-8
	beq t0, a0, L2653
	ret
L2653:


	# Test 2655
	# b32/ =0 i -0.000001P-126 -0.32FAEBP-126 -> +1.20B0A4P-22 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x8032faeb	 #-0x1.97d75800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34a0b0a4	 #0x1.41614800p-22
	beq t0, a0, L2654
	ret
L2654:


	# Test 2656
	# b32/ =0 i -1.000000P0 -0.71C9EDP-126 -> +1.0FFC70P126 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8071c9ed	 #-0x1.c727b400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e8ffc70	 #0x1.1ff8e000p+126
	beq t0, a0, L2655
	ret
L2655:


	# Test 2657
	# b32/ =0 i -Zero -0.3D6955P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x803d6955	 #-0x1.eb4aa800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2656
	ret
L2656:


	# Test 2658
	# b32/ =0 i +Zero -0.4848BCP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x804848bc	 #-0x1.2122f000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2657
	ret
L2657:


	# Test 2659
	# b32/ =0 i +1.000000P0 -0.7EAB78P-126 -> -1.01581BP126 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807eab78	 #-0x1.faade000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe81581b	 #-0x1.02b03600p+126
	beq t0, a0, L2658
	ret
L2658:


	# Test 2660
	# b32/ =0 i +0.000001P-126 -0.098ADFP-126 -> -1.569E9EP-20 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80098adf	 #-0x1.315be000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb5d69e9e	 #-0x1.ad3d3c00p-20
	beq t0, a0, L2659
	ret
L2659:


	# Test 2661
	# b32/ =0 i +0.016033P-126 -0.13B392P-126 -> -1.0F03BAP-4 x


	addi a1, a1, 1
	li t0, 0x16033	 #0x1.60330000p-133
	fmv.s.x f1, t0
	li t0, 0x8013b392	 #-0x1.3b392000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbd8f03ba	 #-0x1.1e077400p-4
	beq t0, a0, L2660
	ret
L2660:


	# Test 2662
	# b32/ =0 i +0.7FFFFFP-126 -0.68B5F5P-126 -> -1.1C7816P0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x8068b5f5	 #-0x1.a2d7d400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf9c7816	 #-0x1.38f02c00p+0
	beq t0, a0, L2661
	ret
L2661:


	# Test 2663
	# b32/ =0 i +1.000000P-126 -0.5F58B1P-126 -> -1.2BD625P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x805f58b1	 #-0x1.7d62c400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbfabd625	 #-0x1.57ac4a00p+0
	beq t0, a0, L2662
	ret
L2662:


	# Test 2664
	# b32/ =0 i +1.0A4F37P62 -0.1B7062P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x5e8a4f37	 #0x1.149e6e00p+62
	fmv.s.x f1, t0
	li t0, 0x801b7062	 #-0x1.b7062000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2663
	ret
L2663:


	# Test 2665
	# b32/ =0 i +1.7FFFFFP127 -0.3E03C6P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x803e03c6	 #-0x1.f01e3000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2664
	ret
L2664:


	# Test 2666
	# b32/ =0 i +Inf -0.0C6682P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800c6682	 #-0x1.8cd04000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2665
	ret
L2665:


	# Test 2667
	# b32/ =0 i -Inf -0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2666
	ret
L2666:


	# Test 2668
	# b32/ =0 i -1.7FFFFFP127 -0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2667
	ret
L2667:


	# Test 2669
	# b32/ =0 i -1.104D48P-18 -0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xb6904d48	 #-0x1.209a9000p-18
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2668
	ret
L2668:


	# Test 2670
	# b32/ =0 i -1.000000P-126 -0.000001P-126 -> +1.000000P23 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4b000000	 #0x1.00000000p+23
	beq t0, a0, L2669
	ret
L2669:


	# Test 2671
	# b32/ =0 i -0.7FFFFFP-126 -0.000001P-126 -> +1.7FFFFEP22 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4afffffe	 #0x1.fffffc00p+22
	beq t0, a0, L2670
	ret
L2670:


	# Test 2672
	# b32/ =0 i -0.067C4BP-126 -0.000001P-126 -> +1.4F8960P18 


	addi a1, a1, 1
	li t0, 0x80067c4b	 #-0x1.9f12c000p-131
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x48cf8960	 #0x1.9f12c000p+18
	beq t0, a0, L2671
	ret
L2671:


	# Test 2673
	# b32/ =0 i -0.000001P-126 -0.000001P-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2672
	ret
L2672:


	# Test 2674
	# b32/ =0 i -1.000000P0 -0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2673
	ret
L2673:


	# Test 2675
	# b32/ =0 i -Zero -0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2674
	ret
L2674:


	# Test 2676
	# b32/ =0 i +Zero -0.000001P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2675
	ret
L2675:


	# Test 2677
	# b32/ =0 i +1.000000P0 -0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2676
	ret
L2676:


	# Test 2678
	# b32/ =0 i +0.000001P-126 -0.000001P-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2677
	ret
L2677:


	# Test 2679
	# b32/ =0 i +0.4744EFP-126 -0.000001P-126 -> -1.0E89DEP22 


	addi a1, a1, 1
	li t0, 0x4744ef	 #0x1.1d13bc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffca8e89de	 #-0x1.1d13bc00p+22
	beq t0, a0, L2678
	ret
L2678:


	# Test 2680
	# b32/ =0 i +0.7FFFFFP-126 -0.000001P-126 -> -1.7FFFFEP22 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcafffffe	 #-0x1.fffffc00p+22
	beq t0, a0, L2679
	ret
L2679:


	# Test 2681
	# b32/ =0 i +1.000000P-126 -0.000001P-126 -> -1.000000P23 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcb000000	 #-0x1.00000000p+23
	beq t0, a0, L2680
	ret
L2680:


	# Test 2682
	# b32/ =0 i +1.58F3F2P44 -0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x55d8f3f2	 #0x1.b1e7e400p+44
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2681
	ret
L2681:


	# Test 2683
	# b32/ =0 i +1.7FFFFFP127 -0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2682
	ret
L2682:


	# Test 2684
	# b32/ =0 i +Inf -0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2683
	ret
L2683:


	# Test 2685
	# b32/ =0 i -Inf -1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2684
	ret
L2684:


	# Test 2686
	# b32/ =0 i -1.7FFFFFP127 -1.000000P0 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L2685
	ret
L2685:


	# Test 2687
	# b32/ =0 i -1.7329D0P-126 -1.000000P0 -> +1.7329D0P-126 


	addi a1, a1, 1
	li t0, 0x80f329d0	 #-0x1.e653a000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xf329d0	 #0x1.e653a000p-126
	beq t0, a0, L2686
	ret
L2686:


	# Test 2688
	# b32/ =0 i -1.000000P-126 -1.000000P0 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L2687
	ret
L2687:


	# Test 2689
	# b32/ =0 i -0.7FFFFFP-126 -1.000000P0 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L2688
	ret
L2688:


	# Test 2690
	# b32/ =0 i -0.70E106P-126 -1.000000P0 -> +0.70E106P-126 


	addi a1, a1, 1
	li t0, 0x8070e106	 #-0x1.c3841800p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x70e106	 #0x1.c3841800p-127
	beq t0, a0, L2689
	ret
L2689:


	# Test 2691
	# b32/ =0 i -0.000001P-126 -1.000000P0 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L2690
	ret
L2690:


	# Test 2692
	# b32/ =0 i -1.000000P0 -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2691
	ret
L2691:


	# Test 2693
	# b32/ =0 i -Zero -1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2692
	ret
L2692:


	# Test 2694
	# b32/ =0 i +Zero -1.000000P0 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2693
	ret
L2693:


	# Test 2695
	# b32/ =0 i +1.000000P0 -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2694
	ret
L2694:


	# Test 2696
	# b32/ =0 i +0.000001P-126 -1.000000P0 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L2695
	ret
L2695:


	# Test 2697
	# b32/ =0 i +0.55E9AAP-126 -1.000000P0 -> -0.55E9AAP-126 


	addi a1, a1, 1
	li t0, 0x55e9aa	 #0x1.57a6a800p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8055e9aa	 #-0x1.57a6a800p-127
	beq t0, a0, L2696
	ret
L2696:


	# Test 2698
	# b32/ =0 i +0.7FFFFFP-126 -1.000000P0 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L2697
	ret
L2697:


	# Test 2699
	# b32/ =0 i +1.000000P-126 -1.000000P0 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L2698
	ret
L2698:


	# Test 2700
	# b32/ =0 i +1.2798ADP27 -1.000000P0 -> -1.2798ADP27 


	addi a1, a1, 1
	li t0, 0x4d2798ad	 #0x1.4f315a00p+27
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcd2798ad	 #-0x1.4f315a00p+27
	beq t0, a0, L2699
	ret
L2699:


	# Test 2701
	# b32/ =0 i +1.7FFFFFP127 -1.000000P0 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L2700
	ret
L2700:


	# Test 2702
	# b32/ =0 i +Inf -1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2701
	ret
L2701:


	# Test 2703
	# b32/ =0 i -Inf -Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2702
	ret
L2702:


	# Test 2704
	# b32/ =0 i -1.7FFFFFP127 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2703
	ret
L2703:


	# Test 2705
	# b32/ =0 i -1.41CE8BP-124 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x81c1ce8b	 #-0x1.839d1600p-124
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2704
	ret
L2704:


	# Test 2706
	# b32/ =0 i -1.000000P-126 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2705
	ret
L2705:


	# Test 2707
	# b32/ =0 i -0.7FFFFFP-126 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2706
	ret
L2706:


	# Test 2708
	# b32/ =0 i -0.7F85C2P-126 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x807f85c2	 #-0x1.fe170800p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2707
	ret
L2707:


	# Test 2709
	# b32/ =0 i -0.000001P-126 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2708
	ret
L2708:


	# Test 2710
	# b32/ =0 i -1.000000P0 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2709
	ret
L2709:


	# Test 2711
	# b32/ =0 i +1.000000P0 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2710
	ret
L2710:


	# Test 2712
	# b32/ =0 i +0.000001P-126 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2711
	ret
L2711:


	# Test 2713
	# b32/ =0 i +0.2DDE4AP-126 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x2dde4a	 #0x1.6ef25000p-128
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2712
	ret
L2712:


	# Test 2714
	# b32/ =0 i +0.7FFFFFP-126 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2713
	ret
L2713:


	# Test 2715
	# b32/ =0 i +1.000000P-126 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2714
	ret
L2714:


	# Test 2716
	# b32/ =0 i +1.767D69P-55 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x24767d69	 #0x1.ecfad200p-55
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2715
	ret
L2715:


	# Test 2717
	# b32/ =0 i +1.7FFFFFP127 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2716
	ret
L2716:


	# Test 2718
	# b32/ =0 i +Inf -Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2717
	ret
L2717:


	# Test 2719
	# b32/ =0 i -Inf +Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2718
	ret
L2718:


	# Test 2720
	# b32/ =0 i -1.7FFFFFP127 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2719
	ret
L2719:


	# Test 2721
	# b32/ =0 i -1.258B5FP30 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0xcea58b5f	 #-0x1.4b16be00p+30
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2720
	ret
L2720:


	# Test 2722
	# b32/ =0 i -1.000000P-126 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2721
	ret
L2721:


	# Test 2723
	# b32/ =0 i -0.7FFFFFP-126 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2722
	ret
L2722:


	# Test 2724
	# b32/ =0 i -0.4E2A7DP-126 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x804e2a7d	 #-0x1.38a9f400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2723
	ret
L2723:


	# Test 2725
	# b32/ =0 i -0.000001P-126 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2724
	ret
L2724:


	# Test 2726
	# b32/ =0 i -1.000000P0 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2725
	ret
L2725:


	# Test 2727
	# b32/ =0 i +1.000000P0 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2726
	ret
L2726:


	# Test 2728
	# b32/ =0 i +0.000001P-126 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2727
	ret
L2727:


	# Test 2729
	# b32/ =0 i +0.1C8306P-126 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x1c8306	 #0x1.c8306000p-129
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2728
	ret
L2728:


	# Test 2730
	# b32/ =0 i +0.7FFFFFP-126 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2729
	ret
L2729:


	# Test 2731
	# b32/ =0 i +1.000000P-126 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2730
	ret
L2730:


	# Test 2732
	# b32/ =0 i +1.5A3A3DP55 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x5b5a3a3d	 #0x1.b4747a00p+55
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2731
	ret
L2731:


	# Test 2733
	# b32/ =0 i +1.7FFFFFP127 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2732
	ret
L2732:


	# Test 2734
	# b32/ =0 i +Inf +Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2733
	ret
L2733:


	# Test 2735
	# b32/ =0 i -Inf +1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2734
	ret
L2734:


	# Test 2736
	# b32/ =0 i -1.7FFFFFP127 +1.000000P0 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L2735
	ret
L2735:


	# Test 2737
	# b32/ =0 i -1.74B01AP12 +1.000000P0 -> -1.74B01AP12 


	addi a1, a1, 1
	li t0, 0xc5f4b01a	 #-0x1.e9603400p+12
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc5f4b01a	 #-0x1.e9603400p+12
	beq t0, a0, L2736
	ret
L2736:


	# Test 2738
	# b32/ =0 i -1.000000P-126 +1.000000P0 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L2737
	ret
L2737:


	# Test 2739
	# b32/ =0 i -0.7FFFFFP-126 +1.000000P0 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L2738
	ret
L2738:


	# Test 2740
	# b32/ =0 i -0.722751P-126 +1.000000P0 -> -0.722751P-126 


	addi a1, a1, 1
	li t0, 0x80722751	 #-0x1.c89d4400p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80722751	 #-0x1.c89d4400p-127
	beq t0, a0, L2739
	ret
L2739:


	# Test 2741
	# b32/ =0 i -0.000001P-126 +1.000000P0 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L2740
	ret
L2740:


	# Test 2742
	# b32/ =0 i -1.000000P0 +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2741
	ret
L2741:


	# Test 2743
	# b32/ =0 i -Zero +1.000000P0 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2742
	ret
L2742:


	# Test 2744
	# b32/ =0 i +Zero +1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2743
	ret
L2743:


	# Test 2745
	# b32/ =0 i +1.000000P0 +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2744
	ret
L2744:


	# Test 2746
	# b32/ =0 i +0.000001P-126 +1.000000P0 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L2745
	ret
L2745:


	# Test 2747
	# b32/ =0 i +0.1B27C1P-126 +1.000000P0 -> +0.1B27C1P-126 


	addi a1, a1, 1
	li t0, 0x1b27c1	 #0x1.b27c1000p-129
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1b27c1	 #0x1.b27c1000p-129
	beq t0, a0, L2746
	ret
L2746:


	# Test 2748
	# b32/ =0 i +0.7FFFFFP-126 +1.000000P0 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L2747
	ret
L2747:


	# Test 2749
	# b32/ =0 i +1.000000P-126 +1.000000P0 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L2748
	ret
L2748:


	# Test 2750
	# b32/ =0 i +1.28DEF8P-122 +1.000000P0 -> +1.28DEF8P-122 


	addi a1, a1, 1
	li t0, 0x2a8def8	 #0x1.51bdf000p-122
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2a8def8	 #0x1.51bdf000p-122
	beq t0, a0, L2749
	ret
L2749:


	# Test 2751
	# b32/ =0 i +1.7FFFFFP127 +1.000000P0 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L2750
	ret
L2750:


	# Test 2752
	# b32/ =0 i +Inf +1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2751
	ret
L2751:


	# Test 2753
	# b32/ =0 i -Inf +0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2752
	ret
L2752:


	# Test 2754
	# b32/ =0 i -1.7FFFFFP127 +0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2753
	ret
L2753:


	# Test 2755
	# b32/ =0 i -1.4314D5P107 +0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xf54314d5	 #-0x1.8629aa00p+107
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2754
	ret
L2754:


	# Test 2756
	# b32/ =0 i -1.000000P-126 +0.000001P-126 -> -1.000000P23 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcb000000	 #-0x1.00000000p+23
	beq t0, a0, L2755
	ret
L2755:


	# Test 2757
	# b32/ =0 i -0.7FFFFFP-126 +0.000001P-126 -> -1.7FFFFEP22 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcafffffe	 #-0x1.fffffc00p+22
	beq t0, a0, L2756
	ret
L2756:


	# Test 2758
	# b32/ =0 i -0.40CC0CP-126 +0.000001P-126 -> -1.019818P22 


	addi a1, a1, 1
	li t0, 0x8040cc0c	 #-0x1.03303000p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffca819818	 #-0x1.03303000p+22
	beq t0, a0, L2757
	ret
L2757:


	# Test 2759
	# b32/ =0 i -0.000001P-126 +0.000001P-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2758
	ret
L2758:


	# Test 2760
	# b32/ =0 i -1.000000P0 +0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2759
	ret
L2759:


	# Test 2761
	# b32/ =0 i -Zero +0.000001P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2760
	ret
L2760:


	# Test 2762
	# b32/ =0 i +Zero +0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2761
	ret
L2761:


	# Test 2763
	# b32/ =0 i +1.000000P0 +0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2762
	ret
L2762:


	# Test 2764
	# b32/ =0 i +0.000001P-126 +0.000001P-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2763
	ret
L2763:


	# Test 2765
	# b32/ =0 i +0.098C7CP-126 +0.000001P-126 -> +1.18C7C0P19 


	addi a1, a1, 1
	li t0, 0x98c7c	 #0x1.318f8000p-130
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4918c7c0	 #0x1.318f8000p+19
	beq t0, a0, L2764
	ret
L2764:


	# Test 2766
	# b32/ =0 i +0.7FFFFFP-126 +0.000001P-126 -> +1.7FFFFEP22 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4afffffe	 #0x1.fffffc00p+22
	beq t0, a0, L2765
	ret
L2765:


	# Test 2767
	# b32/ =0 i +1.000000P-126 +0.000001P-126 -> +1.000000P23 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4b000000	 #0x1.00000000p+23
	beq t0, a0, L2766
	ret
L2766:


	# Test 2768
	# b32/ =0 i +1.7783B3P-120 +0.000001P-126 -> +1.7783B3P29 


	addi a1, a1, 1
	li t0, 0x3f783b3	 #0x1.ef076600p-120
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4e7783b3	 #0x1.ef076600p+29
	beq t0, a0, L2767
	ret
L2767:


	# Test 2769
	# b32/ =0 i +1.7FFFFFP127 +0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2768
	ret
L2768:


	# Test 2770
	# b32/ =0 i +Inf +0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2769
	ret
L2769:


	# Test 2771
	# b32/ =0 i -Inf +0.4B1145P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x4b1145	 #0x1.2c451400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2770
	ret
L2770:


	# Test 2772
	# b32/ =0 i -1.7FFFFFP127 +0.3630ACP-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3630ac	 #0x1.b1856000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2771
	ret
L2771:


	# Test 2773
	# b32/ =0 i -1.11B991P106 +0.157222P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xf491b991	 #-0x1.23732200p+106
	fmv.s.x f1, t0
	li t0, 0x157222	 #0x1.57222000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2772
	ret
L2772:


	# Test 2774
	# b32/ =0 i -1.000000P-126 +0.57B2CFP-126 -> -1.3AD26CP0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x57b2cf	 #0x1.5ecb3c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbfbad26c	 #-0x1.75a4d800p+0
	beq t0, a0, L2773
	ret
L2773:


	# Test 2775
	# b32/ =0 i -0.7FFFFFP-126 +0.0A9237P-126 -> -1.41BC1BP3 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xa9237	 #0x1.5246e000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc141bc1b	 #-0x1.83783600p+3
	beq t0, a0, L2774
	ret
L2774:


	# Test 2776
	# b32/ =0 i -0.4FB0C7P-126 +0.1D0839P-126 -> -1.2FAC98P1 x


	addi a1, a1, 1
	li t0, 0x804fb0c7	 #-0x1.3ec31c00p-127
	fmv.s.x f1, t0
	li t0, 0x1d0839	 #0x1.d0839000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc02fac98	 #-0x1.5f593000p+1
	beq t0, a0, L2775
	ret
L2775:


	# Test 2777
	# b32/ =0 i -0.000001P-126 +0.0D00A1P-126 -> -1.1D823AP-20 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xd00a1	 #0x1.a0142000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb59d823a	 #-0x1.3b047400p-20
	beq t0, a0, L2776
	ret
L2776:


	# Test 2778
	# b32/ =0 i -1.000000P0 +0.786008P-126 -> -1.081B9DP126 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x786008	 #0x1.e1802000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe881b9d	 #-0x1.10373a00p+126
	beq t0, a0, L2777
	ret
L2777:


	# Test 2779
	# b32/ =0 i -Zero +0.0B7F6FP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xb7f6f	 #0x1.6fede000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2778
	ret
L2778:


	# Test 2780
	# b32/ =0 i +Zero +0.424E72P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x424e72	 #0x1.0939c800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2779
	ret
L2779:


	# Test 2781
	# b32/ =0 i +1.000000P0 +0.0DADD9P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xdadd9	 #0x1.b5bb2000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2780
	ret
L2780:


	# Test 2782
	# b32/ =0 i +0.000001P-126 +0.58CD41P-126 -> +1.388036P-23 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x58cd41	 #0x1.63350400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34388036	 #0x1.71006c00p-23
	beq t0, a0, L2781
	ret
L2781:


	# Test 2783
	# b32/ =0 i +0.1DC950P-126 +0.5EF727P-126 -> +1.209761P-2 x


	addi a1, a1, 1
	li t0, 0x1dc950	 #0x1.dc950000p-129
	fmv.s.x f1, t0
	li t0, 0x5ef727	 #0x1.7bdc9c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3ea09761	 #0x1.412ec200p-2
	beq t0, a0, L2782
	ret
L2782:


	# Test 2784
	# b32/ =0 i +0.7FFFFFP-126 +0.6F0C0FP-126 -> +1.138A7DP0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x6f0c0f	 #0x1.bc303c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f938a7d	 #0x1.2714fa00p+0
	beq t0, a0, L2783
	ret
L2783:


	# Test 2785
	# b32/ =0 i +1.000000P-126 +0.42D7BDP-126 -> +1.751CD8P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x42d7bd	 #0x1.0b5ef400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3ff51cd8	 #0x1.ea39b000p+0
	beq t0, a0, L2784
	ret
L2784:


	# Test 2786
	# b32/ =0 i +1.315056P4 +0.3EFBC1P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x41b15056	 #0x1.62a0ac00p+4
	fmv.s.x f1, t0
	li t0, 0x3efbc1	 #0x1.f7de0800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2785
	ret
L2785:


	# Test 2787
	# b32/ =0 i +1.7FFFFFP127 +0.0459E1P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x459e1	 #0x1.16784000p-131
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2786
	ret
L2786:


	# Test 2788
	# b32/ =0 i +Inf +0.4F3948P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x4f3948	 #0x1.3ce52000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2787
	ret
L2787:


	# Test 2789
	# b32/ =0 i -Inf +0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2788
	ret
L2788:


	# Test 2790
	# b32/ =0 i -1.7FFFFFP127 +0.7FFFFFP-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2789
	ret
L2789:


	# Test 2791
	# b32/ =0 i -1.60DE4CP-40 +0.7FFFFFP-126 -> -1.60DE4EP86 x


	addi a1, a1, 1
	li t0, 0xabe0de4c	 #-0x1.c1bc9800p-40
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffeae0de4e	 #-0x1.c1bc9c00p+86
	beq t0, a0, L2790
	ret
L2790:


	# Test 2792
	# b32/ =0 i -1.000000P-126 +0.7FFFFFP-126 -> -1.000001P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800001	 #-0x1.00000200p+0
	beq t0, a0, L2791
	ret
L2791:


	# Test 2793
	# b32/ =0 i -0.7FFFFFP-126 +0.7FFFFFP-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2792
	ret
L2792:


	# Test 2794
	# b32/ =0 i -0.493D6AP-126 +0.7FFFFFP-126 -> -1.127AD5P-1 x


	addi a1, a1, 1
	li t0, 0x80493d6a	 #-0x1.24f5a800p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf127ad5	 #-0x1.24f5aa00p-1
	beq t0, a0, L2793
	ret
L2793:


	# Test 2795
	# b32/ =0 i -0.000001P-126 +0.7FFFFFP-126 -> -1.000001P-23 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4000001	 #-0x1.00000200p-23
	beq t0, a0, L2794
	ret
L2794:


	# Test 2796
	# b32/ =0 i -1.000000P0 +0.7FFFFFP-126 -> -1.000001P126 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe800001	 #-0x1.00000200p+126
	beq t0, a0, L2795
	ret
L2795:


	# Test 2797
	# b32/ =0 i -Zero +0.7FFFFFP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2796
	ret
L2796:


	# Test 2798
	# b32/ =0 i +Zero +0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2797
	ret
L2797:


	# Test 2799
	# b32/ =0 i +1.000000P0 +0.7FFFFFP-126 -> +1.000001P126 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e800001	 #0x1.00000200p+126
	beq t0, a0, L2798
	ret
L2798:


	# Test 2800
	# b32/ =0 i +0.000001P-126 +0.7FFFFFP-126 -> +1.000001P-23 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34000001	 #0x1.00000200p-23
	beq t0, a0, L2799
	ret
L2799:


	# Test 2801
	# b32/ =0 i +0.5755F3P-126 +0.7FFFFFP-126 -> +1.2EABE7P-1 x


	addi a1, a1, 1
	li t0, 0x5755f3	 #0x1.5d57cc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f2eabe7	 #0x1.5d57ce00p-1
	beq t0, a0, L2800
	ret
L2800:


	# Test 2802
	# b32/ =0 i +0.7FFFFFP-126 +0.7FFFFFP-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2801
	ret
L2801:


	# Test 2803
	# b32/ =0 i +1.000000P-126 +0.7FFFFFP-126 -> +1.000001P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800001	 #0x1.00000200p+0
	beq t0, a0, L2802
	ret
L2802:


	# Test 2804
	# b32/ =0 i +1.154D2AP-118 +0.7FFFFFP-126 -> +1.154D2BP8 x


	addi a1, a1, 1
	li t0, 0x4954d2a	 #0x1.2a9a5400p-118
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x43954d2b	 #0x1.2a9a5600p+8
	beq t0, a0, L2803
	ret
L2803:


	# Test 2805
	# b32/ =0 i +1.7FFFFFP127 +0.7FFFFFP-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2804
	ret
L2804:


	# Test 2806
	# b32/ =0 i +Inf +0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2805
	ret
L2805:


	# Test 2807
	# b32/ =0 i -Inf +1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2806
	ret
L2806:


	# Test 2808
	# b32/ =0 i -1.7FFFFFP127 +1.000000P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2807
	ret
L2807:


	# Test 2809
	# b32/ =0 i -1.44DB20P-58 +1.000000P-126 -> -1.44DB20P68 


	addi a1, a1, 1
	li t0, 0xa2c4db20	 #-0x1.89b64000p-58
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe1c4db20	 #-0x1.89b64000p+68
	beq t0, a0, L2808
	ret
L2808:


	# Test 2810
	# b32/ =0 i -1.000000P-126 +1.000000P-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2809
	ret
L2809:


	# Test 2811
	# b32/ =0 i -0.7FFFFFP-126 +1.000000P-126 -> -1.7FFFFEP-1 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf7ffffe	 #-0x1.fffffc00p-1
	beq t0, a0, L2810
	ret
L2810:


	# Test 2812
	# b32/ =0 i -0.57E225P-126 +1.000000P-126 -> -1.2FC44AP-1 


	addi a1, a1, 1
	li t0, 0x8057e225	 #-0x1.5f889400p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf2fc44a	 #-0x1.5f889400p-1
	beq t0, a0, L2811
	ret
L2811:


	# Test 2813
	# b32/ =0 i -0.000001P-126 +1.000000P-126 -> -1.000000P-23 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4000000	 #-0x1.00000000p-23
	beq t0, a0, L2812
	ret
L2812:


	# Test 2814
	# b32/ =0 i -1.000000P0 +1.000000P-126 -> -1.000000P126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe800000	 #-0x1.00000000p+126
	beq t0, a0, L2813
	ret
L2813:


	# Test 2815
	# b32/ =0 i -Zero +1.000000P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2814
	ret
L2814:


	# Test 2816
	# b32/ =0 i +Zero +1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2815
	ret
L2815:


	# Test 2817
	# b32/ =0 i +1.000000P0 +1.000000P-126 -> +1.000000P126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e800000	 #0x1.00000000p+126
	beq t0, a0, L2816
	ret
L2816:


	# Test 2818
	# b32/ =0 i +0.000001P-126 +1.000000P-126 -> +1.000000P-23 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34000000	 #0x1.00000000p-23
	beq t0, a0, L2817
	ret
L2817:


	# Test 2819
	# b32/ =0 i +0.667AAEP-126 +1.000000P-126 -> +1.4CF55CP-1 


	addi a1, a1, 1
	li t0, 0x667aae	 #0x1.99eab800p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f4cf55c	 #0x1.99eab800p-1
	beq t0, a0, L2818
	ret
L2818:


	# Test 2820
	# b32/ =0 i +0.7FFFFFP-126 +1.000000P-126 -> +1.7FFFFEP-1 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f7ffffe	 #0x1.fffffc00p-1
	beq t0, a0, L2819
	ret
L2819:


	# Test 2821
	# b32/ =0 i +1.000000P-126 +1.000000P-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2820
	ret
L2820:


	# Test 2822
	# b32/ =0 i +1.63F1E5P32 +1.000000P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x4fe3f1e5	 #0x1.c7e3ca00p+32
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2821
	ret
L2821:


	# Test 2823
	# b32/ =0 i +1.7FFFFFP127 +1.000000P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2822
	ret
L2822:


	# Test 2824
	# b32/ =0 i +Inf +1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2823
	ret
L2823:


	# Test 2825
	# b32/ =0 i -Inf +1.4F27EAP-88 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x13cf27ea	 #0x1.9e4fd400p-88
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2824
	ret
L2824:


	# Test 2826
	# b32/ =0 i -1.7FFFFFP127 +1.237398P49 -> -1.4879AEP78 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x58237398	 #0x1.46e73000p+49
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe6c879ae	 #-0x1.90f35c00p+78
	beq t0, a0, L2825
	ret
L2825:


	# Test 2827
	# b32/ =0 i -1.7E67C2P-75 +1.4A9270P61 -> -0.002830P-126 xu


	addi a1, a1, 1
	li t0, 0x9a7e67c2	 #-0x1.fccf8400p-75
	fmv.s.x f1, t0
	li t0, 0x5e4a9270	 #0x1.9524e000p+61
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80002830	 #-0x1.41800000p-136
	beq t0, a0, L2826
	ret
L2826:


	# Test 2828
	# b32/ =0 i -1.000000P-126 +1.24F5BBP93 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x6e24f5bb	 #0x1.49eb7600p+93
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2827
	ret
L2827:


	# Test 2829
	# b32/ =0 i -0.7FFFFFP-126 +1.2FD523P98 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x70afd523	 #0x1.5faa4600p+98
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2828
	ret
L2828:


	# Test 2830
	# b32/ =0 i -0.4FD6C6P-126 +1.62CF40P-9 -> -1.343A7EP-119 x


	addi a1, a1, 1
	li t0, 0x804fd6c6	 #-0x1.3f5b1800p-127
	fmv.s.x f1, t0
	li t0, 0x3b62cf40	 #0x1.c59e8000p-9
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff84343a7e	 #-0x1.6874fc00p-119
	beq t0, a0, L2829
	ret
L2829:


	# Test 2831
	# b32/ =0 i -0.000001P-126 +1.79C38DP-15 -> -0.004199P-126 xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3879c38d	 #0x1.f3871a00p-15
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80004199	 #-0x1.06640000p-135
	beq t0, a0, L2830
	ret
L2830:


	# Test 2832
	# b32/ =0 i -1.000000P0 +1.199FA0P85 -> -1.554CF7P-86 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x6a199fa0	 #0x1.333f4000p+85
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff94d54cf7	 #-0x1.aa99ee00p-86
	beq t0, a0, L2831
	ret
L2831:


	# Test 2833
	# b32/ =0 i -Zero +1.257F07P106 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x74a57f07	 #0x1.4afe0e00p+106
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2832
	ret
L2832:


	# Test 2834
	# b32/ =0 i +Zero +1.301E6EP95 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x6f301e6e	 #0x1.603cdc00p+95
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2833
	ret
L2833:


	# Test 2835
	# b32/ =0 i +1.000000P0 +1.26812AP6 -> +1.44CCA7P-7 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x42a6812a	 #0x1.4d025400p+6
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3c44cca7	 #0x1.89994e00p-7
	beq t0, a0, L2834
	ret
L2834:


	# Test 2836
	# b32/ =0 i +0.000001P-126 +1.7A4CD8P-98 -> +1.02EA31P-52 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xefa4cd8	 #0x1.f499b000p-98
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2582ea31	 #0x1.05d46200p-52
	beq t0, a0, L2835
	ret
L2835:


	# Test 2837
	# b32/ =0 i +0.749F69P-126 +1.602EB6P-7 -> +1.052CB3P-120 x


	addi a1, a1, 1
	li t0, 0x749f69	 #0x1.d27da400p-127
	fmv.s.x f1, t0
	li t0, 0x3c602eb6	 #0x1.c05d6c00p-7
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3852cb3	 #0x1.0a596600p-120
	beq t0, a0, L2836
	ret
L2836:


	# Test 2838
	# b32/ =0 i +0.7FFFFFP-126 +1.7BCEFCP-38 -> +1.02216FP-89 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x2cfbcefc	 #0x1.f79df800p-38
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1302216f	 #0x1.0442de00p-89
	beq t0, a0, L2837
	ret
L2837:


	# Test 2839
	# b32/ =0 i +1.000000P-126 +1.076E63P-64 -> +1.71F3FFP-63 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1f876e63	 #0x1.0edcc600p-64
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2071f3ff	 #0x1.e3e7fe00p-63
	beq t0, a0, L2838
	ret
L2838:


	# Test 2840
	# b32/ =0 i +1.3216A0P15 +1.68C4CDP-46 -> +1.43DCC0P60 x


	addi a1, a1, 1
	li t0, 0x473216a0	 #0x1.642d4000p+15
	fmv.s.x f1, t0
	li t0, 0x28e8c4cd	 #0x1.d1899a00p-46
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x5dc3dcc0	 #0x1.87b98000p+60
	beq t0, a0, L2839
	ret
L2839:


	# Test 2841
	# b32/ =0 i +1.7FFFFFP127 +1.65D978P-51 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x2665d978	 #0x1.cbb2f000p-51
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2840
	ret
L2840:


	# Test 2842
	# b32/ =0 i +Inf +1.5C3C34P84 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x69dc3c34	 #0x1.b8786800p+84
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2841
	ret
L2841:


	# Test 2843
	# b32/ =0 i -Inf +1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2842
	ret
L2842:


	# Test 2844
	# b32/ =0 i -1.7FFFFFP127 +1.7FFFFFP127 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2843
	ret
L2843:


	# Test 2845
	# b32/ =0 i -1.4CCC7EP100 +1.7FFFFFP127 -> -1.4CCC7FP-28 x


	addi a1, a1, 1
	li t0, 0xf1cccc7e	 #-0x1.9998fc00p+100
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb1cccc7f	 #-0x1.9998fe00p-28
	beq t0, a0, L2844
	ret
L2844:


	# Test 2846
	# b32/ =0 i -1.000000P-126 +1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2845
	ret
L2845:


	# Test 2847
	# b32/ =0 i -0.7FFFFFP-126 +1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2846
	ret
L2846:


	# Test 2848
	# b32/ =0 i -0.5EFB81P-126 +1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x805efb81	 #-0x1.7bee0400p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2847
	ret
L2847:


	# Test 2849
	# b32/ =0 i -0.000001P-126 +1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2848
	ret
L2848:


	# Test 2850
	# b32/ =0 i -1.000000P0 +1.7FFFFFP127 -> -0.200000P-126 xu


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80200000	 #-0x1.00000000p-128
	beq t0, a0, L2849
	ret
L2849:


	# Test 2851
	# b32/ =0 i -Zero +1.7FFFFFP127 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2850
	ret
L2850:


	# Test 2852
	# b32/ =0 i +Zero +1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2851
	ret
L2851:


	# Test 2853
	# b32/ =0 i +1.000000P0 +1.7FFFFFP127 -> +0.200000P-126 xu


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x200000	 #0x1.00000000p-128
	beq t0, a0, L2852
	ret
L2852:


	# Test 2854
	# b32/ =0 i +0.000001P-126 +1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2853
	ret
L2853:


	# Test 2855
	# b32/ =0 i +0.430425P-126 +1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x430425	 #0x1.0c109400p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2854
	ret
L2854:


	# Test 2856
	# b32/ =0 i +0.7FFFFFP-126 +1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2855
	ret
L2855:


	# Test 2857
	# b32/ =0 i +1.000000P-126 +1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2856
	ret
L2856:


	# Test 2858
	# b32/ =0 i +1.157328P-109 +1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x9157328	 #0x1.2ae65000p-109
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2857
	ret
L2857:


	# Test 2859
	# b32/ =0 i +1.7FFFFFP127 +1.7FFFFFP127 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2858
	ret
L2858:


	# Test 2860
	# b32/ =0 i +Inf +1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2859
	ret
L2859:


	# Test 2861
	# b32/ =0 i -1.7FFFFFP127 +Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2860
	ret
L2860:


	# Test 2862
	# b32/ =0 i -1.1BF139P99 +Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0xf11bf139	 #-0x1.37e27200p+99
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2861
	ret
L2861:


	# Test 2863
	# b32/ =0 i -1.000000P-126 +Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2862
	ret
L2862:


	# Test 2864
	# b32/ =0 i -0.7FFFFFP-126 +Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2863
	ret
L2863:


	# Test 2865
	# b32/ =0 i -0.6D603DP-126 +Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x806d603d	 #-0x1.b580f400p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2864
	ret
L2864:


	# Test 2866
	# b32/ =0 i -0.000001P-126 +Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2865
	ret
L2865:


	# Test 2867
	# b32/ =0 i -1.000000P0 +Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2866
	ret
L2866:


	# Test 2868
	# b32/ =0 i -Zero +Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2867
	ret
L2867:


	# Test 2869
	# b32/ =0 i +Zero +Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2868
	ret
L2868:


	# Test 2870
	# b32/ =0 i +1.000000P0 +Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2869
	ret
L2869:


	# Test 2871
	# b32/ =0 i +0.000001P-126 +Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2870
	ret
L2870:


	# Test 2872
	# b32/ =0 i +0.6700F8P-126 +Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x6700f8	 #0x1.9c03e000p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2871
	ret
L2871:


	# Test 2873
	# b32/ =0 i +0.7FFFFFP-126 +Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2872
	ret
L2872:


	# Test 2874
	# b32/ =0 i +1.000000P-126 +Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2873
	ret
L2873:


	# Test 2875
	# b32/ =0 i +1.6397E4P-111 +Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x86397e4	 #0x1.c72fc800p-111
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2874
	ret
L2874:


	# Test 2876
	# b32/ =0 i +1.7FFFFFP127 +Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7f800000	 #inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2875
	ret
L2875:


	# Test 2877
	# b32/ =0 -Inf -Inf -> q i


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2876
	ret
L2876:


	# Test 2878
	# b32/ =0 -1.7FFFFFP127 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2877
	ret
L2877:


	# Test 2879
	# b32/ =0 -1.42E78FP-100 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x8dc2e78f	 #-0x1.85cf1e00p-100
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2878
	ret
L2878:


	# Test 2880
	# b32/ =0 -1.000000P-126 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2879
	ret
L2879:


	# Test 2881
	# b32/ =0 -0.7FFFFFP-126 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2880
	ret
L2880:


	# Test 2882
	# b32/ =0 -0.141693P-126 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x80141693	 #-0x1.41693000p-129
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2881
	ret
L2881:


	# Test 2883
	# b32/ =0 -0.000001P-126 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2882
	ret
L2882:


	# Test 2884
	# b32/ =0 -1.000000P0 -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2883
	ret
L2883:


	# Test 2885
	# b32/ =0 -Zero -Inf -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2884
	ret
L2884:


	# Test 2886
	# b32/ =0 +Zero -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2885
	ret
L2885:


	# Test 2887
	# b32/ =0 +1.000000P0 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2886
	ret
L2886:


	# Test 2888
	# b32/ =0 +0.000001P-126 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2887
	ret
L2887:


	# Test 2889
	# b32/ =0 +0.051F36P-126 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x51f36	 #0x1.47cd8000p-131
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2888
	ret
L2888:


	# Test 2890
	# b32/ =0 +0.7FFFFFP-126 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2889
	ret
L2889:


	# Test 2891
	# b32/ =0 +1.000000P-126 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2890
	ret
L2890:


	# Test 2892
	# b32/ =0 +1.77566DP5 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x4277566d	 #0x1.eeacda00p+5
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2891
	ret
L2891:


	# Test 2893
	# b32/ =0 +1.7FFFFFP127 -Inf -> -Zero 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2892
	ret
L2892:


	# Test 2894
	# b32/ =0 +Inf -Inf -> q i


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2893
	ret
L2893:


	# Test 2895
	# b32/ =0 q -Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2894
	ret
L2894:


	# Test 2896
	# b32/ =0 q -Inf -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff800000	 #-inf
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2895
	ret
L2895:


	# Test 2897
	# b32/ =0 -Inf -1.7FFFFFP127 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2896
	ret
L2896:


	# Test 2898
	# b32/ =0 -1.7FFFFFP127 -1.7FFFFFP127 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2897
	ret
L2897:


	# Test 2899
	# b32/ =0 -1.110C4BP27 -1.7FFFFFP127 -> +1.110C4CP-101 x


	addi a1, a1, 1
	li t0, 0xcd110c4b	 #-0x1.22189600p+27
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xd110c4c	 #0x1.22189800p-101
	beq t0, a0, L2898
	ret
L2898:


	# Test 2900
	# b32/ =0 -1.000000P-126 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2899
	ret
L2899:


	# Test 2901
	# b32/ =0 -0.7FFFFFP-126 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2900
	ret
L2900:


	# Test 2902
	# b32/ =0 -0.0B7B4EP-126 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x800b7b4e	 #-0x1.6f69c000p-130
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2901
	ret
L2901:


	# Test 2903
	# b32/ =0 -0.000001P-126 -1.7FFFFFP127 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2902
	ret
L2902:


	# Test 2904
	# b32/ =0 -1.000000P0 -1.7FFFFFP127 -> +0.200000P-126 xu


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x200000	 #0x1.00000000p-128
	beq t0, a0, L2903
	ret
L2903:


	# Test 2905
	# b32/ =0 -Zero -1.7FFFFFP127 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2904
	ret
L2904:


	# Test 2906
	# b32/ =0 +Zero -1.7FFFFFP127 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2905
	ret
L2905:


	# Test 2907
	# b32/ =0 +1.000000P0 -1.7FFFFFP127 -> -0.200000P-126 xu


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80200000	 #-0x1.00000000p-128
	beq t0, a0, L2906
	ret
L2906:


	# Test 2908
	# b32/ =0 +0.000001P-126 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2907
	ret
L2907:


	# Test 2909
	# b32/ =0 +0.4803F2P-126 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x4803f2	 #0x1.200fc800p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2908
	ret
L2908:


	# Test 2910
	# b32/ =0 +0.7FFFFFP-126 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2909
	ret
L2909:


	# Test 2911
	# b32/ =0 +1.000000P-126 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2910
	ret
L2910:


	# Test 2912
	# b32/ =0 +1.59B2F5P-39 -1.7FFFFFP127 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x2c59b2f5	 #0x1.b365ea00p-39
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2911
	ret
L2911:


	# Test 2913
	# b32/ =0 +1.7FFFFFP127 -1.7FFFFFP127 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2912
	ret
L2912:


	# Test 2914
	# b32/ =0 +Inf -1.7FFFFFP127 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2913
	ret
L2913:


	# Test 2915
	# b32/ =0 q -1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2914
	ret
L2914:


	# Test 2916
	# b32/ =0 q -1.7FFFFFP127 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2915
	ret
L2915:


	# Test 2917
	# b32/ =0 -Inf -1.356AEDP-121 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x83356aed	 #-0x1.6ad5da00p-121
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2916
	ret
L2916:


	# Test 2918
	# b32/ =0 -1.7FFFFFP127 -1.2B4DA9P78 -> +1.3F4950P49 x


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xe6ab4da9	 #-0x1.569b5200p+78
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x583f4950	 #0x1.7e92a000p+49
	beq t0, a0, L2917
	ret
L2917:


	# Test 2919
	# b32/ =0 -1.603106P9 -1.58BDE8P15 -> +1.046647P-6 x


	addi a1, a1, 1
	li t0, 0xc4603106	 #-0x1.c0620c00p+9
	fmv.s.x f1, t0
	li t0, 0xc758bde8	 #-0x1.b17bd000p+15
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3c846647	 #0x1.08cc8e00p-6
	beq t0, a0, L2918
	ret
L2918:


	# Test 2920
	# b32/ =0 -1.000000P-126 -1.0A38BFP28 -> +Zero xu


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xcd8a38bf	 #-0x1.14717e00p+28
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2919
	ret
L2919:


	# Test 2921
	# b32/ =0 -0.7FFFFFP-126 -1.009B7BP-61 -> +1.7ECA80P-66 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xa1009b7b	 #-0x1.0136f600p-61
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1efeca80	 #0x1.fd950000p-66
	beq t0, a0, L2920
	ret
L2920:


	# Test 2922
	# b32/ =0 -0.03A00AP-126 -1.70FAB9P9 -> +0.0000F6P-126 xu


	addi a1, a1, 1
	li t0, 0x8003a00a	 #-0x1.d0050000p-132
	fmv.s.x f1, t0
	li t0, 0xc470fab9	 #-0x1.e1f57200p+9
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xf6	 #0x1.ec000000p-142
	beq t0, a0, L2921
	ret
L2921:


	# Test 2923
	# b32/ =0 -0.000001P-126 -1.5FC690P-112 -> +1.126EB1P-38 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x87dfc690	 #-0x1.bf8d2000p-112
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x2c926eb1	 #0x1.24dd6200p-38
	beq t0, a0, L2922
	ret
L2922:


	# Test 2924
	# b32/ =0 -1.000000P0 -1.6AA5F7P-75 -> +1.0BA5B5P74 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x9a6aa5f7	 #-0x1.d54bee00p-75
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x648ba5b5	 #0x1.174b6a00p+74
	beq t0, a0, L2923
	ret
L2923:


	# Test 2925
	# b32/ =0 -Zero -1.6108B3P-36 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xade108b3	 #-0x1.c2116600p-36
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2924
	ret
L2924:


	# Test 2926
	# b32/ =0 +Zero -1.6C281BP-31 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xb06c281b	 #-0x1.d8503600p-31
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2925
	ret
L2925:


	# Test 2927
	# b32/ =0 +1.000000P0 -1.770782P-26 -> -1.04A5F2P25 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xb2f70782	 #-0x1.ee0f0400p-26
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcc04a5f2	 #-0x1.094be400p+25
	beq t0, a0, L2926
	ret
L2926:


	# Test 2928
	# b32/ =0 +0.000001P-126 -1.361685P97 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xf0361685	 #-0x1.6c2d0a00p+97
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2927
	ret
L2927:


	# Test 2929
	# b32/ =0 +0.03C0C5P-126 -1.42C2EDP62 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x3c0c5	 #0x1.e0628000p-132
	fmv.s.x f1, t0
	li t0, 0xdec2c2ed	 #-0x1.8585da00p+62
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2928
	ret
L2928:


	# Test 2930
	# b32/ =0 +0.7FFFFFP-126 -1.4C9553P91 -> -Zero xu


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xed4c9553	 #-0x1.992aa600p+91
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2929
	ret
L2929:


	# Test 2931
	# b32/ =0 +1.000000P-126 -1.6CB166P-34 -> -1.0A70E2P-93 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xaeecb166	 #-0x1.d962cc00p-34
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff910a70e2	 #-0x1.14e1c400p-93
	beq t0, a0, L2930
	ret
L2930:


	# Test 2932
	# b32/ =0 +1.2817B1P-104 -1.065700P-106 -> -1.2028EAP2 x


	addi a1, a1, 1
	li t0, 0xba817b1	 #0x1.502f6200p-104
	fmv.s.x f1, t0
	li t0, 0x8a865700	 #-0x1.0cae0000p-106
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0a028ea	 #-0x1.4051d400p+2
	beq t0, a0, L2931
	ret
L2931:


	# Test 2933
	# b32/ =0 +1.7FFFFFP127 -1.369FD0P126 -> -1.336DB2P1 x


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xfeb69fd0	 #-0x1.6d3fa000p+126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0336db2	 #-0x1.66db6400p+1
	beq t0, a0, L2932
	ret
L2932:


	# Test 2934
	# b32/ =0 +Inf -1.41FF38P51 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xd941ff38	 #-0x1.83fe7000p+51
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2933
	ret
L2933:


	# Test 2935
	# b32/ =0 q -1.0EED17P-121 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x830eed17	 #-0x1.1dda2e00p-121
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2934
	ret
L2934:


	# Test 2936
	# b32/ =0 q -1.4FC91DP65 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xe04fc91d	 #-0x1.9f923a00p+65
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2935
	ret
L2935:


	# Test 2937
	# b32/ =0 -Inf -1.000000P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2936
	ret
L2936:


	# Test 2938
	# b32/ =0 -1.7FFFFFP127 -1.000000P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2937
	ret
L2937:


	# Test 2939
	# b32/ =0 -1.446DDAP55 -1.000000P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xdb446dda	 #-0x1.88dbb400p+55
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2938
	ret
L2938:


	# Test 2940
	# b32/ =0 -1.000000P-126 -1.000000P-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2939
	ret
L2939:


	# Test 2941
	# b32/ =0 -0.7FFFFFP-126 -1.000000P-126 -> +1.7FFFFEP-1 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f7ffffe	 #0x1.fffffc00p-1
	beq t0, a0, L2940
	ret
L2940:


	# Test 2942
	# b32/ =0 -0.4044C5P-126 -1.000000P-126 -> +1.00898AP-1 


	addi a1, a1, 1
	li t0, 0x804044c5	 #-0x1.01131400p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f00898a	 #0x1.01131400p-1
	beq t0, a0, L2941
	ret
L2941:


	# Test 2943
	# b32/ =0 -0.000001P-126 -1.000000P-126 -> +1.000000P-23 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34000000	 #0x1.00000000p-23
	beq t0, a0, L2942
	ret
L2942:


	# Test 2944
	# b32/ =0 -1.000000P0 -1.000000P-126 -> +1.000000P126 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e800000	 #0x1.00000000p+126
	beq t0, a0, L2943
	ret
L2943:


	# Test 2945
	# b32/ =0 -Zero -1.000000P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2944
	ret
L2944:


	# Test 2946
	# b32/ =0 +Zero -1.000000P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2945
	ret
L2945:


	# Test 2947
	# b32/ =0 +1.000000P0 -1.000000P-126 -> -1.000000P126 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe800000	 #-0x1.00000000p+126
	beq t0, a0, L2946
	ret
L2946:


	# Test 2948
	# b32/ =0 +0.000001P-126 -1.000000P-126 -> -1.000000P-23 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4000000	 #-0x1.00000000p-23
	beq t0, a0, L2947
	ret
L2947:


	# Test 2949
	# b32/ =0 +0.7AE581P-126 -1.000000P-126 -> -1.75CB02P-1 


	addi a1, a1, 1
	li t0, 0x7ae581	 #0x1.eb960400p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf75cb02	 #-0x1.eb960400p-1
	beq t0, a0, L2948
	ret
L2948:


	# Test 2950
	# b32/ =0 +0.7FFFFFP-126 -1.000000P-126 -> -1.7FFFFEP-1 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf7ffffe	 #-0x1.fffffc00p-1
	beq t0, a0, L2949
	ret
L2949:


	# Test 2951
	# b32/ =0 +1.000000P-126 -1.000000P-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2950
	ret
L2950:


	# Test 2952
	# b32/ =0 +1.0C5484P-74 -1.000000P-126 -> -1.0C5484P52 


	addi a1, a1, 1
	li t0, 0x1a8c5484	 #0x1.18a90800p-74
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd98c5484	 #-0x1.18a90800p+52
	beq t0, a0, L2951
	ret
L2951:


	# Test 2953
	# b32/ =0 +1.7FFFFFP127 -1.000000P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2952
	ret
L2952:


	# Test 2954
	# b32/ =0 +Inf -1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2953
	ret
L2953:


	# Test 2955
	# b32/ =0 q -1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2954
	ret
L2954:


	# Test 2956
	# b32/ =0 q -1.000000P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2955
	ret
L2955:


	# Test 2957
	# b32/ =0 -Inf -0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2956
	ret
L2956:


	# Test 2958
	# b32/ =0 -1.7FFFFFP127 -0.7FFFFFP-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2957
	ret
L2957:


	# Test 2959
	# b32/ =0 -1.129295P38 -0.7FFFFFP-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xd2929295	 #-0x1.25252a00p+38
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2958
	ret
L2958:


	# Test 2960
	# b32/ =0 -1.000000P-126 -0.7FFFFFP-126 -> +1.000001P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800001	 #0x1.00000200p+0
	beq t0, a0, L2959
	ret
L2959:


	# Test 2961
	# b32/ =0 -0.7FFFFFP-126 -0.7FFFFFP-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L2960
	ret
L2960:


	# Test 2962
	# b32/ =0 -0.640199P-126 -0.7FFFFFP-126 -> +1.480334P-1 x


	addi a1, a1, 1
	li t0, 0x80640199	 #-0x1.90066400p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f480334	 #0x1.90066800p-1
	beq t0, a0, L2961
	ret
L2961:


	# Test 2963
	# b32/ =0 -0.000001P-126 -0.7FFFFFP-126 -> +1.000001P-23 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34000001	 #0x1.00000200p-23
	beq t0, a0, L2962
	ret
L2962:


	# Test 2964
	# b32/ =0 -1.000000P0 -0.7FFFFFP-126 -> +1.000001P126 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e800001	 #0x1.00000200p+126
	beq t0, a0, L2963
	ret
L2963:


	# Test 2965
	# b32/ =0 -Zero -0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2964
	ret
L2964:


	# Test 2966
	# b32/ =0 +Zero -0.7FFFFFP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2965
	ret
L2965:


	# Test 2967
	# b32/ =0 +1.000000P0 -0.7FFFFFP-126 -> -1.000001P126 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe800001	 #-0x1.00000200p+126
	beq t0, a0, L2966
	ret
L2966:


	# Test 2968
	# b32/ =0 +0.000001P-126 -0.7FFFFFP-126 -> -1.000001P-23 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4000001	 #-0x1.00000200p-23
	beq t0, a0, L2967
	ret
L2967:


	# Test 2969
	# b32/ =0 +0.490A3CP-126 -0.7FFFFFP-126 -> -1.121479P-1 x


	addi a1, a1, 1
	li t0, 0x490a3c	 #0x1.2428f000p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf121479	 #-0x1.2428f200p-1
	beq t0, a0, L2968
	ret
L2968:


	# Test 2970
	# b32/ =0 +0.7FFFFFP-126 -0.7FFFFFP-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L2969
	ret
L2969:


	# Test 2971
	# b32/ =0 +1.000000P-126 -0.7FFFFFP-126 -> -1.000001P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800001	 #-0x1.00000200p+0
	beq t0, a0, L2970
	ret
L2970:


	# Test 2972
	# b32/ =0 +1.5B7940P-92 -0.7FFFFFP-126 -> -1.5B7942P34 x


	addi a1, a1, 1
	li t0, 0x11db7940	 #0x1.b6f28000p-92
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffd0db7942	 #-0x1.b6f28400p+34
	beq t0, a0, L2971
	ret
L2971:


	# Test 2973
	# b32/ =0 +1.7FFFFFP127 -0.7FFFFFP-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2972
	ret
L2972:


	# Test 2974
	# b32/ =0 +Inf -0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2973
	ret
L2973:


	# Test 2975
	# b32/ =0 q -0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2974
	ret
L2974:


	# Test 2976
	# b32/ =0 q -0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2975
	ret
L2975:


	# Test 2977
	# b32/ =0 -Inf -0.41EDDAP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x8041edda	 #-0x1.07b76800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2976
	ret
L2976:


	# Test 2978
	# b32/ =0 -1.7FFFFFP127 -0.2CCD41P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x802ccd41	 #-0x1.666a0800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2977
	ret
L2977:


	# Test 2979
	# b32/ =0 -1.752F1DP-119 -0.12EDAEP-126 -> +1.4F405BP10 x


	addi a1, a1, 1
	li t0, 0x84752f1d	 #-0x1.ea5e3a00p-119
	fmv.s.x f1, t0
	li t0, 0x8012edae	 #-0x1.2edae000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x44cf405b	 #0x1.9e80b600p+10
	beq t0, a0, L2978
	ret
L2978:


	# Test 2980
	# b32/ =0 -1.000000P-126 -0.373BABP-126 -> +1.145122P1 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80373bab	 #-0x1.b9dd5800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x40145122	 #0x1.28a24400p+1
	beq t0, a0, L2979
	ret
L2979:


	# Test 2981
	# b32/ =0 -0.7FFFFFP-126 -0.621B12P-126 -> +1.2700E2P0 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80621b12	 #-0x1.886c4800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3fa700e2	 #0x1.4e01c400p+0
	beq t0, a0, L2980
	ret
L2980:


	# Test 2982
	# b32/ =0 -0.732654P-126 -0.1A83C5P-126 -> +1.0AF8B4P2 x


	addi a1, a1, 1
	li t0, 0x80732654	 #-0x1.cc995000p-127
	fmv.s.x f1, t0
	li t0, 0x801a83c5	 #-0x1.a83c5000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x408af8b4	 #0x1.15f16800p+2
	beq t0, a0, L2981
	ret
L2981:


	# Test 2983
	# b32/ =0 -0.000001P-126 -0.639D36P-126 -> +1.247986P-23 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80639d36	 #-0x1.8e74d800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34247986	 #0x1.48f30c00p-23
	beq t0, a0, L2982
	ret
L2982:


	# Test 2984
	# b32/ =0 -1.000000P0 -0.2EFC9DP-126 -> +1.2E58D2P127 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x802efc9d	 #-0x1.77e4e800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f2e58d2	 #0x1.5cb1a400p+127
	beq t0, a0, L2983
	ret
L2983:


	# Test 2985
	# b32/ =0 -Zero -0.42C84BP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8042c84b	 #-0x1.0b212c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L2984
	ret
L2984:


	# Test 2986
	# b32/ =0 +Zero -0.1DA7B2P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x801da7b2	 #-0x1.da7b2000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L2985
	ret
L2985:


	# Test 2987
	# b32/ =0 +1.000000P0 -0.59471AP-126 -> -1.378467P126 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x8059471a	 #-0x1.651c6800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffeb78467	 #-0x1.6f08ce00p+126
	beq t0, a0, L2986
	ret
L2986:


	# Test 2988
	# b32/ =0 +0.000001P-126 -0.142681P-126 -> -1.4B4577P-21 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80142681	 #-0x1.42681000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb54b4577	 #-0x1.968aee00p-21
	beq t0, a0, L2987
	ret
L2987:


	# Test 2989
	# b32/ =0 +0.582EF7P-126 -0.38A33BP-126 -> -1.474B04P0 x


	addi a1, a1, 1
	li t0, 0x582ef7	 #0x1.60bbdc00p-127
	fmv.s.x f1, t0
	li t0, 0x8038a33b	 #-0x1.c519d800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbfc74b04	 #-0x1.8e960800p+0
	beq t0, a0, L2988
	ret
L2988:


	# Test 2990
	# b32/ =0 +0.7FFFFFP-126 -0.1E14EBP-126 -> -1.082996P2 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x801e14eb	 #-0x1.e14eb000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0882996	 #-0x1.10532c00p+2
	beq t0, a0, L2989
	ret
L2989:


	# Test 2991
	# b32/ =0 +1.000000P-126 -0.79F453P-126 -> -1.065866P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x8079f453	 #-0x1.e7d14c00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf865866	 #-0x1.0cb0cc00p+0
	beq t0, a0, L2990
	ret
L2990:


	# Test 2992
	# b32/ =0 +1.1485E2P20 -0.3C374DP-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x499485e2	 #0x1.290bc400p+20
	fmv.s.x f1, t0
	li t0, 0x803c374d	 #-0x1.e1ba6800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2991
	ret
L2991:


	# Test 2993
	# b32/ =0 +1.7FFFFFP127 -0.7B7676P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x807b7676	 #-0x1.edd9d800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2992
	ret
L2992:


	# Test 2994
	# b32/ =0 +Inf -0.1E8224P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x801e8224	 #-0x1.e8224000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L2993
	ret
L2993:


	# Test 2995
	# b32/ =0 q -0.240D64P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80240d64	 #-0x1.206b2000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2994
	ret
L2994:


	# Test 2996
	# b32/ =0 q -0.799228P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80799228	 #-0x1.e648a000p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L2995
	ret
L2995:


	# Test 2997
	# b32/ =0 -Inf -0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2996
	ret
L2996:


	# Test 2998
	# b32/ =0 -1.7FFFFFP127 -0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L2997
	ret
L2997:


	# Test 2999
	# b32/ =0 -1.4453D8P-104 -0.000001P-126 -> +1.4453D8P45 


	addi a1, a1, 1
	li t0, 0x8bc453d8	 #-0x1.88a7b000p-104
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x564453d8	 #0x1.88a7b000p+45
	beq t0, a0, L2998
	ret
L2998:


	# Test 3000
	# b32/ =0 -1.000000P-126 -0.000001P-126 -> +1.000000P23 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4b000000	 #0x1.00000000p+23
	beq t0, a0, L2999
	ret
L2999:


	# Test 3001
	# b32/ =0 -0.7FFFFFP-126 -0.000001P-126 -> +1.7FFFFEP22 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4afffffe	 #0x1.fffffc00p+22
	beq t0, a0, L3000
	ret
L3000:


	# Test 3002
	# b32/ =0 -0.6CF2F7P-126 -0.000001P-126 -> +1.59E5EEP22 


	addi a1, a1, 1
	li t0, 0x806cf2f7	 #-0x1.b3cbdc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4ad9e5ee	 #0x1.b3cbdc00p+22
	beq t0, a0, L3001
	ret
L3001:


	# Test 3003
	# b32/ =0 -0.000001P-126 -0.000001P-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L3002
	ret
L3002:


	# Test 3004
	# b32/ =0 -1.000000P0 -0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3003
	ret
L3003:


	# Test 3005
	# b32/ =0 -Zero -0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L3004
	ret
L3004:


	# Test 3006
	# b32/ =0 +Zero -0.000001P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L3005
	ret
L3005:


	# Test 3007
	# b32/ =0 +1.000000P0 -0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3006
	ret
L3006:


	# Test 3008
	# b32/ =0 +0.000001P-126 -0.000001P-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L3007
	ret
L3007:


	# Test 3009
	# b32/ =0 +0.65B367P-126 -0.000001P-126 -> -1.4B66CEP22 


	addi a1, a1, 1
	li t0, 0x65b367	 #0x1.96cd9c00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcacb66ce	 #-0x1.96cd9c00p+22
	beq t0, a0, L3008
	ret
L3008:


	# Test 3010
	# b32/ =0 +0.7FFFFFP-126 -0.000001P-126 -> -1.7FFFFEP22 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcafffffe	 #-0x1.fffffc00p+22
	beq t0, a0, L3009
	ret
L3009:


	# Test 3011
	# b32/ =0 +1.000000P-126 -0.000001P-126 -> -1.000000P23 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcb000000	 #-0x1.00000000p+23
	beq t0, a0, L3010
	ret
L3010:


	# Test 3012
	# b32/ =0 +1.632A9EP2 -0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x40e32a9e	 #0x1.c6553c00p+2
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3011
	ret
L3011:


	# Test 3013
	# b32/ =0 +1.7FFFFFP127 -0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3012
	ret
L3012:


	# Test 3014
	# b32/ =0 +Inf -0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3013
	ret
L3013:


	# Test 3015
	# b32/ =0 q -0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3014
	ret
L3014:


	# Test 3016
	# b32/ =0 q -0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3015
	ret
L3015:


	# Test 3017
	# b32/ =0 -Inf -1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3016
	ret
L3016:


	# Test 3018
	# b32/ =0 -1.7FFFFFP127 -1.000000P0 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L3017
	ret
L3017:


	# Test 3019
	# b32/ =0 -1.12F894P-105 -1.000000P0 -> +1.12F894P-105 


	addi a1, a1, 1
	li t0, 0x8b12f894	 #-0x1.25f12800p-105
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xb12f894	 #0x1.25f12800p-105
	beq t0, a0, L3018
	ret
L3018:


	# Test 3020
	# b32/ =0 -1.000000P-126 -1.000000P0 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L3019
	ret
L3019:


	# Test 3021
	# b32/ =0 -0.7FFFFFP-126 -1.000000P0 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L3020
	ret
L3020:


	# Test 3022
	# b32/ =0 -0.7B17B2P-126 -1.000000P0 -> +0.7B17B2P-126 


	addi a1, a1, 1
	li t0, 0x807b17b2	 #-0x1.ec5ec800p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7b17b2	 #0x1.ec5ec800p-127
	beq t0, a0, L3021
	ret
L3021:


	# Test 3023
	# b32/ =0 -0.000001P-126 -1.000000P0 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L3022
	ret
L3022:


	# Test 3024
	# b32/ =0 -1.000000P0 -1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L3023
	ret
L3023:


	# Test 3025
	# b32/ =0 -Zero -1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L3024
	ret
L3024:


	# Test 3026
	# b32/ =0 +Zero -1.000000P0 -> -Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L3025
	ret
L3025:


	# Test 3027
	# b32/ =0 +1.000000P0 -1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L3026
	ret
L3026:


	# Test 3028
	# b32/ =0 +0.000001P-126 -1.000000P0 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L3027
	ret
L3027:


	# Test 3029
	# b32/ =0 +0.05F03BP-126 -1.000000P0 -> -0.05F03BP-126 


	addi a1, a1, 1
	li t0, 0x5f03b	 #0x1.7c0ec000p-131
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff8005f03b	 #-0x1.7c0ec000p-131
	beq t0, a0, L3028
	ret
L3028:


	# Test 3030
	# b32/ =0 +0.7FFFFFP-126 -1.000000P0 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L3029
	ret
L3029:


	# Test 3031
	# b32/ =0 +1.000000P-126 -1.000000P0 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L3030
	ret
L3030:


	# Test 3032
	# b32/ =0 +1.318F59P-15 -1.000000P0 -> -1.318F59P-15 


	addi a1, a1, 1
	li t0, 0x38318f59	 #0x1.631eb200p-15
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb8318f59	 #-0x1.631eb200p-15
	beq t0, a0, L3031
	ret
L3031:


	# Test 3033
	# b32/ =0 +1.7FFFFFP127 -1.000000P0 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L3032
	ret
L3032:


	# Test 3034
	# b32/ =0 +Inf -1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3033
	ret
L3033:


	# Test 3035
	# b32/ =0 q -1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3034
	ret
L3034:


	# Test 3036
	# b32/ =0 q -1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3035
	ret
L3035:


	# Test 3037
	# b32/ =0 -Inf -Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3036
	ret
L3036:


	# Test 3038
	# b32/ =0 -1.7FFFFFP127 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3037
	ret
L3037:


	# Test 3039
	# b32/ =0 -1.619D4FP5 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0xc2619d4f	 #-0x1.c33a9e00p+5
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3038
	ret
L3038:


	# Test 3040
	# b32/ =0 -1.000000P-126 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3039
	ret
L3039:


	# Test 3041
	# b32/ =0 -0.7FFFFFP-126 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3040
	ret
L3040:


	# Test 3042
	# b32/ =0 -0.4A3C6DP-126 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x804a3c6d	 #-0x1.28f1b400p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3041
	ret
L3041:


	# Test 3043
	# b32/ =0 -0.000001P-126 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3042
	ret
L3042:


	# Test 3044
	# b32/ =0 -1.000000P0 -Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3043
	ret
L3043:


	# Test 3045
	# b32/ =0 -Zero -Zero -> q i


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3044
	ret
L3044:


	# Test 3046
	# b32/ =0 +Zero -Zero -> q i


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3045
	ret
L3045:


	# Test 3047
	# b32/ =0 +1.000000P0 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3046
	ret
L3046:


	# Test 3048
	# b32/ =0 +0.000001P-126 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3047
	ret
L3047:


	# Test 3049
	# b32/ =0 +0.5814F6P-126 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x5814f6	 #0x1.6053d800p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3048
	ret
L3048:


	# Test 3050
	# b32/ =0 +0.7FFFFFP-126 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3049
	ret
L3049:


	# Test 3051
	# b32/ =0 +1.000000P-126 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3050
	ret
L3050:


	# Test 3052
	# b32/ =0 +1.15CC2DP119 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x7b15cc2d	 #0x1.2b985a00p+119
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3051
	ret
L3051:


	# Test 3053
	# b32/ =0 +1.7FFFFFP127 -Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3052
	ret
L3052:


	# Test 3054
	# b32/ =0 +Inf -Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3053
	ret
L3053:


	# Test 3055
	# b32/ =0 q -Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3054
	ret
L3054:


	# Test 3056
	# b32/ =0 q -Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3055
	ret
L3055:


	# Test 3057
	# b32/ =0 -Inf +Zero -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3056
	ret
L3056:


	# Test 3058
	# b32/ =0 -1.7FFFFFP127 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3057
	ret
L3057:


	# Test 3059
	# b32/ =0 -1.30420AP52 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0xd9b0420a	 #-0x1.60841400p+52
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3058
	ret
L3058:


	# Test 3060
	# b32/ =0 -1.000000P-126 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3059
	ret
L3059:


	# Test 3061
	# b32/ =0 -0.7FFFFFP-126 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3060
	ret
L3060:


	# Test 3062
	# b32/ =0 -0.6DF941P-126 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x806df941	 #-0x1.b7e50400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3061
	ret
L3061:


	# Test 3063
	# b32/ =0 -0.000001P-126 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3062
	ret
L3062:


	# Test 3064
	# b32/ =0 -1.000000P0 +Zero -> -Inf z


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3063
	ret
L3063:


	# Test 3065
	# b32/ =0 -Zero +Zero -> q i


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3064
	ret
L3064:


	# Test 3066
	# b32/ =0 +Zero +Zero -> q i


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3065
	ret
L3065:


	# Test 3067
	# b32/ =0 +1.000000P0 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3066
	ret
L3066:


	# Test 3068
	# b32/ =0 +0.000001P-126 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3067
	ret
L3067:


	# Test 3069
	# b32/ =0 +0.66B9B1P-126 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x66b9b1	 #0x1.9ae6c400p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3068
	ret
L3068:


	# Test 3070
	# b32/ =0 +0.7FFFFFP-126 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3069
	ret
L3069:


	# Test 3071
	# b32/ =0 +1.000000P-126 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3070
	ret
L3070:


	# Test 3072
	# b32/ =0 +1.64F0E8P-51 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x2664f0e8	 #0x1.c9e1d000p-51
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3071
	ret
L3071:


	# Test 3073
	# b32/ =0 +1.7FFFFFP127 +Zero -> +Inf z


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3072
	ret
L3072:


	# Test 3074
	# b32/ =0 +Inf +Zero -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3073
	ret
L3073:


	# Test 3075
	# b32/ =0 q +Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3074
	ret
L3074:


	# Test 3076
	# b32/ =0 q +Zero -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3075
	ret
L3075:


	# Test 3077
	# b32/ =0 -Inf +1.000000P0 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3076
	ret
L3076:


	# Test 3078
	# b32/ =0 -1.7FFFFFP127 +1.000000P0 -> -1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff7fffff	 #-0x1.fffffe00p+127
	beq t0, a0, L3077
	ret
L3077:


	# Test 3079
	# b32/ =0 -1.7EA6C6P66 +1.000000P0 -> -1.7EA6C6P66 


	addi a1, a1, 1
	li t0, 0xe0fea6c6	 #-0x1.fd4d8c00p+66
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffe0fea6c6	 #-0x1.fd4d8c00p+66
	beq t0, a0, L3078
	ret
L3078:


	# Test 3080
	# b32/ =0 -1.000000P-126 +1.000000P0 -> -1.000000P-126 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80800000	 #-0x1.00000000p-126
	beq t0, a0, L3079
	ret
L3079:


	# Test 3081
	# b32/ =0 -0.7FFFFFP-126 +1.000000P0 -> -0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807fffff	 #-0x1.fffffc00p-127
	beq t0, a0, L3080
	ret
L3080:


	# Test 3082
	# b32/ =0 -0.7C9DFCP-126 +1.000000P0 -> -0.7C9DFCP-126 


	addi a1, a1, 1
	li t0, 0x807c9dfc	 #-0x1.f277f000p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff807c9dfc	 #-0x1.f277f000p-127
	beq t0, a0, L3081
	ret
L3081:


	# Test 3083
	# b32/ =0 -0.000001P-126 +1.000000P0 -> -0.000001P-126 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000001	 #-0x1.00000000p-149
	beq t0, a0, L3082
	ret
L3082:


	# Test 3084
	# b32/ =0 -1.000000P0 +1.000000P0 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L3083
	ret
L3083:


	# Test 3085
	# b32/ =0 -Zero +1.000000P0 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L3084
	ret
L3084:


	# Test 3086
	# b32/ =0 +Zero +1.000000P0 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L3085
	ret
L3085:


	# Test 3087
	# b32/ =0 +1.000000P0 +1.000000P0 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L3086
	ret
L3086:


	# Test 3088
	# b32/ =0 +0.000001P-126 +1.000000P0 -> +0.000001P-126 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x1	 #0x1.00000000p-149
	beq t0, a0, L3087
	ret
L3087:


	# Test 3089
	# b32/ =0 +0.75DE6CP-126 +1.000000P0 -> +0.75DE6CP-126 


	addi a1, a1, 1
	li t0, 0x75de6c	 #0x1.d779b000p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x75de6c	 #0x1.d779b000p-127
	beq t0, a0, L3088
	ret
L3088:


	# Test 3090
	# b32/ =0 +0.7FFFFFP-126 +1.000000P0 -> +0.7FFFFFP-126 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	beq t0, a0, L3089
	ret
L3089:


	# Test 3091
	# b32/ =0 +1.000000P-126 +1.000000P0 -> +1.000000P-126 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x800000	 #0x1.00000000p-126
	beq t0, a0, L3090
	ret
L3090:


	# Test 3092
	# b32/ =0 +1.3315A3P-4 +1.000000P0 -> +1.3315A3P-4 


	addi a1, a1, 1
	li t0, 0x3db315a3	 #0x1.662b4600p-4
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3db315a3	 #0x1.662b4600p-4
	beq t0, a0, L3091
	ret
L3091:


	# Test 3093
	# b32/ =0 +1.7FFFFFP127 +1.000000P0 -> +1.7FFFFFP127 


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	beq t0, a0, L3092
	ret
L3092:


	# Test 3094
	# b32/ =0 +Inf +1.000000P0 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3093
	ret
L3093:


	# Test 3095
	# b32/ =0 q +1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3094
	ret
L3094:


	# Test 3096
	# b32/ =0 q +1.000000P0 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3095
	ret
L3095:


	# Test 3097
	# b32/ =0 -Inf +0.000001P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3096
	ret
L3096:


	# Test 3098
	# b32/ =0 -1.7FFFFFP127 +0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3097
	ret
L3097:


	# Test 3099
	# b32/ =0 -1.4DCB81P-47 +0.000001P-126 -> -1.4DCB81P102 


	addi a1, a1, 1
	li t0, 0xa84dcb81	 #-0x1.9b970200p-47
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff2cdcb81	 #-0x1.9b970200p+102
	beq t0, a0, L3098
	ret
L3098:


	# Test 3100
	# b32/ =0 -1.000000P-126 +0.000001P-126 -> -1.000000P23 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcb000000	 #-0x1.00000000p+23
	beq t0, a0, L3099
	ret
L3099:


	# Test 3101
	# b32/ =0 -0.7FFFFFP-126 +0.000001P-126 -> -1.7FFFFEP22 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffcafffffe	 #-0x1.fffffc00p+22
	beq t0, a0, L3100
	ret
L3100:


	# Test 3102
	# b32/ =0 -0.4B02B8P-126 +0.000001P-126 -> -1.160570P22 


	addi a1, a1, 1
	li t0, 0x804b02b8	 #-0x1.2c0ae000p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffca960570	 #-0x1.2c0ae000p+22
	beq t0, a0, L3101
	ret
L3101:


	# Test 3103
	# b32/ =0 -0.000001P-126 +0.000001P-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L3102
	ret
L3102:


	# Test 3104
	# b32/ =0 -1.000000P0 +0.000001P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3103
	ret
L3103:


	# Test 3105
	# b32/ =0 -Zero +0.000001P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L3104
	ret
L3104:


	# Test 3106
	# b32/ =0 +Zero +0.000001P-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L3105
	ret
L3105:


	# Test 3107
	# b32/ =0 +1.000000P0 +0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3106
	ret
L3106:


	# Test 3108
	# b32/ =0 +0.000001P-126 +0.000001P-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L3107
	ret
L3107:


	# Test 3109
	# b32/ =0 +0.59DB40P-126 +0.000001P-126 -> +1.33B680P22 


	addi a1, a1, 1
	li t0, 0x59db40	 #0x1.676d0000p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4ab3b680	 #0x1.676d0000p+22
	beq t0, a0, L3108
	ret
L3108:


	# Test 3110
	# b32/ =0 +0.7FFFFFP-126 +0.000001P-126 -> +1.7FFFFEP22 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4afffffe	 #0x1.fffffc00p+22
	beq t0, a0, L3109
	ret
L3109:


	# Test 3111
	# b32/ =0 +1.000000P-126 +0.000001P-126 -> +1.000000P23 


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x4b000000	 #0x1.00000000p+23
	beq t0, a0, L3110
	ret
L3110:


	# Test 3112
	# b32/ =0 +1.023A5FP-21 +0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x35023a5f	 #0x1.0474be00p-21
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3111
	ret
L3111:


	# Test 3113
	# b32/ =0 +1.7FFFFFP127 +0.000001P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3112
	ret
L3112:


	# Test 3114
	# b32/ =0 +Inf +0.000001P-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3113
	ret
L3113:


	# Test 3115
	# b32/ =0 q +0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3114
	ret
L3114:


	# Test 3116
	# b32/ =0 q +0.000001P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3115
	ret
L3115:


	# Test 3117
	# b32/ =0 -Inf +0.734749P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x734749	 #0x1.cd1d2400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3116
	ret
L3116:


	# Test 3118
	# b32/ =0 -1.7FFFFFP127 +0.4752F7P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x4752f7	 #0x1.1d4bdc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3117
	ret
L3117:


	# Test 3119
	# b32/ =0 -1.1C703CP0 +0.25B90DP-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xbf9c703c	 #-0x1.38e07800p+0
	fmv.s.x f1, t0
	li t0, 0x25b90d	 #0x1.2dc86800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3118
	ret
L3118:


	# Test 3120
	# b32/ =0 -1.000000P-126 +0.720E71P-126 -> -1.0FA5F1P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x720e71	 #0x1.c839c400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf8fa5f1	 #-0x1.1f4be200p+0
	beq t0, a0, L3119
	ret
L3119:


	# Test 3121
	# b32/ =0 -0.7FFFFFP-126 +0.28F12DP-126 -> -1.481663P1 x


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x28f12d	 #0x1.47896800p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffc0481663	 #-0x1.902cc600p+1
	beq t0, a0, L3120
	ret
L3120:


	# Test 3122
	# b32/ =0 -0.3A6773P-126 +0.2E4F24P-126 -> -1.216E4DP0 x


	addi a1, a1, 1
	li t0, 0x803a6773	 #-0x1.d33b9800p-128
	fmv.s.x f1, t0
	li t0, 0x2e4f24	 #0x1.72792000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbfa16e4d	 #-0x1.42dc9a00p+0
	beq t0, a0, L3121
	ret
L3121:


	# Test 3123
	# b32/ =0 -0.000001P-126 +0.179C42P-126 -> -1.2D7BA6P-21 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x179c42	 #0x1.79c42000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb52d7ba6	 #-0x1.5af74c00p-21
	beq t0, a0, L3122
	ret
L3122:


	# Test 3124
	# b32/ =0 -1.000000P0 +0.7DBEFEP-126 -> -1.024B5AP126 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7dbefe	 #0x1.f6fbf800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe824b5a	 #-0x1.0496b400p+126
	beq t0, a0, L3123
	ret
L3123:


	# Test 3125
	# b32/ =0 -Zero +0.295E66P-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x295e66	 #0x1.4af33000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L3124
	ret
L3124:


	# Test 3126
	# b32/ =0 +Zero +0.547DCDP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x547dcd	 #0x1.51f73400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L3125
	ret
L3125:


	# Test 3127
	# b32/ =0 +1.000000P0 +0.17C97BP-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x17c97b	 #0x1.7c97b000p-129
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3126
	ret
L3126:


	# Test 3128
	# b32/ =0 +0.000001P-126 +0.5E2C37P-126 -> +1.2DFA6CP-23 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x5e2c37	 #0x1.78b0dc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x342dfa6c	 #0x1.5bf4d800p-23
	beq t0, a0, L3127
	ret
L3127:


	# Test 3129
	# b32/ =0 +0.687FFCP-126 +0.6FBE12P-126 -> +1.6F6852P-1 x


	addi a1, a1, 1
	li t0, 0x687ffc	 #0x1.a1fff000p-127
	fmv.s.x f1, t0
	li t0, 0x6fbe12	 #0x1.bef84800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f6f6852	 #0x1.ded0a400p-1
	beq t0, a0, L3128
	ret
L3128:


	# Test 3130
	# b32/ =0 +0.7FFFFFP-126 +0.74EB06P-126 -> +1.0C21DFP0 x


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x74eb06	 #0x1.d3ac1800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f8c21df	 #0x1.1843be00p+0
	beq t0, a0, L3129
	ret
L3129:


	# Test 3131
	# b32/ =0 +1.000000P-126 +0.2B0DC2P-126 -> +1.3E4619P1 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x2b0dc2	 #0x1.586e1000p-128
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x403e4619	 #0x1.7c8c3200p+1
	beq t0, a0, L3130
	ret
L3130:


	# Test 3132
	# b32/ =0 +1.65B732P-40 +0.779429P-126 -> +1.75E4A7P86 x


	addi a1, a1, 1
	li t0, 0x2be5b732	 #0x1.cb6e6400p-40
	fmv.s.x f1, t0
	li t0, 0x779429	 #0x1.de50a400p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6af5e4a7	 #0x1.ebc94e00p+86
	beq t0, a0, L3131
	ret
L3131:


	# Test 3133
	# b32/ =0 +1.7FFFFFP127 +0.0EF583P-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0xef583	 #0x1.deb06000p-130
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3132
	ret
L3132:


	# Test 3134
	# b32/ =0 +Inf +0.69D4EAP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x69d4ea	 #0x1.a753a800p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3133
	ret
L3133:


	# Test 3135
	# b32/ =0 q +0.7B283BP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7b283b	 #0x1.eca0ec00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3134
	ret
L3134:


	# Test 3136
	# b32/ =0 q +0.04C441P-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x4c441	 #0x1.31104000p-131
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3135
	ret
L3135:


	# Test 3137
	# b32/ =0 -Inf +0.7FFFFFP-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3136
	ret
L3136:


	# Test 3138
	# b32/ =0 -1.7FFFFFP127 +0.7FFFFFP-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3137
	ret
L3137:


	# Test 3139
	# b32/ =0 -1.002D10P-18 +0.7FFFFFP-126 -> -1.002D11P108 x


	addi a1, a1, 1
	li t0, 0xb6802d10	 #-0x1.005a2000p-18
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffff5802d11	 #-0x1.005a2200p+108
	beq t0, a0, L3138
	ret
L3138:


	# Test 3140
	# b32/ =0 -1.000000P-126 +0.7FFFFFP-126 -> -1.000001P0 x


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800001	 #-0x1.00000200p+0
	beq t0, a0, L3139
	ret
L3139:


	# Test 3141
	# b32/ =0 -0.7FFFFFP-126 +0.7FFFFFP-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L3140
	ret
L3140:


	# Test 3142
	# b32/ =0 -0.7CC3FBP-126 +0.7FFFFFP-126 -> -1.7987F8P-1 x


	addi a1, a1, 1
	li t0, 0x807cc3fb	 #-0x1.f30fec00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf7987f8	 #-0x1.f30ff000p-1
	beq t0, a0, L3141
	ret
L3141:


	# Test 3143
	# b32/ =0 -0.000001P-126 +0.7FFFFFP-126 -> -1.000001P-23 x


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4000001	 #-0x1.00000200p-23
	beq t0, a0, L3142
	ret
L3142:


	# Test 3144
	# b32/ =0 -1.000000P0 +0.7FFFFFP-126 -> -1.000001P126 x


	addi a1, a1, 1
	li t0, 0xbf800000	 #-0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xfffffffffe800001	 #-0x1.00000200p+126
	beq t0, a0, L3143
	ret
L3143:


	# Test 3145
	# b32/ =0 -Zero +0.7FFFFFP-126 -> -Zero 


	addi a1, a1, 1
	li t0, 0x80000000	 #-0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffff80000000	 #-0x0.00000000p+0
	beq t0, a0, L3144
	ret
L3144:


	# Test 3146
	# b32/ =0 +Zero +0.7FFFFFP-126 -> +Zero 


	addi a1, a1, 1
	li t0, 0x0	 #0x0.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x0	 #0x0.00000000p+0
	beq t0, a0, L3145
	ret
L3145:


	# Test 3147
	# b32/ =0 +1.000000P0 +0.7FFFFFP-126 -> +1.000001P126 x


	addi a1, a1, 1
	li t0, 0x3f800000	 #0x1.00000000p+0
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7e800001	 #0x1.00000200p+126
	beq t0, a0, L3146
	ret
L3146:


	# Test 3148
	# b32/ =0 +0.000001P-126 +0.7FFFFFP-126 -> +1.000001P-23 x


	addi a1, a1, 1
	li t0, 0x1	 #0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x34000001	 #0x1.00000200p-23
	beq t0, a0, L3147
	ret
L3147:


	# Test 3149
	# b32/ =0 +0.76A4B7P-126 +0.7FFFFFP-126 -> +1.6D4970P-1 x


	addi a1, a1, 1
	li t0, 0x76a4b7	 #0x1.da92dc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f6d4970	 #0x1.da92e000p-1
	beq t0, a0, L3148
	ret
L3148:


	# Test 3150
	# b32/ =0 +0.7FFFFFP-126 +0.7FFFFFP-126 -> +1.000000P0 


	addi a1, a1, 1
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800000	 #0x1.00000000p+0
	beq t0, a0, L3149
	ret
L3149:


	# Test 3151
	# b32/ =0 +1.000000P-126 +0.7FFFFFP-126 -> +1.000001P0 x


	addi a1, a1, 1
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x3f800001	 #0x1.00000200p+0
	beq t0, a0, L3150
	ret
L3150:


	# Test 3152
	# b32/ =0 +1.34DBEEP-57 +0.7FFFFFP-126 -> +1.34DBEFP69 x


	addi a1, a1, 1
	li t0, 0x2334dbee	 #0x1.69b7dc00p-57
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x6234dbef	 #0x1.69b7de00p+69
	beq t0, a0, L3151
	ret
L3151:


	# Test 3153
	# b32/ =0 +1.7FFFFFP127 +0.7FFFFFP-126 -> +Inf xo


	addi a1, a1, 1
	li t0, 0x7f7fffff	 #0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3152
	ret
L3152:


	# Test 3154
	# b32/ =0 +Inf +0.7FFFFFP-126 -> +Inf 


	addi a1, a1, 1
	li t0, 0x7f800000	 #inf
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7f800000	 #inf
	beq t0, a0, L3153
	ret
L3153:


	# Test 3155
	# b32/ =0 q +0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3154
	ret
L3154:


	# Test 3156
	# b32/ =0 q +0.7FFFFFP-126 -> q 


	addi a1, a1, 1
	li t0, 0x7fc00000	 #nan
	fmv.s.x f1, t0
	li t0, 0x7fffff	 #0x1.fffffc00p-127
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0x7fc00000	 #nan
	beq t0, a0, L3155
	ret
L3155:


	# Test 3157
	# b32/ =0 -Inf +1.000000P-126 -> -Inf 


	addi a1, a1, 1
	li t0, 0xff800000	 #-inf
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3156
	ret
L3156:


	# Test 3158
	# b32/ =0 -1.7FFFFFP127 +1.000000P-126 -> -Inf xo


	addi a1, a1, 1
	li t0, 0xff7fffff	 #-0x1.fffffe00p+127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffff800000	 #-inf
	beq t0, a0, L3157
	ret
L3157:


	# Test 3159
	# b32/ =0 -1.4F51CBP-36 +1.000000P-126 -> -1.4F51CBP90 


	addi a1, a1, 1
	li t0, 0xadcf51cb	 #-0x1.9ea39600p-36
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffeccf51cb	 #-0x1.9ea39600p+90
	beq t0, a0, L3158
	ret
L3158:


	# Test 3160
	# b32/ =0 -1.000000P-126 +1.000000P-126 -> -1.000000P0 


	addi a1, a1, 1
	li t0, 0x80800000	 #-0x1.00000000p-126
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf800000	 #-0x1.00000000p+0
	beq t0, a0, L3159
	ret
L3159:


	# Test 3161
	# b32/ =0 -0.7FFFFFP-126 +1.000000P-126 -> -1.7FFFFEP-1 


	addi a1, a1, 1
	li t0, 0x807fffff	 #-0x1.fffffc00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf7ffffe	 #-0x1.fffffc00p-1
	beq t0, a0, L3160
	ret
L3160:


	# Test 3162
	# b32/ =0 -0.6080CFP-126 +1.000000P-126 -> -1.41019EP-1 


	addi a1, a1, 1
	li t0, 0x806080cf	 #-0x1.82033c00p-127
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffbf41019e	 #-0x1.82033c00p-1
	beq t0, a0, L3161
	ret
L3161:


	# Test 3163
	# b32/ =0 -0.000001P-126 +1.000000P-126 -> -1.000000P-23 


	addi a1, a1, 1
	li t0, 0x80000001	 #-0x1.00000000p-149
	fmv.s.x f1, t0
	li t0, 0x800000	 #0x1.00000000p-126
	fmv.s.x f2, t0
	csrrci zero, fflags, 0x1f
	fdiv.s f4, f1, f2
	fmv.x.s a0, f4
	li t0, 0xffffffffb4000000	 #-0x1.00000000p-23
	beq t0, a0, L3162
	ret
L3162:


	# Test 3164
	# b32/ =0 -1.000000P0 +1.000000P-126 -> -1.000000P126 


	ret

