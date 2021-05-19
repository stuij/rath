# PandaForth by Torlus (c) 2005
# A 32-bit DTC Forth for ARM, based on
# Bradford J. Rodriguez's CamelForth.
# See the original copyright notice below.
# Some hi-level Forth definitions come from
# other well-known implementations, or articles.
# ===============================================
# CamelForth for the Zilog Z80
# (c) 1994 Bradford J. Rodriguez
# Permission is granted to freely copy, modify,
# and distribute this program for personal or
# educational use.  Commercial inquiries should
# be directed to the author at 221 King St. E.,
# #32, Hamilton, Ontario L8N 1B5 Canada

.section .iwram,"ax",%progbits

.global boot

.extern service

.arm
.align

.set link_0, 0

# ip =  IP
# sp = PSP
# fp = RSP
# r0 =   W
# r1 = TOS
# r2 =  UP
# r3 used for DOES>
# r10 = reserved (info buffer)

.macro codeh label, length, name, prev
.align
.word link_\prev
.word 0
link_\label:
# a "16-bit chars" version could be done this way
# .hword \length
# .irpc p,"\name"
# .byte 0
# .ascii "\p"
# .endr
.byte \length
.ascii "\name"
.align
\label:
.endm

.macro head label, length, name, action, prev
.align
.word link_\prev
.word 0
link_\label:
# a "16-bit chars" version could be done this way
# .hword \length
# .irpc p,"\name"
# .byte 0
# .ascii "\p"
# .endr
.byte \length
.ascii "\name"
.align
\label:
	bl 	\action
.endm

.macro immed label, length, name, action, prev
.align
.word link_\prev
.word 1
link_\label:
# a "16-bit chars" version could be done this way
# .hword \length
# .irpc p,"\name"
# .byte 0
# .ascii "\p"
# .endr
.byte \length
.ascii "\name"
.align
\label:
	bl 	\action
.endm

.macro next
	ldr 	r0, [ip], #4
	add	lr, pc, #4
	bx	r0
.endm

boot:
	b	reset

.string "CODE"
.align

reset:
	sub	ip, pc, #4    /* get the address of reset */
	mov	r10, r0	      /* info buffer */

	/* info buffer */
	/* $+0  : ps_area_end */
	/* $+4  : rs_area_end */
	/* $+8  : user_area */
	/* $+12	: pad_area */

	ldr	sp, [r10]
	ldr	fp, [r10, #4]
	ldr	r2, [r10, #8]

	bl	cold

# INTERPRETER LOGIC =============================
# See also "defining words" at end of this file

#C EXIT     --      exit a colon definition
    codeh exit,4,"exit",0
	ldr	ip, [fp], #4	/* pop old IP from ret stk */
	next

#Z lit      -- x    fetch inline literal to stack
# This is the primtive compiled by LITERAL.
    codeh lit,3,"lit",exit
	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [ip], #4	/* fetch cell at IP to TOS, advancing IP */
	next

#C EXECUTE   i*x xt -- j*x   execute Forth word
#C                           at 'xt'
    codeh execute,7,"execute",lit
	mov	r0, r1		/* address of word -> HL */
	ldr	r1, [sp], #4	/* pop new TOS */

	/*mov	r1, r0
	mov	r0, #0
	bl	service*/

	/*add	pc, lr, #4*/
	bx	r0

# DEFINING WORDS ================================

# ENTER, a.k.a. DOCOLON, entered by CALL ENTER
# to enter a new high-level thread (colon def'n.)
# (internal code fragment, not a Forth word)
# N.B.: DOCOLON must be defined before any
# appearance of 'docolon' in a 'word' macro!
           .type docolon, STT_FUNC
docolon:
enter:
	str	ip, [fp, #-4]!	/* push old IP on ret stack */
	mov	ip, lr		/* param field adrs -> IP */
	next

#C VARIABLE   --      define a Forth variable
#   CREATE 1 CELLS ALLOT ;
# Action of RAM variable is identical to CREATE,
# so we don't need a DOES> clause to change it.
    head variable,8,"variable",docolon,execute
	.word create
	.word lit,1,cells,allot
	/*.word here,lit,0,bdos*/
	.word exit

# DOVAR, code action of VARIABLE, entered by CALL
# DOCREATE, code action of newly created words
.type dovar, STT_FUNC
docreate:
dovar:  /* -- a-addr */
	str	r1, [sp, #-4]!	/* push old TOS */
	mov	r1, lr		/* pfa = variables adrs -> TOS */
	next

#C CONSTANT   n --      define a Forth constant
#   CREATE , DOES> (machine code fragment)
.type docon, STT_FUNC
    head constant,8,"constant",docolon,variable
	.word create,comma,xdoes
# DOCON, code action of CONSTANT,
# entered by CALL DOCON
docon:  /* -- x */
	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [lr]	/* fetch contents of parameter field -> TOS */
	next

#Z USER     n --        define user variable 'n'
#   CREATE , DOES> (machine code fragment)
    head user,4,"user",docolon,constant
	.word create,comma,xdoes
# douser, code action of user,
# entered by call douser
douser:  /* -- a-addr */
	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [lr]	/* fetch contents of parameter field -> TOS */
	add	r1, r1, r2	/* and add offset */
	next

# DODOES, code action of DOES> clause
# entered by       CALL fragment
#                  parameter field
#                       ...
#        fragment: CALL DODOES
#                  high-level thread
# Enters high-level thread with address of
# parameter field on top of stack.
# (internal code fragment, not a Forth word)
dodoes: /* -- a-addr */
/* ######################################################### */
/* DOES> will need to do the following :                     */
/*    - compile (DOES>)                                      */
/*    - compile an instruction like "mov r3, lr"             */
/*    - do ,CF i.e make a "bl ..."                           */
/* This is necessary, as we need the "first lr" for the PFA  */
/* and the "second lr" for the IP                            */
/* The action of (DOES>) remains the same, i.e compile a     */
/* "bl ..." pointing to the address of our "mov r3, lr"      */
/* ######################################################### */
	str	ip, [fp, #-4]!	/* push old IP on ret stack */
	mov	ip, lr		/* adrs of new thread -> IP */
	str	r1, [sp, #-4]!	/* push old TOS */
	mov	r1, r3
	next

#Z BDOS   de c -- a   call CP/M BDOS
    codeh bdos,4,"bdos",user
	mov 	r0, r1		/* TOS = first parameter */
	ldr	r1, [sp], #4	/* pop new TOS, into second parameter */
	stmfd sp!, {r2-r3,r10,fp,ip,lr}
/* TODO System Interface function, most likely to be in C */
	bl	service
	ldmfd sp!, {r2-r3,r10,fp,ip,lr}
/* Return value in r0, so we put in into TOS */
	mov	r1, r0
	next
#C EMIT     c --    output character to console
#   6 BDOS DROP ;
# warning: if c=0ffh, will read one keypress
   head emit,4,"emit",docolon,bdos
	.word lit,0x06,bdos,drop,exit

#Z SAVEKEY  -- addr  temporary storage for KEY?
   head savekey,7,"savekey",dovar,emit
	.word 0

#X KEY?     -- f    return true if char waiting
#_   0FF 6 BDOS DUP SAVEKEY C!    rtns 0 or key
# must use BDOS function 6 to work with KEY
    head querykey,4,"key?",docolon,savekey
        .word lit,0xff,lit,0x06,bdos
        .word dup,savekey,cstore,exit

#C KEY      -- c    get character from keyboard
#   BEGIN SAVEKEY C@ 0= WHILE KEY? DROP REPEAT
#   SAVEKEY C@  0 SAVEKEY C! ;
# must use CP/M direct console I/O to avoid echo
# (BDOS function 6, contained within KEY?)
    head key,3,"key",docolon,querykey
key1:   .word savekey,cfetch,zeroequal,qbranch,key2
        .word querykey,drop,branch,key1
key2:   .word savekey,cfetch,lit,0,savekey,cstore
        .word exit

#X BYE     i*x --    return to CP/M
    codeh bye,3,"bye",key
	b 	reset

# STACK OPERATIONS ==============================

#C DUP      x -- x x      duplicate top of stack
    codeh dup,3,"dup",bye
	str	r1, [sp, #-4]!	/* push TOS */
        next

#C ?DUP     x -- 0 | x x    DUP if nonzero
    codeh qdup,4,"?dup",dup
        cmp	r1, #0
	strne	r1, [sp, #-4]!	/* push TOS */
        next

#C DROP     x --          drop top of stack
    codeh drop,4,"drop",qdup
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#C SWAP     x1 x2 -- x2 x1    swap top two items
    codeh swap,4,"swap",drop
	ldr	r0, [sp]
	str	r1, [sp]
	mov	r1, r0
	next

#C OVER    x1 x2 -- x1 x2 x1   per stack diagram
    codeh over,4,"over",swap
	str	r1, [sp, #-4]!	/* push TOS */
	ldr	r1, [sp, #4]	/* get new TOS */
	next

#C ROT    x1 x2 x3 -- x2 x3 x1  per stack diagram
    codeh rot,3,"rot",over
	ldr	r4, [sp]	/* get x2 */
	str	r1, [sp]	/* x1 x3 x3 */
	ldr	r1, [sp, #4]	/* x1 x3 x1 */
	str	r4, [sp, #4]	/* x2 x3 x1 */
	next

#X NIP    x1 x2 -- x2           per stack diagram
    head nip,3,"nip",docolon,rot
        .word swap,drop,exit

#X TUCK   x1 x2 -- x2 x1 x2     per stack diagram
    head tuck,4,"tuck",docolon,nip
        .word swap,over,exit

#C >R    x --   R: -- x   push to return stack
    codeh tor,2,">r",tuck
	str	r1, [fp, #-4]!	/* push TOS on return stack */
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#C R>    -- x    R: x --   pop from return stack
    codeh rfrom,2,"r>",tor
	str	r1, [sp, #-4]!	/* push TOS */
	ldr	r1, [fp], #4	/* pop new TOS from return stack */
	next

#C R@    -- x     R: x -- x   fetch from rtn stk
    codeh rfetch,2,"r@",rfrom
    	str	r1, [sp, #-4]!	/* push TOS */
	ldr	r1, [fp]	/* get new TOS from return stack */
	next

#Z SP@  -- a-addr       get data stack pointer
    codeh spfetch,3,"sp@",rfetch
	str	r1, [sp, #-4]!	/* push TOS */
	mov	r1, sp
	next

#Z SP!  a-addr --       set data stack pointer
    codeh spstore,3,"sp!",spfetch
	mov	sp, r1
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#Z RP@  -- a-addr       get return stack pointer
    codeh rpfetch,3,"rp@",spstore
	str	r1, [sp, #-4]!	/* push TOS */
	mov	r1, fp
	next

#Z RP!  a-addr --       set return stack pointer
    codeh rpstore,3,"rp!",rpfetch
    	mov	fp, r1
	ldr	r1, [sp], #4	/* pop new TOS */
	next

# MEMORY AND I/O OPERATIONS =====================

#C !        x a-addr --   store cell in memory
    codeh store,1,"!",rpstore
	ldr	r4, [sp], #4	/* read value and update sp */
	str	r4, [r1]
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#Z H!        half h-addr --   store half in memory
    codeh hstore,2,"h!",store
	ldr  r4, [sp], #4	/* read value and update sp */
	strh r4, [r1]
	ldr  r1, [sp], #4	/* pop new TOS */
	next

#C C!      char c-addr --    store char in memory
    codeh cstore,2,"c!",hstore
	ldr	 r4, [sp], #4	/* read value and update sp */
	strb r4, [r1]
	ldr	 r1, [sp], #4	/* pop new TOS */
	next

#Z CH!      char cv-addr --    store char in non-byte addressable memory
    codeh chstore,3,"ch!",cstore
	ldr	r4, [sp], #4	/* read value and update sp */
	and	r4, r4, #0xff
	ands	r5, r1, #1
	subne	r1, r1, #1	/* get halfword-aligned address */
	ldrh	r6, [r1]	/* fetch the halfword */
	andeq	r6, r6, #0xff00 /* mask the corresponding byte */
	movne	r4, r4, lsl #8
	andne	r6, r6, #0x00ff
	orr	r6, r6, r4
	strh	r6, [r1]	/* store the updated halfowrd */
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#C @       a-addr -- x   fetch cell from memory
    codeh fetch,1,"@",chstore
	ldr	r1, [r1]
	next

#Z H@       h-addr -- x   fetch half from memory
    codeh hfetch,2,"h@",fetch
	ldrh r1, [r1]
	next

#C C@     c-addr -- char   fetch char from memory
    codeh cfetch,2,"c@",hfetch
	ldrb	r1, [r1]
	next

#Z CH@     c-addr -- char   fetch char from non-byte addressable memory
# so OAM, palette ram, vram, gamepak flash
    codeh chfetch,3,"ch@",cfetch
	ands	r5, r1, #1
	subne	r1, r1, #1	/* get halfword-aligned address */
	ldrh	r6, [r1]	/* fetch the halfword */
	movne	r6, r6, lsr #8 	/* mask the corresponding byte */
	andeq	r6, r6, #0x00ff
	mov	r1, r6		/* update TOS */
	next

# ARITHMETIC AND LOGICAL OPERATIONS =============

#C +       n1/u1 n2/u2 -- n3/u3     add n1+n2
    codeh plus,1,"+",chfetch
	ldr	r0, [sp], #4	/* read value and update sp */
	add	r1, r0, r1	/* result in TOS */
	next

#X M+       d n -- d         add single to double
    codeh mplus,2,"m+",plus
	ldr	r4, [sp], #4	/* read HI and update sp */
	ldr	r5, [sp]	/* read LO  */
	adds	r5, r5, r1
	addcs	r4, r4, #1
	mov	r1, r4
	str	r5, [sp]	/* HI+c in TOS, LO+n to stack */
	next

#C -      n1/u1 n2/u2 -- n3/u3    subtract n1-n2
    codeh minus,1,"-",mplus
	ldr	r0, [sp], #4	/* read value and update sp */
	sub	r1, r0, r1	/* result in TOS */
	next

#C AND    x1 x2 -- x3            logical AND
    codeh and,3,"and",minus
	ldr	r0, [sp], #4	/* read value and update sp */
	and	r1, r0, r1	/* result in TOS */
	next

#C OR     x1 x2 -- x3           logical OR
    codeh or,2,"or",and
	ldr	r0, [sp], #4	/* read value and update sp */
	orr	r1, r0, r1	/* result in TOS */
	next

#C XOR    x1 x2 -- x3            logical XOR
    codeh xor,3,"xor",or
	ldr	r0, [sp], #4	/* read value and update sp */
	eor	r1, r0, r1	/* result in TOS */
	next

#C INVERT   x1 -- x2            bitwise inversion
    codeh invert,6,"invert",xor
	mvn	r1, r1
	next

#C NEGATE   x1 -- x2            two's complement
    codeh negate,6,"negate",invert
	rsb	r1, r1, #0
	next

#C 1+      n1/u1 -- n2/u2       add 1 to TOS
    codeh oneplus,2,"1+",negate
    	add	r1, r1, #1
    	next

#C 1-      n1/u1 -- n2/u2     subtract 1 from TOS
    codeh oneminus,2,"1-",oneplus
    	sub	r1, r1, #1
    	next

#C 2*      x1 -- x2         arithmetic left shift
    codeh twostar,2,"2*",oneminus
    	mov	r1, r1, asl #1
	next

#C 2/      x1 -- x2        arithmetic right shift
    codeh twoslash,2,"2/",twostar
    	mov	r1, r1, asr #1
    	next

#C LSHIFT  x1 u -- x2    logical L shift u places
    codeh lshift,6,"lshift",twoslash
	ldr	r0, [sp], #4	/* read value and update sp */
	mov	r1, r0, lsl r1
	next

#C RSHIFT  x1 u -- x2    logical R shift u places
    codeh rshift,6,"rshift",lshift
    	ldr	r0, [sp], #4	/* read value and update sp */
	mov	r1, r0, lsr r1
	next

#C +!     n/u a-addr --       add cell to memory
    codeh plusstore,2,"+!",rshift
	ldr	r0, [sp], #4	/* read value and update sp */
	ldr	r4, [r1]
	add	r4, r4, r0
	str	r4, [r1]
	ldr	r1, [sp], #4	/* pop new TOS */
	next

# COMPARISON OPERATIONS =========================

#C 0=     n/u -- flag    return true if TOS=0
    codeh zeroequal,2,"0=",plusstore
	cmp	r1, #0
	mvneq	r1, #0
	movne	r1, #0
	next

#C 0<     n -- flag      true if TOS negative
    codeh zeroless,2,"0<",zeroequal
	cmp r1, #0
	mvnlt	r1, #0
	movge	r1, #0
	next

#C =      x1 x2 -- flag         test x1=x2
    codeh equal,1,"=",zeroless
    	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvneq	r1, #0
	movne	r1, #0
	next

#X <>     x1 x2 -- flag    test not eq (not ANSI)
    codeh notequal,2,"<>",equal
    	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r1, r0
	mvnne	r1, #0
	moveq	r1, #0
	next

#C <      n1 n2 -- flag        test n1<n2, signed
    codeh less,1,"<",notequal
	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvnlt	r1, #0
	movge	r1, #0
	next

#C >     n1 n2 -- flag         test n1>n2, signed
    codeh greater,1,">",less
	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvngt	r1, #0
	movle	r1, #0
	next

#C U<    u1 u2 -- flag       test u1<n2, unsigned
    codeh uless,2,"u<",greater
	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvncc	r1, #0
	movcs	r1, #0
	next

#X U>    u1 u2 -- flag     u1>u2 unsgd (not ANSI)
    codeh ugreater,2,"u>",uless
	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvnhi	r1, #0
	movls	r1, #0
	next

# LOOP AND BRANCH OPERATIONS ====================

#Z branch   --                  branch always
    codeh branch,6,"branch",ugreater
	ldr	ip, [ip]
	next

#Z ?branch   x --              branch if TOS zero
    codeh qbranch,7,"?branch",branch
	cmp	r1, #0
	addne	ip, ip, #4	/* skip inline value */
	ldreq	ip, [ip]
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#Z (do)    n1|u1 n2|u2 --  R: -- sys1 sys2
#Z                          run-time code for DO
# '83 and ANSI standard loops terminate when the
# boundary of limit-1 and limit is crossed, in
# either direction.  This can be conveniently
# implemented by making the limit 8000h, so that
# arithmetic overflow logic can detect crossing.
# I learned this trick from Laxen & Perry F83.
# fudge factor = 8000h-limit, to be added to
# the start value.

#_ 32-bit version
    codeh xdo,4,"(do)",qbranch
	ldr	r0, [sp], #4	/* read "limit" and update sp */
	rsb	r4, r0, #0x80000000
	str	r4, [fp, #-4]!	/* push fudge-factor to RS */
	add	r4, r4, r1
	str	r4, [fp, #-4]!	/* push index to RS */
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#Z (loop)   R: sys1 sys2 --  | sys1 sys2
#Z                        run-time code for LOOP
# Add 1 to the loop index.  If loop terminates,
# clean up the return stack and skip the branch.
# Else take the inline branch.  Note that LOOP
# terminates when index=8000h.
    codeh xloop,6,"(loop)",xdo
	ldr	r0, [fp]	/* get item on top of RS (index) */
	adds	r0, r0, #1
	bvs	loopterm	/* overflow test */
	str	r0, [fp]	/* update index on top of RS */
	ldr	ip, [ip]	/* take the inline branch */
	next
loopterm:			/* end of the loop */
	add	fp, fp, #8	/* discard the loop info */
	add	ip, ip, #4	/* skip the inline branch */
	next

#Z (+loop)   n --   R: sys1 sys2 --  | sys1 sys2
#Z                        run-time code for +LOOP
# Add n to the loop index.  If loop terminates,
# clean up the return stack and skip the branch.
# Else take the inline branch.
    codeh xplusloop,7,"(+loop)",xloop
	ldr	r0, [fp]	/* get item on top of RS (index) */
	adds	r0, r0, r1
	ldr	r1, [sp], #4	/* pop new TOS */
	bvs	loopterm
	str	r0, [fp]	/* update index on top of RS */
	ldr	ip, [ip]	/* take the inline branch */
	next

#C I        -- n   R: sys1 sys2 -- sys1 sys2
#C                  get the innermost loop index
    codeh ii,1,"i",xplusloop
    	str	r1, [sp, #-4]!	/* push TOS */
    	ldr	r0, [fp]	/* get item on top of RS (index) */
    	ldr	r1, [fp, #4]	/* get fudge-factor */
	sub	r1, r0, r1	/* true index in TOS */
	next

#C J        -- n   R: 4*sys -- 4*sys
#C                  get the second loop index
    codeh jj,1,"j",ii
    	str	r1, [sp, #-4]!	/* push TOS */
    	ldr	r0, [fp, #8]	/* get 2nd index from RS */
    	ldr	r1, [fp, #12]	/* get 2nd fudge-factor */
	sub	r1, r0, r1	/* true index in TOS */
	next

#C UNLOOP   --   R: sys1 sys2 --  drop loop parms
    codeh unloop,6,"unloop",jj
	add	fp, fp, #8
	next

# MULTIPLY AND DIVIDE ===========================

#C UM*     u1 u2 -- ud   unsigned 16x16->32 mult.
# Here is eForth hi-level version :
# UM* ( u u -- ud ) ( 6.1.2360 )( 0xD4 )
#  D# 0 SWAP  [ #BITS ] LITERAL
#  BEGIN DUP
#  WHILE >R  DUP UM+ >R >R  DUP UM+ R> + R>
#    IF >R OVER UM+ R> + THEN  R> D# 1 -
#  REPEAT DROP >R  NIP R> ;

# let's define UM+ first ( u u -- u cy )
    codeh umplus,3,"um+",unloop
	ldr	r0, [sp]	/* read value */
	adds	r0, r0, r1
	movcs	r1, #1
	movcc	r1, #0
	str	r0, [sp]
	next

    head umstar,3,"um*",docolon,umplus
	.word lit,0,swap,lit,32
umloop:	.word dup,qbranch,umend
	.word tor,dup,umplus,tor,tor
	.word dup,umplus,rfrom,plus,rfrom
	.word qbranch,umnext
	.word tor,over,umplus,rfrom,plus
umnext:	.word rfrom,oneminus
	.word branch,umloop
umend:	.word drop,tor,nip,rfrom,exit

#C UM/MOD   ud u1 -- u2 u3   unsigned 32/16->16
# eForth hi-level version... too lazy to rewrite it in assembly :)
#_ 32-bit version
    head umslashmod,6,"um/mod",docolon,umstar
    	.word twodup,uless,qbranch,ummend
	.word negate,lit,32
ummbeg: .word dup,qbranch,ummrep
	.word tor,tor,dup,umplus
	.word tor,tor,dup,umplus
	.word rfrom,plus
	.word dup,rfrom,rfetch,swap,tor,umplus,rfrom,or
	.word qbranch,ummels
	.word tor,drop,oneplus,rfrom
	.word branch,ummthn
ummels:	.word drop
ummthn:	.word rfrom,rfrom,oneminus
	.word branch,ummbeg
ummrep:	.word twodrop,swap,exit
ummend: .word drop,twodrop,lit,-1,dup,exit

# BLOCK AND STRING OPERATIONS ===================

#C FILL   c-addr u char --  fill memory with char
# eForth hi-level version...
# needs BOUNDS ( a n -- a+n a ) ( 0xAC ) OVER + SWAP ;
# uses chstore, so could be better
    head bounds,6,"bounds",docolon,umslashmod
	.word over,plus,swap,exit

    head fill,4,"fill",docolon,bounds
	.word tor,chars,bounds
filbeg:	.word twodup,xor
	.word qbranch,filrep
	.word rfetch,over,chstore,charplus
	.word branch,filbeg
filrep:	.word rfrom,drop,twodrop

#Z WMOVE   a-addr1 a-addr2 u --  move word steps from bottom
# hi-level version
    head wmove,5,"wmove",docolon,fill
	.word over,plus,tor	/* a-addr1 a-addr2   R: a-addr2+u */
wmbeg:	.word dup,rfetch,xor
	.word qbranch,wmrep
	.word tor,dup,fetch,rfetch,store,cellplus,rfrom,cellplus
	.word branch,wmbeg
wmrep:	.word rfrom,drop,twodrop,exit

#Z HMOVE   h-addr1 h-addr2 u --  move halfword steps from bottom
# hi-level version
    head hmove,5,"hmove",docolon,wmove
	.word over,plus,tor	/* h-addr1 h-addr2   R: h-addr2+u */
hmbeg:	.word dup,rfetch,xor
	.word qbranch,hmrep
	.word tor,dup,hfetch,rfetch,hstore,halfplus,rfrom,halfplus
	.word branch,hmbeg
hmrep:	.word rfrom,drop,twodrop,exit

#X CMOVE   c-addr1 c-addr2 u --  move char steps from bottom
# hi-level version
    head cmove,5,"cmove",docolon,hmove
	.word over,plus,tor	/* c-addr1 c-addr2   R: c-addr2+u */
cmbeg:	.word dup,rfetch,xor
	.word qbranch,cmrep
	.word tor,dup,cfetch,rfetch,chstore,charplus,rfrom,charplus
	.word branch,cmbeg
cmrep:	.word rfrom,drop,twodrop,exit

#X CMOVE>  c-addr1 c-addr2 u --  move from top
# hi-level version
    head cmoveup,6,"cmove>",docolon,cmove
    	.word tor
cmubeg:	.word rfrom,dup
	.word qbranch,cmurep
	.word charminus,rfrom,over,rfetch,plus,cfetch,over,rfetch,plus,chstore
	.word branch,cmubeg
cmurep:	.word drop,twodrop,exit

#Z SKIP   c-addr u c -- c-addr' u'
#Z                          skip matching chars
# my own hi-level version
    head skip,4,"skip",docolon,cmoveup
	.word swap,tor
skibeg: .word rfetch,qbranch,skiend    	/* c-addr c  R: u */
	.word over,cfetch,over,equal 	/* c-addr c f  R: u */
	.word qbranch,skiend
	.word swap,charplus,swap        /* c-addr+1 c  R: u */
	.word rfrom,oneminus,tor	/* c-addr+1 c  R: u-1 */
	.word branch,skibeg
skiend: .word drop,rfrom,exit

#Z SCAN    c-addr u c -- c-addr' u'
#Z                      find matching char
# my own hi-level version
# difference with SKIP lies in the loop test
    head scan,4,"scan",docolon,skip
	.word swap,tor
scabeg: .word rfetch,qbranch,scaend    	/* c-addr c  R: u */
	.word over,cfetch,over,notequal /* c-addr c f  R: u */
	.word qbranch,scaend
	.word swap,charplus,swap        /* c-addr+1 c  R: u */
	.word rfrom,oneminus,tor	/* c-addr+1 c  R: u-1 */
	.word branch,scabeg
scaend: .word drop,rfrom,exit

#Z S=    c-addr1 c-addr2 u -- n   string compare
#Z             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
# my own hi-level version
    head sequal,2,"s=",docolon,scan
	.word tor
seqbeg: .word rfetch,qbranch,seqmat    	/* c-addr1 c-addr2    R: u */
	.word over,cfetch,over,cfetch,minus,dup	/* c-addr1 c-addr2 c1-c2 c1-c2   R: u */
	.word qbranch,seqaga		/* c-addr1 c-addr2 c1-c2   R: u */
	.word rfrom,drop,tor,twodrop,rfrom,exit	/* mismatch */
seqaga:	.word drop,charplus,swap,charplus,swap
	.word rfrom,oneminus,tor	/* c-addr1+1 c-addr2+2   R: u-1 */
	.word branch,seqbeg
seqmat: .word twodrop,rfrom,exit	/* u=0 */

.include "../source/PFD.asm"
.include "../source/PFH.asm"

# last word from PFH.asm is COLD

# Example of resource inclusion
# Notice the last parameter, which refer to the previous
# first parameter of "head" macro (think about a linked list)
# Notice also the definition of "lastword",
# that must be defined as link_<last word label>

.section .rom,"ax",%progbits

    head gfx_ball,8,"gfx_ball",dovar,cold
.incbin "../assets/ball.raw"
.align
    head pal_ball1,9,"pal_ball1",dovar,gfx_ball
.incbin "../assets/pal1.pal"
.align
    head pal_ball2,9,"pal_ball2",dovar,pal_ball1
.incbin "../assets/pal2.pal"
.align
    head pal_ball3,9,"pal_ball3",dovar,pal_ball2
.incbin "../assets/pal3.pal"
.align

    head apt_tiles,9,"apt-tiles",dovar,pal_ball3
.incbin "apartment-map.img.bin"
.align
    head apt_tiles_len,13,"apt-tiles-len",docon,apt_tiles
  .word 256

    head apt_pal,7,"apt-pal",dovar,apt_tiles_len
.incbin "apartment-map.pal.bin"
.align
    head apt_pal_len,11,"apt-pal-len",docon,apt_pal
  .word 512

    head apt_map,7,"apt-map",dovar,apt_pal_len
.incbin "apartment-map.map.bin"
.align
    head apt_map_len,11,"apt-map-len",docon,apt_map
  .word 4096

    head beany_tiles,11,"beany-tiles",docon,apt_map_len
  .word beany_sheetTiles
    head beany_pal,9,"beany-pal",docon,beany_tiles
  .word beany_sheetPal

.section .ewram,"ax",%progbits

    head apt_toi,7,"apt-toi",dovar,beany_tiles
.incbin "apt-toi.bin"
.align

.include "ass.asm"

enddict:

# Reserve some room for Forth
.rept 16
.space 1024
.endr
