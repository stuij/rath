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
	
	bl	COLD

# INTERPRETER LOGIC =============================
# See also "defining words" at end of this file

#C EXIT     --      exit a colon definition
    codeh EXIT,4,"EXIT",0
	ldr	ip, [fp], #4	/* pop old IP from ret stk */
	next

#Z lit      -- x    fetch inline literal to stack
# This is the primtive compiled by LITERAL.
    codeh LIT,3,"LIT",EXIT
	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [ip], #4	/* fetch cell at IP to TOS, advancing IP */
	next	

#C EXECUTE   i*x xt -- j*x   execute Forth word
#C                           at 'xt'
    codeh EXECUTE,7,"EXECUTE",LIT
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
docolon:
enter:
	str	ip, [fp, #-4]!	/* push old IP on ret stack */
	mov	ip, lr		/* param field adrs -> IP */	
	next	

#C VARIABLE   --      define a Forth variable
#   CREATE 1 CELLS ALLOT ;
# Action of RAM variable is identical to CREATE,
# so we don't need a DOES> clause to change it.
    head VARIABLE,8,"VARIABLE",docolon,EXECUTE
	.word CREATE
	.word LIT,1,CELLS,ALLOT	
	/*.word HERE,LIT,0,BDOS*/
	.word EXIT

# DOVAR, code action of VARIABLE, entered by CALL
# DOCREATE, code action of newly created words
docreate:
dovar:  /* -- a-addr */
	str	r1, [sp, #-4]!	/* push old TOS */
	mov	r1, lr		/* pfa = variables adrs -> TOS */
	next

#C CONSTANT   n --      define a Forth constant
#   CREATE , DOES> (machine code fragment)
    head CONSTANT,8,"CONSTANT",docolon,VARIABLE
	.word CREATE,COMMA,XDOES
# DOCON, code action of CONSTANT,
# entered by CALL DOCON
docon:  /* -- x */
	str	r1, [sp, #-4]!	/* push old TOS */
	ldr	r1, [lr]	/* fetch contents of parameter field -> TOS */
	next

#Z USER     n --        define user variable 'n'
#   CREATE , DOES> (machine code fragment)
    head USER,4,"USER",docolon,CONSTANT
	.word CREATE,COMMA,XDOES
# DOUSER, code action of USER,
# entered by CALL DOUSER
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
    codeh BDOS,4,"BDOS",USER    	
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
   head EMIT,4,"EMIT",docolon,BDOS
	.word LIT,0x06,BDOS,DROP,EXIT

#Z SAVEKEY  -- addr  temporary storage for KEY?
   head SAVEKEY,7,"SAVEKEY",dovar,EMIT
	.word 0

#X KEY?     -- f    return true if char waiting
#_   0FF 6 BDOS DUP SAVEKEY C!    rtns 0 or key 
# must use BDOS function 6 to work with KEY
    head QUERYKEY,4,"KEY?",docolon,SAVEKEY
        .word LIT,0xFF,LIT,0x06,BDOS
        .word DUP,SAVEKEY,CSTORE,EXIT

#C KEY      -- c    get character from keyboard
#   BEGIN SAVEKEY C@ 0= WHILE KEY? DROP REPEAT
#   SAVEKEY C@  0 SAVEKEY C! ;
# must use CP/M direct console I/O to avoid echo
# (BDOS function 6, contained within KEY?)
    head KEY,3,"KEY",docolon,QUERYKEY
KEY1:   .word SAVEKEY,CFETCH,ZEROEQUAL,QBRANCH,KEY2
        .word QUERYKEY,DROP,BRANCH,KEY1
KEY2:   .word SAVEKEY,CFETCH,LIT,0,SAVEKEY,CSTORE
        .word EXIT

#X BYE     i*x --    return to CP/M
    codeh BYE,3,"BYE",KEY
	b 	reset

# STACK OPERATIONS ==============================

#C DUP      x -- x x      duplicate top of stack
    codeh DUP,3,"DUP",BYE
	str	r1, [sp, #-4]!	/* push TOS */	
        next

#C ?DUP     x -- 0 | x x    DUP if nonzero
    codeh QDUP,4,"?DUP",DUP
        cmp	r1, #0
	strne	r1, [sp, #-4]!	/* push TOS */	
        next

#C DROP     x --          drop top of stack
    codeh DROP,4,"DROP",QDUP
	ldr	r1, [sp], #4	/* pop new TOS */	
	next

#C SWAP     x1 x2 -- x2 x1    swap top two items
    codeh SWAP,4,"SWAP",DROP
	ldr	r0, [sp]
	str	r1, [sp]
	mov	r1, r0
	next

#C OVER    x1 x2 -- x1 x2 x1   per stack diagram
    codeh OVER,4,"OVER",SWAP
	str	r1, [sp, #-4]!	/* push TOS */	
	ldr	r1, [sp, #4]	/* get new TOS */
	next

#C ROT    x1 x2 x3 -- x2 x3 x1  per stack diagram
    codeh ROT,3,"ROT",OVER
	ldr	r4, [sp]	/* get x2 */
	str	r1, [sp]	/* x1 x3 x3 */
	ldr	r1, [sp, #4]	/* x1 x3 x1 */
	str	r4, [sp, #4]	/* x2 x3 x1 */
	next	

#X NIP    x1 x2 -- x2           per stack diagram
    head NIP,3,"NIP",docolon,ROT
        .word SWAP,DROP,EXIT

#X TUCK   x1 x2 -- x2 x1 x2     per stack diagram
    head TUCK,4,"TUCK",docolon,NIP
        .word SWAP,OVER,EXIT

#C >R    x --   R: -- x   push to return stack
    codeh TOR,2,">R",TUCK
	str	r1, [fp, #-4]!	/* push TOS on return stack */
	ldr	r1, [sp], #4	/* pop new TOS */
	next
	
#C R>    -- x    R: x --   pop from return stack
    codeh RFROM,2,"R>",TOR
	str	r1, [sp, #-4]!	/* push TOS */			
	ldr	r1, [fp], #4	/* pop new TOS from return stack */	
	next
	
#C R@    -- x     R: x -- x   fetch from rtn stk
    codeh RFETCH,2,"R@",RFROM
    	str	r1, [sp, #-4]!	/* push TOS */
	ldr	r1, [fp]	/* get new TOS from return stack */	    				
	next

#Z SP@  -- a-addr       get data stack pointer
    codeh SPFETCH,3,"SP@",RFETCH
	str	r1, [sp, #-4]!	/* push TOS */
	mov	r1, sp
	next

#Z SP!  a-addr --       set data stack pointer
    codeh SPSTORE,3,"SP!",SPFETCH
	mov	sp, r1
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#Z RP@  -- a-addr       get return stack pointer
    codeh RPFETCH,3,"RP@",SPSTORE
	str	r1, [sp, #-4]!	/* push TOS */
	mov	r1, fp
	next	
	
#Z RP!  a-addr --       set return stack pointer
    codeh RPSTORE,3,"RP!",RPFETCH
    	mov	fp, r1
	ldr	r1, [sp], #4	/* pop new TOS */
	next
	
# MEMORY AND I/O OPERATIONS =====================

#C !        x a-addr --   store cell in memory
    codeh STORE,1,"!",RPSTORE
	ldr	r4, [sp], #4	/* read value and update sp */
	str	r4, [r1]
	ldr	r1, [sp], #4	/* pop new TOS */
	next

#C C!      char c-addr --    store char in memory
    codeh CSTORE,2,"C!",STORE
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
    codeh FETCH,1,"@",CSTORE
	ldr	r1, [r1]
	next

#C C@     c-addr -- char   fetch char from memory
    codeh CFETCH,2,"C@",FETCH
	ands	r5, r1, #1
	subne	r1, r1, #1	/* get halfword-aligned address */
	ldrh	r6, [r1]	/* fetch the halfword */
	movne	r6, r6, lsr #8 	/* mask the corresponding byte */
	andeq	r6, r6, #0x00ff
	mov	r1, r6		/* update TOS */
	next	

# ARITHMETIC AND LOGICAL OPERATIONS =============

#C +       n1/u1 n2/u2 -- n3/u3     add n1+n2
    codeh PLUS,1,"+",CFETCH
	ldr	r0, [sp], #4	/* read value and update sp */
	add	r1, r0, r1	/* result in TOS */
	next

#X M+       d n -- d         add single to double
    codeh MPLUS,2,"M+",PLUS
	ldr	r4, [sp], #4	/* read HI and update sp */
	ldr	r5, [sp]	/* read LO  */
	adds	r5, r5, r1	
	addcs	r4, r4, #1
	mov	r1, r4
	str	r5, [sp]	/* HI+c in TOS, LO+n to stack */
	next

#C -      n1/u1 n2/u2 -- n3/u3    subtract n1-n2
    codeh MINUS,1,"-",MPLUS
	ldr	r0, [sp], #4	/* read value and update sp */
	sub	r1, r0, r1	/* result in TOS */
	next

#C AND    x1 x2 -- x3            logical AND
    codeh AND,3,"AND",MINUS
	ldr	r0, [sp], #4	/* read value and update sp */
	and	r1, r0, r1	/* result in TOS */
	next

#C OR     x1 x2 -- x3           logical OR
    codeh OR,2,"OR",AND
	ldr	r0, [sp], #4	/* read value and update sp */
	orr	r1, r0, r1	/* result in TOS */
	next

#C XOR    x1 x2 -- x3            logical XOR
    codeh XOR,3,"XOR",OR
	ldr	r0, [sp], #4	/* read value and update sp */
	eor	r1, r0, r1	/* result in TOS */
	next

#C INVERT   x1 -- x2            bitwise inversion
    codeh INVERT,6,"INVERT",XOR
	mvn	r1, r1
	next

#C NEGATE   x1 -- x2            two's complement
    codeh NEGATE,6,"NEGATE",INVERT
	rsb	r1, r1, #0
	next
	
#C 1+      n1/u1 -- n2/u2       add 1 to TOS
    codeh ONEPLUS,2,"1+",NEGATE
    	add	r1, r1, #1
    	next

#C 1-      n1/u1 -- n2/u2     subtract 1 from TOS
    codeh ONEMINUS,2,"1-",ONEPLUS
    	sub	r1, r1, #1	
    	next

#C 2*      x1 -- x2         arithmetic left shift
    codeh TWOSTAR,2,"2*",ONEMINUS
    	mov	r1, r1, asl #1	
	next
		
#C 2/      x1 -- x2        arithmetic right shift
    codeh TWOSLASH,2,"2/",TWOSTAR
    	mov	r1, r1, asr #1
    	next

#C LSHIFT  x1 u -- x2    logical L shift u places
    codeh LSHIFT,6,"LSHIFT",TWOSLASH
	ldr	r0, [sp], #4	/* read value and update sp */
	mov	r1, r0, lsl r1
	next
	
#C RSHIFT  x1 u -- x2    logical R shift u places
    codeh RSHIFT,6,"RSHIFT",LSHIFT
    	ldr	r0, [sp], #4	/* read value and update sp */
	mov	r1, r0, lsr r1
	next

#C +!     n/u a-addr --       add cell to memory
    codeh PLUSSTORE,2,"+!",RSHIFT
	ldr	r0, [sp], #4	/* read value and update sp */
	ldr	r4, [r1]
	add	r4, r4, r0
	str	r4, [r1]
	ldr	r1, [sp], #4	/* pop new TOS */
	next

# COMPARISON OPERATIONS =========================

#C 0=     n/u -- flag    return true if TOS=0
    codeh ZEROEQUAL,2,"0=",PLUSSTORE
	cmp	r1, #0
	mvneq	r1, #0
	movne	r1, #0
	next

#C 0<     n -- flag      true if TOS negative
    codeh ZEROLESS,2,"0<",ZEROEQUAL
	cmp r1, #0
	mvnlt	r1, #0
	movge	r1, #0
	next
    
#C =      x1 x2 -- flag         test x1=x2
    codeh EQUAL,1,"=",ZEROLESS
    	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvneq	r1, #0
	movne	r1, #0
	next
    
#X <>     x1 x2 -- flag    test not eq (not ANSI)
    codeh NOTEQUAL,2,"<>",EQUAL
    	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r1, r0
	mvnne	r1, #0
	moveq	r1, #0
	next

#C <      n1 n2 -- flag        test n1<n2, signed
    codeh LESS,1,"<",NOTEQUAL
	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvnlt	r1, #0
	movge	r1, #0
	next

#C >     n1 n2 -- flag         test n1>n2, signed
    codeh GREATER,1,">",LESS
	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvngt	r1, #0
	movle	r1, #0
	next

#C U<    u1 u2 -- flag       test u1<n2, unsigned
    codeh ULESS,2,"U<",GREATER
	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvncc	r1, #0
	movcs	r1, #0
	next

#X U>    u1 u2 -- flag     u1>u2 unsgd (not ANSI)
    codeh UGREATER,2,"U>",ULESS
	ldr	r0, [sp], #4	/* read value and update sp */
	cmp	r0, r1
	mvnhi	r1, #0
	movls	r1, #0
	next
    
# LOOP AND BRANCH OPERATIONS ====================

#Z branch   --                  branch always
    codeh BRANCH,6,"BRANCH",UGREATER
	ldr	ip, [ip]
	next

#Z ?branch   x --              branch if TOS zero
    codeh QBRANCH,7,"?BRANCH",BRANCH
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
    codeh XDO,4,"(DO)",QBRANCH
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
    codeh XLOOP,6,"(LOOP)",XDO
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
    codeh XPLUSLOOP,7,"(+LOOP)",XLOOP
	ldr	r0, [fp]	/* get item on top of RS (index) */
	adds	r0, r0, r1	
	ldr	r1, [sp], #4	/* pop new TOS */
	bvs	loopterm
	str	r0, [fp]	/* update index on top of RS */	
	ldr	ip, [ip]	/* take the inline branch */
	next

#C I        -- n   R: sys1 sys2 -- sys1 sys2
#C                  get the innermost loop index
    codeh II,1,"I",XPLUSLOOP
    	str	r1, [sp, #-4]!	/* push TOS */
    	ldr	r0, [fp]	/* get item on top of RS (index) */
    	ldr	r1, [fp, #4]	/* get fudge-factor */
	sub	r1, r0, r1	/* true index in TOS */
	next		
	
#C J        -- n   R: 4*sys -- 4*sys
#C                  get the second loop index
    codeh JJ,1,"J",II
    	str	r1, [sp, #-4]!	/* push TOS */
    	ldr	r0, [fp, #8]	/* get 2nd index from RS */
    	ldr	r1, [fp, #12]	/* get 2nd fudge-factor */
	sub	r1, r0, r1	/* true index in TOS */
	next		

#C UNLOOP   --   R: sys1 sys2 --  drop loop parms
    codeh UNLOOP,6,"UNLOOP",JJ
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
    codeh UMPLUS,3,"UM+",UNLOOP
	ldr	r0, [sp]	/* read value */
	adds	r0, r0, r1
	movcs	r1, #1
	movcc	r1, #0
	str	r0, [sp]
	next

    head UMSTAR,3,"UM*",docolon,UMPLUS
	.word LIT,0,SWAP,LIT,32
umloop:	.word DUP,QBRANCH,umend
	.word TOR,DUP,UMPLUS,TOR,TOR
	.word DUP,UMPLUS,RFROM,PLUS,RFROM
	.word QBRANCH,umnext
	.word TOR,OVER,UMPLUS,RFROM,PLUS
umnext:	.word RFROM,ONEMINUS
	.word BRANCH,umloop
umend:	.word DROP,TOR,NIP,RFROM,EXIT		

#C UM/MOD   ud u1 -- u2 u3   unsigned 32/16->16
# eForth hi-level version... too lazy to rewrite it in assembly :)
#_ 32-bit version
    head UMSLASHMOD,6,"UM/MOD",docolon,UMSTAR
    	.word TWODUP,ULESS,QBRANCH,ummend
	.word NEGATE,LIT,32
ummbeg: .word DUP,QBRANCH,ummrep
	.word TOR,TOR,DUP,UMPLUS
	.word TOR,TOR,DUP,UMPLUS
	.word RFROM,PLUS
	.word DUP,RFROM,RFETCH,SWAP,TOR,UMPLUS,RFROM,OR
	.word QBRANCH,ummels
	.word TOR,DROP,ONEPLUS,RFROM
	.word BRANCH,ummthn
ummels:	.word DROP
ummthn:	.word RFROM,RFROM,ONEMINUS
	.word BRANCH,ummbeg		
ummrep:	.word TWODROP,SWAP,EXIT
ummend: .word DROP,TWODROP,LIT,-1,DUP,EXIT

# BLOCK AND STRING OPERATIONS ===================

#C FILL   c-addr u char --  fill memory with char
# eForth hi-level version... 
# needs BOUNDS ( a n -- a+n a ) ( 0xAC ) OVER + SWAP ;
    head BOUNDS,6,"BOUNDS",docolon,UMSLASHMOD
	.word OVER,PLUS,SWAP,EXIT
    
    head FILL,4,"FILL",docolon,BOUNDS
	.word TOR,CHARS,BOUNDS
filbeg:	.word TWODUP,XOR
	.word QBRANCH,filrep
	.word RFETCH,OVER,CSTORE,CHARPLUS
	.word BRANCH,filbeg
filrep:	.word RFROM,DROP,TWODROP

#X CMOVE   c-addr1 c-addr2 u --  move from bottom
# hi-level version
    head CMOVE,5,"CMOVE",docolon,FILL
	.word OVER,PLUS,TOR	/* c-addr1 c-addr2   R: c-addr2+u */
cmbeg:	.word DUP,RFETCH,XOR
	.word QBRANCH,cmrep
	.word TOR,DUP,CFETCH,RFETCH,CSTORE,CHARPLUS,RFROM,CHARPLUS
	.word BRANCH,cmbeg
cmrep:	.word RFROM,DROP,TWODROP,EXIT	

#X CMOVE>  c-addr1 c-addr2 u --  move from top
# hi-level version
    head CMOVEUP,6,"CMOVE>",docolon,CMOVE
    	.word TOR
cmubeg:	.word RFROM,DUP
	.word QBRANCH,cmurep
	.word CHARMINUS,RFROM,OVER,RFETCH,PLUS,CFETCH,OVER,RFETCH,PLUS,CSTORE    	
	.word BRANCH,cmubeg
cmurep:	.word DROP,TWODROP,EXIT	

#Z SKIP   c-addr u c -- c-addr' u'
#Z                          skip matching chars
# my own hi-level version
    head SKIP,4,"SKIP",docolon,CMOVEUP
	.word SWAP,TOR
skibeg: .word RFETCH,QBRANCH,skiend    	/* c-addr c  R: u */
	.word OVER,CFETCH,OVER,EQUAL 	/* c-addr c f  R: u */
	.word QBRANCH,skiend		 
	.word SWAP,CHARPLUS,SWAP        /* c-addr+1 c  R: u */
	.word RFROM,ONEMINUS,TOR	/* c-addr+1 c  R: u-1 */
	.word BRANCH,skibeg	
skiend: .word DROP,RFROM,EXIT	

#Z SCAN    c-addr u c -- c-addr' u'
#Z                      find matching char
# my own hi-level version
# difference with SKIP lies in the loop test
    head SCAN,4,"SCAN",docolon,SKIP
	.word SWAP,TOR
scabeg: .word RFETCH,QBRANCH,scaend    	/* c-addr c  R: u */
	.word OVER,CFETCH,OVER,NOTEQUAL /* c-addr c f  R: u */
	.word QBRANCH,scaend		 
	.word SWAP,CHARPLUS,SWAP        /* c-addr+1 c  R: u */
	.word RFROM,ONEMINUS,TOR	/* c-addr+1 c  R: u-1 */
	.word BRANCH,scabeg	
scaend: .word DROP,RFROM,EXIT	

#Z S=    c-addr1 c-addr2 u -- n   string compare
#Z             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
# my own hi-level version
    head SEQUAL,2,"S=",docolon,SCAN
	.word TOR
seqbeg: .word RFETCH,QBRANCH,seqmat    	/* c-addr1 c-addr2    R: u */
	.word OVER,CFETCH,OVER,CFETCH,MINUS,DUP	/* c-addr1 c-addr2 c1-c2 c1-c2   R: u */
	.word QBRANCH,seqaga		/* c-addr1 c-addr2 c1-c2   R: u */
	.word RFROM,DROP,TOR,TWODROP,RFROM,EXIT	/* mismatch */
seqaga:	.word DROP,CHARPLUS,SWAP,CHARPLUS,SWAP
	.word RFROM,ONEMINUS,TOR	/* c-addr1+1 c-addr2+2   R: u-1 */
	.word BRANCH,seqbeg
seqmat: .word TWODROP,RFROM,EXIT	/* u=0 */

.include "../source/PFD.asm"
.include "../source/PFH.asm"
# last word from PFH.asm is COLD

# Example of resource inclusion
# Notice the last parameter, which refer to the previous 
# first parameter of "head" macro (think about a linked list)
# Notice also the definition of "lastword",
# that must be defined as link_<last word label>

    head GFX_BALL,8,"GFX_BALL",dovar,COLD
.incbin "../res/ball.raw"
.align
    head PAL_BALL1,9,"PAL_BALL1",dovar,GFX_BALL
.incbin "../res/pal1.pal"
.align
    head PAL_BALL2,9,"PAL_BALL2",dovar,PAL_BALL1
.incbin "../res/pal2.pal"
.align
    head PAL_BALL3,9,"PAL_BALL3",dovar,PAL_BALL2
.incbin "../res/pal3.pal"
.align

.set lastword, link_PAL_BALL3		/* last word */
enddict:

# Reserve some room for Forth
.rept 16
.space 1024
.endr
