#C ALIGN    --                         align HERE
    codeh ALIGN,5,"ALIGN",SEQUAL
	ldr	r4, [r2, #16]	/* DP will be at UP+16 */
	ands	r5, r4, #0x3
	andne	r4, r4, #0xfffffffc
	addne	r4, r4, #4
	str	r4, [r2, #16]
	next

#C ALIGNED  addr -- a-addr       align given addr
    codeh ALIGNED,7,"ALIGNED",ALIGN
	ands	r5, r1, #0x3
	andne	r1, r1, #0xfffffffc	
	addne	r1, r1, #4
	next

#Z CELL     -- n                 size of one cell
    head CELL,4,"CELL",docon,ALIGNED
        .word 2

#C CELL+    a-addr1 -- a-addr2      add cell size
    codeh CELLPLUS,5,"CELL+",CELL
	add	r1, r1, #4
	next

#C CELLS    n1 -- n2            cells->adrs units
    codeh CELLS,5,"CELLS",CELLPLUS
    	mov	r1, r1, lsl #2
    	next
	
#C CHAR+    c-addr1 -- c-addr2   add char size
    codeh CHARPLUS,5,"CHAR+",CELLS
    	add	r1, r1, #1
    	next
	
# Not defined in CamelForth, but used in some eForth words
#C CHAR-    c-addr1 -- c-addr2   sub char size
    codeh CHARMINUS,5,"CHAR-",CHARPLUS
        sub	r1, r1, #1
        next

#C CHARS    n1 -- n2            chars->adrs units
    codeh CHARS,5,"CHARS",CHARMINUS
	next

#C >BODY    xt -- a-addr      adrs of param field
#_   3 + ;                     Z80 (3 byte CALL)
# Here, 4 bytes
    head TOBODY,5,">BODY",docolon,CHARS
	.word LIT,4,PLUS,EXIT

#X COMPILE,  xt --         append execution token
    codeh COMMAXT,8,"COMPILE,",TOBODY
    	b	COMMA
    	
#Z !CF    adrs cfa --   set code action of a word
#_  0CD OVER C!         store 'CALL adrs' instr
#_  1+ ! ;              Z80 VERSION
# Needs testing...
# Assembles "bl offset", bl = 0xEB, "offset" on 24bits, in 4-bytes units
# when entering this CF, pc = cfa + 4
# so the offset value is (addr-(cfa+4))>>2
   codeh STORECF,3,"!CF",COMMAXT

	ldr	r0, [sp], #4	/* read adrs and update sp */

	sub	r0, r0, r1	/* r0 = adrs-cfa */	
	mov	r0, r0, asr #2	/* r0 = (adrs-(cfa+4))>>2 */
	sub	r0, r0, #2	/* r0 = adrs-cfa-4 */
	and	r0, r0, #0x00ffffff
	orr	r0, r0, #0xeb000000
			
	str	r0, [r1]

	ldr	r1, [sp], #4	/* pop new TOS */
	next

#Z ,CF    adrs --       append a code field
#   HERE !CF 3 ALLOT ;  Z80 VERSION (3 bytes)
#_ 4 bytes here
    head COMMACF,3,",CF",docolon,STORECF        
        .word HERE,STORECF
        .word LIT,4,ALLOT
        .word EXIT

#Z !COLON   --      change code field to docolon
#_   -3 ALLOT docolon-adrs ,CF ;
# This should be used immediately after CREATE.
# This is made a distinct word, because on an STC
# Forth, colon definitions have no code field.
#_ 4 bytes here
    head STORCOLON,6,"!COLON",docolon,COMMACF
        .word LIT,-4,ALLOT
        .word LIT,docolon,COMMACF,EXIT

#Z ,EXIT    --      append hi-level EXIT action
#   ['] EXIT ,XT ;
# This is made a distinct word, because on an STC
# Forth, it appends a RET instruction, not an xt.
    head CEXIT,5,",EXIT",docolon,STORCOLON
        .word LIT,EXIT,COMMAXT,EXIT
    	
# CONTROL STRUCTURES ============================
# These words allow Forth control structure words
# to be defined portably.

#Z ,BRANCH   xt --    append a branch instruction
# xt is the branch operator to use, e.g. qbranch
# or (loop).  It does NOT append the destination
# address.  On the Z80 this is equivalent to ,XT.
    codeh COMMABRANCH,7,",BRANCH",CEXIT
        b	COMMA

#Z ,DEST   dest --        append a branch address
# This appends the given destination address to
# the branch instruction.  On the Z80 this is ','
# ...other CPUs may use relative addressing.
    codeh COMMADEST,5,",DEST",COMMABRANCH
        b	COMMA

#Z !DEST   dest adrs --    change a branch dest'n
# Changes the destination address found at 'adrs'
# to the given 'dest'.  On the Z80 this is '!'
# ...other CPUs may need relative addressing.
    codeh STOREDEST,5,"!DEST",COMMADEST
        b 	STORE
