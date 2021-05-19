#C ALIGN    --                         align HERE
    codeh align,5,"align",sequal
	ldr	r4, [r2, #16]	/* DP will be at UP+16 */
	ands	r5, r4, #0x3
	andne	r4, r4, #0xfffffffc
	addne	r4, r4, #4
	str	r4, [r2, #16]
	next

#C ALIGNED  addr -- a-addr       align given addr
    codeh aligned,7,"aligned",align
	ands	r5, r1, #0x3
	andne	r1, r1, #0xfffffffc
	addne	r1, r1, #4
	next

#Z CELL     -- n                 size of one cell
    head cell,4,"cell",docon,aligned
        .word 4

#C CELL+    a-addr1 -- a-addr2      add cell size
    codeh cellplus,5,"cell+",cell
	add	r1, r1, #4
	next

#C CELLS    n1 -- n2            cells->adrs units
    codeh cells,5,"cells",cellplus
    	mov	r1, r1, lsl #2
    	next

# half operations make working with certain GBA memory types
# much more efficient
#Z HALF+    h-addr1 -- h-addr2   add half size
    codeh halfplus,5,"half+",cells
    	add	r1, r1, #2
    	next

#Z HALF-    h-addr1 -- h-addr2   sub half size
  codeh halfminus,5,"half-",halfplus
  sub	r1, r1, #2
  next

#Z HALVES   n1 -- n2            chars->adrs units
      codeh halves,6,"halves",halfminus
      mov r1, r1, lsl #1
	next

#C CHAR+    c-addr1 -- c-addr2   add char size
    codeh charplus,5,"char+",halves
    	add	r1, r1, #1
    	next

# Not defined in CamelForth, but used in some eForth words
#C CHAR-    c-addr1 -- c-addr2   sub char size
    codeh charminus,5,"char-",charplus
        sub	r1, r1, #1
        next

#C CHARS    n1 -- n2            chars->adrs units
    codeh chars,5,"chars",charminus
	next

#C >BODY    xt -- a-addr      adrs of param field
#_   3 + ;                     Z80 (3 byte CALL)
# Here, 4 bytes
    head tobody,5,">body",docolon,chars
	.word lit,4,plus,exit

#X COMPILE,  xt --         append execution token
    codeh commaxt,8,"compile,",tobody
    	b	comma

#Z !CF    adrs cfa --   set code action of a word
#_  0CD OVER C!         store 'CALL adrs' instr
#_  1+ ! ;              Z80 VERSION
# Needs testing...
# Assembles "bl offset", bl = 0xEB, "offset" on 24bits, in 4-bytes units
# when entering this CF, pc = cfa + 4
# so the offset value is (addr-(cfa+4))>>2
   codeh storecf,3,"!cf",commaxt

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
    head commacf,3,",cf",docolon,storecf
        .word here,storecf
        .word lit,4,allot
        .word exit

#Z !COLON   --      change code field to docolon
#_   -3 ALLOT docolon-adrs ,CF ;
# This should be used immediately after CREATE.
# This is made a distinct word, because on an STC
# Forth, colon definitions have no code field.
#_ 4 bytes here
    head storcolon,6,"!colon",docolon,commacf
        .word lit,-4,allot
        .word lit,docolon,commacf,exit

#Z ,EXIT    --      append hi-level EXIT action
#   ['] EXIT ,XT ;
# This is made a distinct word, because on an STC
# Forth, it appends a RET instruction, not an xt.
    head cexit,5,",exit",docolon,storcolon
        .word lit,exit,commaxt,exit

# CONTROL STRUCTURES ============================
# These words allow Forth control structure words
# to be defined portably.

#Z ,BRANCH   xt --    append a branch instruction
# xt is the branch operator to use, e.g. qbranch
# or (loop).  It does NOT append the destination
# address.  On the Z80 this is equivalent to ,XT.
    codeh commabranch,7,",branch",cexit
        b	comma

#Z ,DEST   dest --        append a branch address
# This appends the given destination address to
# the branch instruction.  On the Z80 this is ','
# ...other CPUs may use relative addressing.
    codeh commadest,5,",dest",commabranch
        b	comma

#Z !DEST   dest adrs --    change a branch dest'n
# Changes the destination address found at 'adrs'
# to the given 'dest'.  On the Z80 this is '!'
# ...other CPUs may need relative addressing.
    codeh storedest,5,"!dest",commadest
        b 	store
