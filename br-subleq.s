
; BeamRacer SUBLEQ Virtual Machine
;
; Maciej 'YTM/Elysium' Witkowiak, 2021
;

NTSC_LAST_SAFE_LINE = 259
NTSC_LINE_COUNT = 263
PAL_LAST_SAFE_LINE = 307

        .include "vlib/vasyl.s"

	sei
	lda	#<irq_handler
	sta	$314
	lda	#>irq_handler
	sta	$315
	cli

	jsr knock_knock
	jsr copy_and_activate_dlist

@keyloop:
	jsr $ffe4   ; check if key pressed
	beq @keyloop

	; stop VASYL and restore IRQ vector
	lda	#0
	sta	$d031
	sei
	lda	#$31
	sta	$314
	lda	#$ea
	sta	$315
	cli
 
	rts

irq_handler:
	lda	$d019
	and	#%00010000
	beq	not_vasyl_irq

	sta	$d019		; ack VASYL IRQ
	lda	VREG_PORT0	; get the character to print
	bne	do_output
	cli			; enable interrupts so that the keyboard can work
@keyloop:
	jsr	$ffe4
	beq	@keyloop
	sta	VREG_PORT0
	inc	VREG_DLISTL	; release VASYL from spinlock, as it is ok to run concurrently from now on
	bne	irq_end	; always taken

do_output:
	inc	VREG_DLISTL	; release VASYL from spinlock, as it is ok to run concurrently from now on
	jsr	$FFD2		; CHROUT
irq_end:
	jmp	$ea81		; pull regs from the stack and RTI

not_vasyl_irq:
	jmp	$ea31		; original IRQ routine


	; Once VASYL enters a spinlock, only 6510 can stop it from spinning by
	; incrementing VREG_DLISTL by one. Otherwise it will spin for an arbitrary amount of time
	; (including across multiple frames).
	.macro SPINLOCK
		MOV		VREG_DLISTL, <@spinlock	; potential race, so do not use just before frame's end
		MOV		VREG_DLISTH, >@spinlock
		MOV		$d021, 2
		.if (* - dl_start) & $ff = $ff	; if spinlock would fall on a page's last byte
			VNOP					; we insert a dummy NOP to prevent that.
		.endif
@spinlock:
		MOV		VREG_DLSTROBE, $a7	; spin, baby, spin ; $a7 is VNOP, which we get pointed to
								; once DLISTL is incremented
		SKIP
		WAIT		NTSC_LAST_SAFE_LINE, 0
		BRA		@safe_to_update	; we are still before NTSC's last safe line, and so also before PAL's.
		WAIT		NTSC_LINE_COUNT, 0	; this only completes on PAL, so an NTSC machine will restart (at spinlock+1)
		SKIP
		WAIT		PAL_LAST_SAFE_LINE, 0
		BRA		@safe_to_update	; we're on PAL and still before last safe line.
		END				; wait for the next frame
@safe_to_update:
		MOV		VREG_DLISTL, <dl_restart
		MOV		VREG_DLISTH, >dl_restart
	.endmacro


	.macro VJMP	label
		MOV		VREG_DLIST2L, <label
		MOV		VREG_DLIST2H, >label
		MOV		VREG_DL2STROBE, 0
	.endmacro

	.macro VJNZA label
		MOV		VREG_DLIST2L, <label
		MOV		VREG_DLIST2H, >label
		DECA
		MOV		VREG_DL2STROBE, 0
	.endmacro



	.include "vlib/vlib.s"

	.segment "VASYL"


	;; instruction action:
	;;	[b] <- [b]-[a] = -[a]+[b]; if b<=0 then goto 0
	;; instruction encoding:
	;; (address to jump to when result negative or zero - c) (address to jump to when result positive) (address of a) (address of b (result))
dl_start:
	; enable reading from both ports, we're in bank0, DL enabled
	MOV		VREG_CONTROL, %00011000	; this can be done from C64 setup
	; start of vm program
	MOV		VREG_ADR0, <vm_start	; this can be done from C64 setup
	MOV		VREG_ADR0+1, >vm_start
	; make DL restart at dl_restart so that program continues in the new frame
	MOV		VREG_DLISTL, <dl_restart
	MOV		VREG_DLISTH, >dl_restart
	MOV		$d01a, %00010000    ; enable VASYL interrupts

	WAIT		300, 0  ; this WAIT can only complete on PAL - subsequent instructions won't be executed on an NTSC machine.
	MOV		VREG_ADR1, <(frame_end+1)
	MOV		VREG_ADR1+1, >(frame_end+1)
	MOV		VREG_PORT1, <PAL_LAST_SAFE_LINE	; default frame-end marker suitable for PAL
	END

character_out:
	MOV		$d021, 7
	IRQ
	MOV		VREG_ADR0, $ff
	MOV		VREG_ADR0+1, $ff
	SPINLOCK
	MOV		$d021,6
	VJMP		back_from_comparator

character_in:
	IRQ
	MOV		VREG_ADR0, $ff
	MOV		VREG_ADR0+1, $ff
	MOV		VREG_STEP0, 0	; so that the byte can accessed multiple times
	MOV		VREG_PORT0, 0	; signal to 6510 that we want input
	SPINLOCK
	VJMP		back_from_comparator_a


; comparator checks if both lo- and hi-byte are equal to $ff.
comparator_a:
comparator_a_addr1:
	MOV		VREG_STEP0, 0	; low byte
	MOV		VREG_ADR0, <(iocheck_table+$80)
	MOV		VREG_ADR0+1, >(iocheck_table+$80)
	MOV		VREG_ADR1, <(comparator_a_addr2+1)
	MOV		VREG_ADR1+1, >(comparator_a_addr2+1)
	XFER		VREG_STEP1, (0)
	XFER		VREG_PORT1, (0)
	VNOP				; one waitcycle needed for the write above to land
comparator_a_addr2:
	SETA		0		; this will be modified
	VJNZA		back_from_comparator_a
comparator_a_addr3:
	MOV		VREG_STEP0, 0	; hi byte
	MOV		VREG_ADR0, <(iocheck_table+$80)
	MOV		VREG_ADR0+1, >(iocheck_table+$80)
	MOV		VREG_ADR1, <(comparator_a_addr4+1)
	MOV		VREG_ADR1+1, >(comparator_a_addr4+1)
	XFER		VREG_STEP1, (0)
	XFER		VREG_PORT1, (0)
	VNOP				; one waitcycle needed for the write above to land
comparator_a_addr4:
	SETA		0		; this will be modified
	VJNZA		back_from_comparator_a
	DECB				; I/O indicator
	BRA		character_in

; comparator checks if both lo- and hi-byte are equal to $ff.
comparator:
comparator_addr:
	MOV		VREG_STEP0, 0	; low byte
	MOV		VREG_ADR0, <(iocheck_table+$80)
	MOV		VREG_ADR0+1, >(iocheck_table+$80)
	MOV		VREG_ADR1, <(comparator_addr2+1)
	MOV		VREG_ADR1+1, >(comparator_addr2+1)
	XFER		VREG_STEP1, (0)
	XFER		VREG_PORT1, (0)
	VNOP				; one waitcycle needed for the write above to land
comparator_addr2:
	SETA		0		; this will be modified
	VJNZA		back_from_comparator

comparator_addr3:
	MOV		VREG_STEP0, 0	; hi byte
	MOV		VREG_ADR0, <(iocheck_table+$80)
	MOV		VREG_ADR0+1, >(iocheck_table+$80)
	MOV		VREG_ADR1, <(comparator_addr4+1)
	MOV		VREG_ADR1+1, >(comparator_addr4+1)
	XFER		VREG_STEP1, (0)
	XFER		VREG_PORT1, (0)
	VNOP				; one waitcycle needed for the write above to land
comparator_addr4:
	SETA		0		; this will be modified
	DECA
char_out_done:
	BRA		back_from_comparator
	DECB				; I/O indicator
	VJMP		character_out
comparator_trampoline:
	BRA		comparator


dl_restart:				;; new frame starts here
	MOV		$20, 2		;; indicator start
mainloop:				;; new instruction processing starts here
	MOV		VREG_STEP0, 1
	MOV		VREG_STEP1, 2	;; skip 2 bytes - over next opcode for all self-modifying writes

	MOV		VREG_ADR1, <(pcleq+1)
	MOV		VREG_ADR1+1, >(pcleq+1)
	XFER		VREG_PORT1, (0) ;; lo byte of new PC (if neq), branch after DECA taken
	XFER		VREG_PORT1, (0) ;; hi byte of new PC (if neq), branch after DECA taken

	MOV		VREG_ADR1, <(pcpos+1)
	MOV		VREG_ADR1+1, >(pcpos+1)
	XFER		VREG_PORT1, (0) ;; lo byte of new PC (if positive), branch after DECA not taken, just address of the next instruction
	XFER		VREG_PORT1, (0) ;; hi byte of new PC (if positive), branch after DECA not taken, just address of the next instruction

	;; copy address of [a] into places where [a] will be read

.if 1	;; three cycles shorter, but harder on the eyes
	MOV		VREG_ADR1, <(addr1+1)
	MOV		VREG_ADR1+1, >(addr1+1)
	XFER		VREG_PORT1, (0)	;; lo byte of [a]
	XFER		VREG_PORT1, (0) ;; hi byte of [a]

	MOV		VREG_STEP0, -1
	MOV		VREG_STEP1, (comparator_a_addr3 - comparator_a_addr1)
	XFER		VREG_ADR1, (0)

	MOV		VREG_ADR1, <(comparator_a_addr1 + 1)
	MOV		VREG_ADR1+1, >(comparator_a_addr1 + 1)
	XFER		VREG_PORT1, (0)
	XFER		VREG_PORT1, (0)

	MOV		VREG_STEP0, 3
	XFER		VREG_STEP1, (0)
.else
	MOV		VREG_STEP0, 0
	MOV		VREG_ADR1, <(addr1+1)
	MOV		VREG_ADR1+1, >(addr1+1)
	XFER		VREG_PORT1, (0)	;; lo byte of [a]
	MOV		VREG_ADR1, <(comparator_a_addr1 + 1)
	MOV		VREG_ADR1+1, >(comparator_a_addr1 + 1)
	MOV		VREG_STEP0, 1
	XFER		VREG_PORT1, (0)	;; lo byte of [a]

	MOV		VREG_STEP0, 0
	MOV		VREG_ADR1, <(addr1+3)
	MOV		VREG_ADR1+1, >(addr1+3)
	XFER		VREG_PORT1, (0) ;; hi byte of [a]
	MOV		VREG_ADR1, <(comparator_a_addr3 + 1)
	MOV		VREG_ADR1+1, >(comparator_a_addr3 + 1)
	MOV		VREG_STEP0, 1
	XFER		VREG_PORT1, (0) ;; hi byte of [a]
.endif


	;; copy address of [b] into three places: to read value to be negated and to store result of [b]-[a]
	MOV		VREG_STEP0, 0
	MOV		VREG_STEP1, 0
	MOV		VREG_ADR1, <(addr2+1)
	MOV		VREG_ADR1+1, >(addr2+1)
	XFER		VREG_PORT1, (0)	;; lo byte of [b]
	MOV		VREG_ADR1, <(addr2_2+1)
	MOV		VREG_ADR1+1, >(addr2_2+1)
	XFER		VREG_PORT1, (0) ;; lo byte of [b]
	MOV		VREG_ADR1, <(comparator_addr+1)
	MOV		VREG_ADR1+1, >(comparator_addr+1)
	MOV		VREG_STEP0, 1	; advance after next read
	XFER		VREG_PORT1, (0) ;; lo byte of [b]

	MOV		VREG_STEP0, 0
	MOV		VREG_ADR1, <(addr2+1+2)
	MOV		VREG_ADR1+1, >(addr2+1+2)
	XFER		VREG_PORT1, (0)	;; hi byte of [b]
	MOV		VREG_ADR1, <(addr2_2+1+2)
	MOV		VREG_ADR1+1, >(addr2_2+1+2)
	XFER		VREG_PORT1, (0) ;; hi byte of [b]
	MOV		VREG_ADR1, <(comparator_addr3 + 1)
	MOV		VREG_ADR1+1, >(comparator_addr3 + 1)
	XFER		VREG_PORT1, (0) ;; hi byte of [b]

	SETB		2	; I/O operation marker
	VJMP		comparator_a
back_from_comparator_a:
	MOV		VREG_STEP0, 0
	;; read value from [a], put as step 0 to be negated
addr1:
	MOV		VREG_ADR0, 0	; this will be modified
	MOV		VREG_ADR0+1, 0	; this will be modified
	MOV		VREG_ADR1, <(addrval_a+1)
	MOV		VREG_ADR1+1, >(addrval_a+1)
	XFER		VREG_PORT1, (0)

	;; put value from [a] in $ffff for possible use by the 6510
	MOV		VREG_ADR1, $ff
	MOV		VREG_ADR1+1, $ff
	XFER		VREG_PORT1, (0)

	BRA		comparator_trampoline
back_from_comparator:

	DECB
	BRA		no_output
	BRA		pcpos		; counter B == 0, output requested
no_output:
	DECB
	BRA		no_input
	MOV		VREG_ADR0, $ff
	MOV		VREG_ADR0+1, $ff
	SETB		1
	BRA		just_store_a
no_input:
	
	MOV		VREG_STEP0, 0
	MOV		VREG_STEP1, 0
addr2:
	;; read value from [b], put as step 0 into add/sign table offsets
	;; (step0,1 must be still set to 0, we read the value twice)
	MOV		VREG_ADR0, 0	; this will be modified
	MOV		VREG_ADR0+1, 0	; this will be modified
	MOV		VREG_ADR1, <(addrval_b+1)
	MOV		VREG_ADR1+1, >(addrval_b+1)
	XFER		VREG_PORT1, (0)
	MOV		VREG_ADR1, <(addrval_b2+1)
	MOV		VREG_ADR1+1, >(addrval_b2+1)
	XFER		VREG_PORT1, (0)

	;; first indexed read - what is -[a]? put it into addrval_aneg2 and addrval_aneg as step0 values
	MOV		VREG_ADR0, <(negtable+$80)	; middle of the table, position of 0
	MOV		VREG_ADR0+1, >(negtable+$80)	; middle of the table, position of 0
	MOV		VREG_ADR1, <(addrval_aneg+1)
	MOV		VREG_ADR1+1, >(addrval_aneg+1)
addrval_a:
	MOV		VREG_STEP0, 0		; this will be set to value from [a]
	XFER		VREG_PORT1, (0)		; read once and advance port0 by [a], but step1 is still 0
	MOV		VREG_STEP0, 0		; don't advance now, we need this value twice
	XFER		VREG_PORT1, (0)		; read -[a] and store at addrval_aneg
	MOV		VREG_ADR1, <(addrval_aneg2+1)
	MOV		VREG_ADR1+1, >(addrval_aneg2+1)
	XFER		VREG_PORT1, (0)		; read -[a] and store at addrval_aneg2

	;; is [b]-[a]>0?
	MOV		VREG_STEP1, 0	; PORT1 will be written thrice, we only want to know last value
	MOV		VREG_ADR0,   <(signtable+$100)	; middle of the table, position of '0'
	MOV		VREG_ADR0+1, >(signtable+$100)	; middle of the table, position of '0'
	MOV		VREG_ADR1, <(setaval+1)
	MOV		VREG_ADR1+1, >(setaval+1)
addrval_aneg:
	MOV		VREG_STEP0, 0		; step here will be -[a] value (-128,127)
	XFER		VREG_PORT1, (0)		; read value at 0, move from 0 to -[a] value
addrval_b:
	MOV		VREG_STEP0, 0		; step here will be [b] value (-128,127)
	XFER		VREG_PORT1, (0)		; read value at -[a], move from -[a] to [b] value
	XFER		VREG_PORT1, (0)		; finally read sign of subtraction result - 0 to skip BRA or <>0 to run BRA

	;; what is the actual value [b]-[a]?
	MOV		VREG_STEP1, 0	; PORT1 will be written thrice, we only want to know last value
	MOV		VREG_ADR0,   <(addtable+$100)	; middle of the table, position of '0'
	MOV		VREG_ADR0+1, >(addtable+$100)	; middle of the table, position of '0'
just_store_a:
addr2_2:
	;; store result in b
	MOV		VREG_ADR1, 0		; this will be modified
	MOV		VREG_ADR1+1, 0		; this will be modified
addrval_aneg2:
	MOV		VREG_STEP0, 0		; step here will be -[a] value (-128,127)
	XFER		VREG_PORT1, (0)		; read value at 0, move from 0 to -[a] value
	DECB			; check input marker
	BRA		pcpos
addrval_b2:
	MOV		VREG_STEP0, 0		; step here will be [b] value (-128,127)
	XFER		VREG_PORT1, (0)		; read value at -[a], move from -[a] to [b] value
	XFER		VREG_PORT1, (0)		; finally read subtraction result, store at addr2

setaval:
	SETA	0					; this will be modified
	DECA						; if A==0 branch will not be taken, when [a]>[b]
	BRA		pcleq				; branch if A<>0

pcpos:
	MOV		VREG_ADR0, 0		; branch not taken, take next instruction
	MOV		VREG_ADR0+1, 0
	BRA		pcrun

pcleq:
	MOV		VREG_ADR0, 0		; branch taken
	MOV		VREG_ADR0+1, 0

pcrun:
d020_val:
	MOV		$20, 6			; debug indicator
	MOV		VREG_DLIST2L, <mainloop
	MOV		VREG_DLIST2H, >mainloop
	SKIP
frame_end:
	WAIT		NTSC_LAST_SAFE_LINE,0   		; default frame-end marker suitable for NTSC
	MOV		VREG_DL2STROBE, 0

	MOV		$20, 15	 		; we have no time for processing next instruction, end this DL run
	END

signtable:
	; table with sign information, (1=negative or 0, 0=positive)
	; note: first/last 128 bytes indicate +/- overflow, in add table they are (respectively) positive/negative, should this be consistent with information here as well?
	.repeat 256	; negative numbers
	.byte 1
	.endrepeat
	.byte 1		; zero
	.repeat 255	; positive numbers
	.byte 0
	.endrepeat

addtable:
	; table for adding numbers, index by offset from the middle
	.repeat 256, I
	.byte I
	.endrepeat
	.repeat 256, I
	.byte I
	.endrepeat

negtable:
	; table for negating numbers, index by offset from the middle
	.repeat 128, I
	.byte 128-I
	.endrepeat
	.repeat 128, I
	.byte <(-I)
	.endrepeat

iocheck_table:
	.repeat 127
	.byte 1
	.endrep
	.byte 0
	.repeat 128
	.byte 1
	.endrep


	; subleq program encoding
	; <negative-jmp> <positive-jmp> <a> <b>; [b]<-[b]-[a]; if [b]-[a]<=0 then [negative-jmp] else [positive-jmp]

	.macro subleq addr_a, addr_b, jump_c
	.ifblank addr_a
		.error "SUBLEQ: first argument required for subleq macro"
	.endif
	.ifnblank jump_c
		.word jump_c	; where to jump if negative
	.else
		.word :+	; if ommited then point to the next instruction
	.endif
		.word :+	; link to next instruction (required for VASYL, not existing in pure Subleq)
		.word (addr_a & $ffff)	; [a]
	.ifnblank addr_b
		.word (addr_b & $ffff)	; [b], [b]<-[b]-[a]	;; "& $ffff" enables negative values
	.else
		.word (addr_a & $ffff)	; if 2nd argument is omitted reuse [a]
	.endif
	:
	.endmacro

vm_start:
	; subleq program starts here, addresses must be absolute, not relative to vm_start

	; debug cases for jumps:
;	subleq three, seven, sloop ; 7-3=4 4>=0 so no jump to sloop, infinite loop with no visuals
;	subleq seven, three, sloop ; 3-7=-4 4<0 so jump to sloop, infinite loop with visuals
;	subleq seven, seven, sloop ; 7-7=0, 0=0 so jump to sloop, infinite loop with visuals
;:	subleq zero, zero, :-	; infinite loop with no visuals

	; debug cases for arithmetics
	subleq three, seven		; 7-3=4, seven=4
	subleq two, isseven		; 0-2=-2, isseven=-2
	subleq isseven, five		; 5-(-2)=5+2=7, five=7
	subleq isseven			; zero-out location isseven

	subleq negone, three		; 3-(-1)=4, three=4


typist:
	subleq -1, char
	subleq char, -1
	subleq char_w, char, not_lower	; break on 'X'
	subleq one, char, print_hello
not_lower:
	subleq zero, zero, typist

; Following code only works by lucky coincidence - just the lo-bytes of ptrs need to be adjusted.
; We need 16-bit arithmetics!
print_hello:
	subleq char_counter
terminator_check:
	subleq zero, hello_txt, ploop
printer:
	subleq hello_txt, -1
	subleq negone, terminator_check+6
	subleq negone, printer+4
	subleq negone, char_counter
	subleq zero, zero, terminator_check
ploop:
	; restore start pointer
	subleq char_counter, terminator_check+6
	subleq char_counter, printer+4
	subleq one, d020_val+1		; visual feedback
	subleq zero, zero, print_hello	; jmp to beginning


sloop:	; infinite loop that changes border color by modifying display list directly
	.word sloop, sloop, one, d020_val+1
	subleq zero, zero, sloop	; infinite loop
:	.word :-, :-, zero, zero	; this is also infinite loop

	; subleq data space - memory, constants, registers, variables
zero:	.byte 0		; literal 0
seven:	.byte 7
three:	.byte 3

one:	.byte 1
two:	.byte 2
five:	.byte 5
isseven:	.byte 0
negone:	.byte $ff
hello_txt:	.byte "vasyl says hello!", $0d, 0
char_counter: .byte 0
char: .byte 0
char_w: .byte "w"
char_x: .byte "x"

; all the exports for debug purposes
; vpeek(seven) should be 4
; vpeek(isseven) should be 0
; vpeek(five) sohuld be 7
.export signtable
.export addtable
.export negtable
.export vm_start
.export zero
.export seven
.export three
.export two
.export five
.export isseven
.export dl_start
.export dl_restart
.export mainloop
.export pcleq
.export pcpos
.export addr1
.export addr2
.export addr2_2
.export addrval_a
.export addrval_aneg
.export addrval_aneg2
.export addrval_b
.export addrval_b2
.export setaval
.export d020_val
.export iocheck_table
.export char
