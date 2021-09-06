
; BeamRacer SUBLEQ Virtual Machine
;
; Maciej 'YTM/Elysium' Witkowiak, 2021
;



        .include "vlib/vasyl.s"

        jsr knock_knock
        jsr copy_and_activate_dlist
 
	rts

	.include "vlib/vlib.s"

	.segment "VASYL"


	;; instruction action:
	;;	[b] <- [b]-[a] = -[a]+[b]; if b<=0 then goto 0
	;; instruction encoding:
	;; (address to jump when result negative or zero - c) (address to jump when result positive) (address of a) (address of b (result))

dl_start:
	; enable reading from both ports, we're in bank0, DL enabled
	MOV		VREG_CONTROL, %00011000	; this can be done from C64 setup
	; start of vm program
	MOV		VREG_ADR0, <vm_start	; this can be done from C64 setup
	MOV		VREG_ADR0+1, >vm_start
	; make DL restart at mainloop so that program continues in the new frame
	MOV		VREG_DLISTL, <dl_restart
	MOV		VREG_DLISTH, >dl_restart
	MOV		VREG_DLIST2L, <mainloop
	MOV		VREG_DLIST2H, >mainloop

	WAIT	300, 0  ; this WAIT can only complete on PAL - subsequent instructions won't be executed on an NTSC machine.
	MOV		VREG_ADR1, <(frame_end+1)   ; adjust frame-end marker for PAL
	MOV		VREG_ADR1+1, >(frame_end+1)
	MOV		VREG_PORT1, <311
	END

dl_restart:
;	SETB		214 ;; how many instructions to run per frame? we can't risk DL restart in the middle of self-modification routine
					;; 214 is max, maybe even 215, this can be increased if all redundant writes to ADR0/1 high-bytes are optimized away
	MOV		$20, 2		;; indicator start
mainloop:
	MOV		VREG_STEP0, 1
	MOV		VREG_STEP1, 2	;; skip 2 bytes - over next 2 instructions for all self-modifying writes

	MOV		VREG_ADR1, <(pcleq+1)
	MOV		VREG_ADR1+1, >(pcleq+1)
	XFER		VREG_PORT1, (0) ;; lo byte of new PC (if neq), branch after DECA taken
	XFER		VREG_PORT1, (0) ;; hi byte of new PC (if neq), branch after DECA taken

	MOV		VREG_ADR1, <(pcpos+1)
	MOV		VREG_ADR1+1, >(pcpos+1)
	XFER		VREG_PORT1, (0) ;; lo byte of new PC (if positive), branch after DECA not taken, just address of the next instruction
	XFER		VREG_PORT1, (0) ;; hi byte of new PC (if positive), branch after DECA not taken, just address of the next instruction

	;; copy address of [a] to place where [a] will be read
	MOV		VREG_ADR1, <(addr1+1)
	MOV		VREG_ADR1+1, >(addr1+1)
	XFER		VREG_PORT1, (0)	;; lo byte of [a]
	XFER		VREG_PORT1, (0) ;; hi byte of [a]

	;; copy address of [b] to two places: to read value to be negated and store result of [a]-[b]
	MOV		VREG_STEP0, 0
	MOV		VREG_STEP1, 0
	MOV		VREG_ADR1, <(addr2+1)
	MOV		VREG_ADR1+1, >(addr2+1)
	XFER		VREG_PORT1, (0)	;; lo byte of [b]
	MOV		VREG_ADR1, <(addr2_2+1)
	MOV		VREG_ADR1+1, >(addr2_2+1)
	MOV		VREG_STEP0, 1	; advance after next read
	XFER		VREG_PORT1, (0) ;; lo byte of [b]

	MOV		VREG_STEP0, 0
	MOV		VREG_ADR1, <(addr2+1+2)
	MOV		VREG_ADR1+1, >(addr2+1+2)
	XFER		VREG_PORT1, (0)	;; hi byte of @a2
	MOV		VREG_ADR1, <(addr2_2+1+2)
	MOV		VREG_ADR1+1, >(addr2_2+1+2)
	XFER		VREG_PORT1, (0) ;; hi byte of @a2

	;; read value from [a], put as step0 one place, to be negated, step0/1 set to 0, because we need this value twice
addr1:
	MOV		VREG_ADR0, 0	; this will be modified
	MOV		VREG_ADR0+1, 0	; this will be modified
	MOV		VREG_ADR1, <(addrval_a+1)
	MOV		VREG_ADR1+1, >(addrval_a+1)
	XFER		VREG_PORT1, (0)
addr2:
	;; read value from [b], put as step0, step0/1 don't matter (still 0)
	MOV		VREG_ADR0, 0	; this will be modified
	MOV		VREG_ADR0+1, 0	; this will be modified
	MOV		VREG_ADR1, <(addrval_b+1)
	MOV		VREG_ADR1+1, >(addrval_b+1)
	XFER		VREG_PORT1, (0)
	MOV		VREG_ADR1, <(addrval_b2+1)
	MOV		VREG_ADR1+1, >(addrval_b2+1)
	XFER		VREG_PORT1, (0)

	;; first indexed read - what is -[a]? put it into addrval_aneg2 and addrval_aneg as step0 values
	MOV		VREG_ADR0,	<(negtable+$80)		; middle of the table, position of 0
	MOV		VREG_ADR0+1, >(negtable+$80)	; middle of the table, position of 0
	MOV		VREG_ADR1,	<(addrval_aneg+1)
	MOV		VREG_ADR1+1, >(addrval_aneg+1)
addrval_a:
	MOV		VREG_STEP0, 0		; this will be set to value from [a]
	XFER		VREG_PORT1, (0)		; read once and advance port0 by [a], but step1 is still 0
	MOV		VREG_STEP0, 0		; dont advance now, we need this value twice
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
addr2_2:
	;; store result in b
	MOV		VREG_ADR1, 0		; this will be modified
	MOV		VREG_ADR1+1, 0		; this will be modified
addrval_aneg2:
	MOV		VREG_STEP0, 0		; step here will be -[a] value (-128,127)
	XFER		VREG_PORT1, (0)		; read value at 0, move from 0 to -[a] value
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
	MOV		$20, 6	;; debug indicator
	SKIP
frame_end:
	WAIT	260,0   ; default frame-end marker suitable for NTSC
	MOV		VREG_DL2STROBE, 0

	MOV		$20, 15	 ; no, end this DL run
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
	; table for adding numbers, index by offset rom the middle
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
	; 0, 127, 126, ..., 0 (@$80), -1=$FF, -2=$FE ..., -127, -128 ; (x :-> -x), but [$7f, ..., 0, $ff, $fe, ..., $80]?

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
		.word addr_a	; [a]
	.ifnblank addr_b
		.word addr_b	; [b], [b]<-[b]-[a]
	.else
		.word addr_a	; if 2nd argument is omitted reuse [a]
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
