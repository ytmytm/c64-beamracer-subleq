
# 16-bit SUBLEQ Virtual Machine for BeamRacer

* Author: Maciej 'YTM/Elysium' Witkowiak <ytm@elysium.pl>
* License: Public Domain

This project contains a virtual machine for programs written in SUBLEQ.

The virtual machine itself is a self-modifying Display List executed by
VASYL - programmable coprocessor inside [BeamRacer](http://beamracer.net),
a hardware expansion of Commodore C64.

# What is BeamRacer?

You can just go and [read the specification](https://beamracer.net/#specifications).

Here all we need to know that BeamRacer contains a coprocessor called VASYL
(Video Assistance and Suport Logic) that has several features:

* it runs its Display List programs independently from C64's CPU, one instruction per CPU clock (1MHz)
* it has its own memory with automatic adjustment of target address with configurable offset after every read or write
* it has several instructions to write data into control registers (VIC's and VASYL's)

VASYL is not suited for general computing because the instruction set and features are very limited:

* no arithmetic instructions (add/sub), there is only decrementation of an internal counter
* no CPU flags (zero, carry, sign)
* the only kind of conditional jump is when internal counter reaches zero, then a following unconditional jump instruction can be skipped
* you can read **only** from one of two memory access ports
* you can write **only** to control registers (fortunately two of them being memory access ports)
* there are no general purpose registers, like A, X or Y on 6502 CPU
* there is no stack

# What is SUBLEQ?

This is a name for One-Instruction Set Computer architecture.

The only possible instruction is "SUBtract and branch if Less-than or EQual to zero".

```	; subleq a, b, c
	Mem[b] = Mem[b]-Mem[a]
	if (Mem[b]<=0) then goto c
```

It turns out that such machines are Turing Complete: using this single instruction
we can define all operations needed for general computing.

Since BeamRacer can execute programs written for SUBLEQ CPu I have proven that VASYL's ISA
is also Turing Complete. This was not at all evident for me when I first started thinking about this project.

There is a very nice introduction to SUBLEQ on [esolangs.org](https://esolangs.org/wiki/Subleq).

# VASYL SUBLEQ VM

Almost whole Display List program in [br-subleq.s](br-subleq.s) is devoted to running the virtual machine.

## Implementation

There are two arithmetic operations need: addition and negation. Both are not available in native VASYL ISA, and we use
lookup tables for this purpose.

We also need one more table to check for sign of the result and decide which branch to take as the next instruction.

The display list modifies itself in two ways:

* source and target addresses for instruction addresses and values are stored in place of data for immediate register load instructions
* lookup table indexing is done by modifying register loads to STEP0/STEP1 registers - these two control by how many bytes the pointer will advance after next read or write to memory port

While the display list is running you can't modify any PORT0/PORT1 settings.

## Performance

The display list is restarted on every frame (50 times per second) and we can't allow this happen in the middle of processing SUBLEQ instruction. Therefore there is an upper limit to how many SUBLEQ instructions can be safely executed every frame.
Without more optimizations currently this is about 214.

Therefore this virtual SUBLEQ CPU runs clocked at about 10.7 kHz (214*50).

## Available memory

BeamRacer has 512KB of RAM in 8 memory banks, 64KB each. Unfortunately there is a single memory bank setting for both memory ports, so we can't read SUBLEQ program from bank 1 and modify display list in bank 0. We can only use one bank at a time.

The VM and lookup tables occupy about 1.5KB of the bank0. The remaining 62.5KB of bank0 is free for SUBLEQ programs and data.

You could setup seven more VMs and programs in remaining RAM banks.

## How the SUBLEQ program looks like

In this impementation the values are signed 8-bit numbers and program consists of four 16-bit words.
All instructions are explicitly linked, you need to provide pointers to both possible outcomes.

This is different than other SUBLEQ implementations listed in references, where program counter advances implicitly to the
next instruction if the result of operation is positive. To make the VM more efficient I have also reordered the way how instructions are stored.

SUBLEQ has only one instruction so we don't need to store the opcode. All we need is data:


```
	.word <address of the next instruction when result is negative or zero>
	.word <address of the next instruction when result is positive>
	.word <address of value 'a'>
	.word <address of value 'b' and also address where result b-a will be stored>
```

This structure is wrapped in macro `subleq` with one required and two optional arguments:

```
	subleq a, b, c	; set b to b-a and jump to c if b-a is negative or zero
	subleq a, b	; set b to b-a and take next instruction (regardless of the result) from next byte
	subleq b	; set b to b-b and take next instruction
```

# Communication with C64 CPU

Reference SUBLEQ implementations use a special memory address (-1, $FFFF) for I/O. For example writing something to $FFFF means
printing out a character in Forth implementation linked below.

Data exchange can be done through memory ports in both directions, but only when SUBLEQ program is not running. It's easy to start the program - just enable Display List.

There are some ways how SUBLEQ program can indicate to the host computer that is has completed:

* it can modify VM's display list and just make it stop by writing END opcode into it or by encoding a write to control register
* it can make display list write a special value to one of VIC registers
* it can make display list to run IRQ command to trigger C64 CPU interrupt

In this implementation I'm using the second method for debug. I can see that the program is running through ever-changing border colour:

```
	; in the display list
d020_val:
	MOV $d020, 5			; $d020 is border colour, this 5 value is at address d020_val+1

	; in the subleq program
one:	.byte 1
zero:	.byte 0

vm_start:				; program starts here
	subleq one, d020_val+1		; set [d020_val+1] to [d020_val+1]-[one], so subtract 1 from current value and store it
	subleq zero, zero, vm_start	; infinite loop to vm_start
```

# Future

Can we have shaders processing bitmaps and running natively in VASYL?

We would need some sort of assembly lanugage built upon macros on top of `subleq` instruction for easier programming.

There is already a Forth implementation (linked below), maybe that can be reused?

I'm not an experienced SUBLEQ programmer so the only examples I provide are debug cases at the end of [br-subleq.s](br-subleq.s).

# References

* https://esolangs.org/wiki/Subleq (Turorial)
* https://github.com/howerj/subleq (Forth)
* https://en.wikipedia.org/wiki/One-instruction_set_computer
* https://hackaday.io/project/158329-dawn-the-subleq-operating-system-by-geri
* https://www.researchgate.net/publication/51911189_A_Simple_Multi-Processor_Computer_Based_on_Subleq/download (SUBLEQ implemented in FPGA)
* https://github.com/davidar/subleq (simulator and libraries)

