
;sample code 
				THUMB 		; Thumb instruction set 
                AREA 		My_code, CODE, READONLY
                EXPORT 		__MAIN
				ENTRY  
__MAIN

; Turn off all LEDs 
				LDR			R10, =LED_BASE_ADR		; R10 is a permenant pointer to the base address for the LEDs, offset of 0x20 and 0x40 for the ports

				MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				STR 		R3, [r10, #0x20]
				MOV 		R3, #0x0000007C
				STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 
				
				;Assign push button as input
				;MOV			R6,	#0X00
				;STRB		R6, [R10,#0X40]
				BL			COUNTER
				
; Initializes R11 to a 16-bit non-zero value, nothing else can write to R11
				MOV			R11, #0xABCD		; Init the random number generator with a non-zero number
				BL 			RandomNum
				MOV			R0, #0x4E20			;Want a 2 second delay, so set R0 to 20000
				BL			DELAY
				MOV			R0, #0xFFFF			;Move 16 1 bits into R0
				MOV			R7, #0X3FFF			;Move 14 1 bits into R7
				AND			R0, R11, R0			;Keep the first 16 bits of the random number
				AND			R7, R11, R7			;Keep the first 14 bits of the random number
				ADD			R0, R0, R7			;Add R0 and R7 and store in R0
				BL			DELAY
				
				
				
				
				
				;Turn on light
				MOV 		R3, #0xA0000000
				STR 		R3, [R10, #0x20]
				MOV			R9, #0
				
poll			LDR 		R6, =FIO2PIN		;Load pin address of the button
				LDR			R6,	[R6]			;Load the values of the address
				
				MOV			R0, #1				;Want a delay of 1 millisecond, so move 1 into R0
				BL			DELAY
				ADD			R9, #1				;Increment R9, which acts as the reflex time counter
				
				AND			R6,R6,#0x00000400	;Get value of pin 10 (the button)
				SUBS		R6,R6,#0x00000400	;Subtract 1 from pin 10
				TEQ			R6, #0				;Test if the result is zero (the button was not pressed)
				BEQ			poll				;Keep polling if the button was not pressed
				;TURN OFF LED
				MOV 		R3, #0xB0000000
				STR 		R3, [r10, #0x20]
				
loop			BL			DISPLAY_NUM
				
				B loop				;END OF MAIN LOOP

DISPLAY_NUM		STMFD		R13!,{R1, R2,R7, R14}
				MOV			R4, #4					;Move 4 into R4 which will be used to count how many bytes have been displayed
				MOV			R7, R9					;Move the reflex timer value into R7
LOOP_BYTE		AND			R8 ,R7,#0xFF			;AND the timer value to get the least significant byte
				
				MOV			R1,	#0					;Clear R1 to 0
				MOV			R2, #0					;Clear R2 to 0
				
				BFI			R2, R8, #27, #5			;Move the 5 least significant bits of the byte into bit positions 27 to 31 in R2 
				;LSL			R2,	R9,	#27
				RBIT		R2,	R2					;Reverse the bits of R2
				EOR			R2, R2, #0x1F			;Invert the bits of R2
				LSL			R2,	R2,	#2				;Shift R2 to the left by 2 bit positions to prepare to store into P2.2 to P2.6
				STR 		R2, [r10, #0x40]		;Store R2 into the five port 2 LEDs
				
				LSR			R1, R8,#5				;Shift the 3 most significant bits of the byte to the 3 least significant bits of R1
				RBIT		R1,	R1					;Reverse the bits of R1
				ASR			R1,R1,#1				;Arthimetic shift right by 1 bit position (ASR instead of LSR because we want the MSB to remain the same)
				EOR			R1, R1, #0xB0000000		;EOR by B to invert the bits of R1 and because B = 1011 because we only want to keep bits 31, 29, and 28
				AND			R1,R1,#0xB0000000		;AND by B to keep bits 31,29,28
				STR 		R1, [r10, #0x20]		;Store R1 into the port 1 LEDs
				
				LSR			R7,R7, #8				;Get the next byte
				MOV			R0, #0x4E20				;Use R0 of 20000 to get delay of 2 seconds
				BL			DELAY
				MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				STR 		R3, [r10, #0x20]
				MOV 		R3, #0x0000007C
				STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2
				SUBS		R4, R4, #1
				BNE			LOOP_BYTE			;If there are still bytes left to display loop back
				MOV			R0, #0xC350			;Five second delay after the entire 32 bit word has been displayed
				BL			DELAY
				LDMFD		R13!,{R1, R2,R7, R15}



COUNTER		STMFD		R13!,{R1, R2,R4, R14}
; Usefull commaands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations
				MOV			R4, #0
				
LOOP_Counter		AND			R4 ,R4,#0xFF
				
				MOV			R1,	#0					;Clear R1 to 0
				MOV			R2, #0					;Clear R2 to 0
				
				BFI			R2, R4, #27, #5			;Move the 5 least significant bits of the byte into bit positions 27 to 31 in R2
				RBIT		R2,	R2					;Reverse the bits of R2
				EOR			R2, R2, #0x1F			;Invert the bits of R2
				LSL			R2,	R2,	#2				;Shift R2 to the left by 2 bit positions to prepare to store into P2.2 to P2.6
				STR 		R2, [r10, #0x40]		;Store R2 into the five port 2 LEDs
				
				LSR			R1, R4,#5				;Shift the 3 most significant bits of the byte to the 3 least significant bits of R1
				RBIT		R1,	R1					;Reverse the bits of R1
				ASR			R1,R1,#1				;Arthimetic shift right by 1 bit position (ASR instead of LSR because we want the MSB to remain the same)
				EOR			R1, R1, #0xB0000000		;EOR by B to invert the bits of R1 and because B = 1011 because we only want to keep bits 31, 29, and 28
				AND			R1,R1,#0xB0000000		;AND by B to keep bits 31,29,28
				STR 		R1, [r10, #0x20]		;Store R1 into the port 1 LEDs
				
				MOV			R0, #0x03E8				;Want a 0.1 second delay between each number
				BL			DELAY
				MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				STR 		R3, [r10, #0x20]
				MOV 		R3, #0x0000007C
				STR 		R3, [R10, #0x40] 	; Turn off five LEDs on
				ADD			R4, R4,#1
				B			LOOP_Counter
				LDMFD		R13!,{R1, R2,R4, R15}
;
; R11 holds a 16-bit random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
; R11 holds a non-zero 16-bit number.  If a zero is fed in the pseudo-random sequence will stay stuck at 0
; Take as many bits of R11 as you need.  If you take the lowest 4 bits then you get a number between 1 and 15.
;   If you take bits 5..1 you'll get a number between 0 and 15 (assuming you right shift by 1 bit).
;
; R11 MUST be initialized to a non-zero 16-bit value at the start of the program OR ELSE!
; R11 can be read anywhere in the code but must only be written to by this subroutine
RandomNum		STMFD		R13!,{R1, R2, R3, R14}

				AND			R1, R11, #0x8000
				AND			R2, R11, #0x2000
				LSL			R2, #2
				EOR			R3, R1, R2
				AND			R1, R11, #0x1000
				LSL			R1, #3
				EOR			R3, R3, R1
				AND			R1, R11, #0x0400
				LSL			R1, #5
				EOR			R3, R3, R1		; the new bit to go into the LSB is present
				LSR			R3, #15
				LSL			R11, #1
				ORR			R11, R11, R3
				
				LDMFD		R13!,{R1, R2, R3, R15}

;
;		Delay 0.1ms (100us) * R0 times
; 		aim for better than 10% accuracy
DELAY			STMFD		R13!,{R2, R14}
MultipleDelay		TEQ		R0, #0		; test R0 to see if it's 0 - set Zero flag so you can use BEQ, BNE
		BEQ exitDelay
		MOV 		R5, #0x7F 	; Initialize R5 to 127 for a counter that implements a 0.1 millisecond delay
SmallDelay
		SUBS R5,#1					;Decrement counter
		BNE SmallDelay				; While R5 is not equal to 0, keep looping and decrementing R5
		SUBS R0,#1					; Decrement the multiple delay counter
		B MultipleDelay
exitDelay		LDMFD		R13!,{R2, R15}


LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
PINSEL3			EQU 	0x4002c00c 		; Address of Pin Select Register 3 for P1[31:16]
PINSEL4			EQU 	0x4002c010 		; Address of Pin Select Register 4 for P2[15:0]
FIODIR			EQU		0x2009C040
FIO2PIN			EQU		0x2009C054		
;	Usefull GPIO Registers
;	FIODIR  - register to set individual pins as input or output
;	FIOPIN  - register to read and write pins
;	FIOSET  - register to set I/O pins to 1 by writing a 1
;	FIOCLR  - register to clr I/O pins to 0 by writing a 1

				ALIGN 

				END 
;Proof of 2-10Second delay:
;Take lowest 16 bits, and lowest 14 bits of random number
;add these 2 numbers, run the delay loop with them
;(10^-4)*(2^16+2^14) ~8.2 seconds
;So this operation gives a range rom 0 to 8.2 seconds
;Combined with an intiial delay of 2 seconds, one gets about a 2-10 second delay