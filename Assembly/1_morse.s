;*----------------------------------------------------------------------------
;* Program reads 4 character string, converts into morse code, then putputs to board
;*----------------------------------------------------------------------------*/
		THUMB 		; Declare THUMB instruction set 
                AREA 		My_code, CODE, READONLY 	; 
                EXPORT 		__MAIN 		; Label __MAIN is used externally q
		ENTRY 
__MAIN
__MAIN
; Turn off all LEDs 
		MOV 		R2, #0xC000
		MOV 		R3, #0xB0000000	
		MOV 		R4, #0x0
		MOVT 		R4, #0x2009
		ADD 		R4, R4, R2 		; 0x2009C000 - the base address for dealing with the ports
		STR 		R3, [r4, #0x20]		; Turn off the three LEDs on port 1
		MOV 		R3, #0x0000007C
		STR 		R3, [R4, #0x40] 	; Turn off five LEDs on port 2 
ResetLUT
		LDR         R5, =InputLUT            ; assign R5 to the address of the word's first letter

NextChar
        LDRB        R0, [R5]		; Read a character to convert to Morse
        ADD         R5, #1              ; point to the next letter in the word
		TEQ         R0, #0              ; If we hit 0 (null at end of the string) then reset to the start of lookup table
		BNE		ProcessChar	; If we have a character process it

		MOV		R0, #4		; delay 4 extra spaces (7 total) between words
		BL		DELAY
		BEQ         ResetLUT

ProcessChar	BL		CHAR2MORSE	; convert ASCII to Morse pattern in R1		


; Reverse the Morse Code LUT and encode the bits as follows (01 = short, 11 = 3 delay long, 00 = done)
; Shift right and peel off 2 bits at a time, encoded information alternates between on and off
; First 01 or 11 count is LED on, and the following one is off, then the next is on ....  till 00 is hit
		BL LED_OFF	;Force led off in case it is still on
Loop2					
		LSRS R1,R1,#1	; Shift the morse code pattern to the right once 
		BCS		Blink	; Go to Blink if the character that was shifted is a 1
		; If the morse pattern for the character is over:
		BL	LED_OFF	; Turn LED off
		MOV	R0, #3	;Want long delay after the end of a character
		BL	DELAY
		B	NextChar
Blink
		BL	LED_TOGGLE
		LSRS R1,R1,#1	; Shift the morse code pattern to the right once 
		MOVCC	R0, #1	; If the second bit is 0 we want a short delay
		MOVCS	R0, #3	; If the second bit is 1 we want a long delay
		BL		DELAY
		BL		Loop2	;Go to the next bit
		
		
; Subroutines
;
;			convert ASCII character to Morse pattern
;			pass ASCII character in R0, output in R1
;			index into MorseLuT must be by steps of 2 bytes
CHAR2MORSE	STMFD		R13!,{R14}	; push Link Register (return address) on stack
		SUB			R1,R0,0x0041	;Subtract by 41 to get the index of letter in the lookup table
		LSL			R1,#1			;Since the morse code pattern binary number is stored in a half word, must multiple the index by 2
		LDR         R7, =MorseLUT	;Loads the memory address for the start of the morse code table
		LDRH        R1, [R7,R1]		; Read a character to convert to Morse
		LDMFD		R13!,{R15}	; restore LR to R15 the Program Counter to return


; Turn the LED on, but deal with the stack in a simpler way
; NOTE: This method of returning from subroutine (BX  LR) does NOT work if subroutines are nested!!

LED_ON 	   	PUSH 		{r3-r4}		; preserve R3 and R4 on the R13 stack
		MOV		R10, 0xA0000000		; Move the value that turns on the LED into R10
		STR 	R10, [R4, #0x20]	; Store the turn on value into the address of the LED
		POP 	{r3-r4}
		BX 		LR		; branch to the address in the Link Register.  Ie return to the caller

; Turn the LED off, but deal with the stack in the proper way
; the Link register gets pushed onto the stack so that subroutines can be nested
;
LED_OFF	   	STMFD		R13!,{R3,R14}	; push R3 and Link Register (return address) on stack
		MOV		R10, 0xB0000000			; Move the value that turns off the LED into R10
		STR 		R10, [R4, #0x20]	; Store the turn off value into the address of the LED
		LDMFD		R13!,{R3,R15}	; restore R3 and LR to R15 the Program Counter to return

LED_TOGGLE	   	STMFD		R13!,{R3,R14}	; push R3 and Link Register (return address) on stack
		MOV			R11, #0x10000000	; This is the exclusive or factor that will be used to toggle the 28th bit
		LDR			R10, [R4, #0x20]	; Load the value of the LED into R10
		EOR			R10, R11, R10		; Exclusive OR R10 and R11, store result in R10 (toggle the 28th bit)
		STR 		R10, [R4, #0x20]	; This toggles the LED 
		LDMFD		R13!,{R3,R15}	; restore R3 and LR to R15 the Program Counter to return

;	Delay 500ms * R0 times
;
DELAY			STMFD		R13!,{R2, R14}

MultipleDelay		TEQ		R0, #0		; test R0 to see if it's 0 - set Zero flag so you can use BEQ, BNE
		BEQ exitDelay
		MOV 		R6, #0xFFFF 	; Initialize R6 lower word for countdown to FFFF
		MOVT		R6, #0x000A		; Initialize R6 upper word for countdown to 000A
SmallDelay
		SUBS R6,#1
		BNE SmallDelay				; While R6 is not equal to 0, keep looping and decrementing R6
		SUBS R0,#1					; Decrement the multiple delay counter
		B MultipleDelay
exitDelay		LDMFD		R13!,{R2, R15}

;
; Data used in the program
; DCB is Define Constant Byte size
; DCW is Define Constant Word (16-bit) size
; EQU is EQUate or assign a value.  This takes no memory but instead of typing the same address in many places one can just use an EQU
;
		ALIGN				; make sure things fall on word addresses

; One way to provide a data to convert to Morse code is to use a string in memory.
; Simply read bytes of the string until the NULL or "0" is hit.  This makes it very easy to loop until done.
;
InputLUT	DCB		"ABCDE", 0	; strings must be stored, and read, as BYTES

		ALIGN				; make sure things fall on word addresses
MorseLUT 
		DCW 	0x35, 0x1557, 0x1757, 0x157 	; A, B, C, D
		DCW 	0x1, 0x1755, 0x177, 0x1555 	; E, F, G, H
		DCW 	0x15, 0x3775, 0x357, 0x1575 	; I, J, K, L
		DCW 	0x37,0x17, 0x377, 0x1775 	; M, N, O, P
		DCW 	0x3577, 0x175, 0x155, 0x3 	; Q, R, S, T
		DCW 	0x355, 0x3555, 0x375, 0x3557 	; U, V, W, X
		DCW 	0x3757, 0x1577 			; Y, Z

; One can also define an address using the EQUate directive
;
LED_PORT_ADR	EQU	0x2009c000	; Base address of the memory that controls I/O like LEDs

		END 
