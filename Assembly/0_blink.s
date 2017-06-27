;*----------------------------------------------------------------------------
;* This code flashes one LED at approximately 1 Hz frequency 
;*----------------------------------------------------------------------------*/
	THUMB		; Declare THUMB instruction set 
	AREA		My_code, CODE, READONLY 	; 
	EXPORT		__MAIN 		; Label __MAIN is used externally q
	ENTRY 
__MAIN

	MOV 		R2, #0xC000		; move 0xC000 into R2
	MOV 		R4, #0x0		; init R4 register to 0 to build address
	MOVT 		R4, #0x2009		; assign 0x20090000 into R4
	ADD 		R4, R4, R2 		; add 0xC000 to R4 to get 0x2009C000 

	MOV 		R3, #0x0000007C	; move initial value for port P2 into R3 
	STR 		R3, [R4, #0x40] 	; Turn off five LEDs on port 2 

	MOV 		R3, #0xB0000000	; move initial value for port P1 into R3
	STR 		R3, [R4, #0x20]	; Turn off three LEDs on Port 1 using an offset

	MOV 		R2, #0x20		; put Port 1 offset into R2 for user later
	MOV 		R3, #0x10000000	; Move 10000000 into R3
	LDR			R5, [R4, #0x20]	; Load the value of the LED into R5
TOGGLE
	MOV 		R0, #0xFFFF 	; Initialize R0 lower word for countdown to FFFF
	MOVT		R0, #0x000A		; Initialize R0 upper word for countdown to 000A	
DECREMENT
	SUBS 		R0, #1 			; Decrement R0 and set the N,Z,C status bits
	
	BNE 			DECREMENT	; If R0 is not equal to zero, loop back up to the start of the DECREMENT loop

	
	EOR			R5, R3, R5		; Exclusive OR R3 and R5, store result in R5 (toggle the 28th bit)
	STR 		R5, [R4, #0x20]	; Store the value of R5 into the address of the LED
	
	BEQ			TOGGLE 			; R0 is equal to zero, so loop back up to the start of the TOGGLE loop
 	END 