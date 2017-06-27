;*-------------------------------------------------------------------
;* Code to implement interrupts
;*-------------------------------------------------------------------
				THUMB 								; Declare THUMB instruction set 
				AREA 	My_code, CODE, READONLY 	; 
				EXPORT 		__MAIN 					; Label __MAIN is used externally 
				EXPORT 		EINT3_IRQHandler 	; without this the interupt routine will not be found

				ENTRY 

__MAIN

; Turn off all LEDs 
				LDR			R10, =LED_BASE_ADR		; R10 is a  pointer to the base address for the LEDs
				MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				STR 		R3, [r10, #0x20]
				MOV 		R3, #0x0000007C
				STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 
				
				
				;Enable the EINT3 channel with ISER0 (Interrupt Set Enable 0)
				LDR			R4, =ISER0	;Get address of ISER0
				MOV			R3, #0X0	
				MOVT		R3, #0X0020 ;Want only the bit 21 of R3 to be 1
				STR			R3,	[R4]	;Write 1 into bit 21 of ISER0
				
				;Enable the GPIO interrupt on pin 2.10 (INT0) for falling edge
				LDR			R4, =IO2IntEnf	;Get address of IO2IntEnf
				MOV			R3, #0X0
				MOV			R3, #0X0400	;Want only the bit 10 of R3 to be 1
				STR			R3,	[R4]	;Write 1 into bit 10 of IO2IntEnf
				
; Initializes R6 to a 16-bit non-zero value, nothing else can write to R6
				MOV			R11, #0xABCD		; Init the random number generator with a non-zero number
LOOP 			BL 			RNG
				MOV			R6, R11				;Get random number from R11 and move into R6
				MOV			R8, #0xFFFF
				MOVT		R8, #0x0000			
				AND			R6, R6, R8			;Only want the low-order 16 bits
				MOV			R7, #0x0148			;Scaling factor of 328 in hex
				SDIV		R6, R6, R7				
				ADD			R6, R6, #0x32		;Offset of 50 in hex
				
				;MOV			R0, R6
				;BL			DELAY				; Delay of 0 - 19.2 seconds
				MOV			R8, #0
				MOV			R0, #0xA			;R0 is 10 in hex, so that the program has a delay of 10*0.1s = 1s
SUB_RANDOM		BL			DISPLAY_BYTE
				BL			DELAY
				
				CMP			R6, #0x9			;Compare R6 to 9
				SUBGT		R6, R6, #10			;Decrement the random number by 10, if it is greater than 9
				MOVGT		R0, #0xA			;R0 is 10 in hex, so that the program has a delay of 1*0.1s = 0.1s
				BGT			SUB_RANDOM
				
				MOV			R6, #-1				;Ensure R6 is less than 0, also less than 9
				EOR			R8, R8, #0xFF		;Toggle R8 between 0 and FF to flash LEDs
				BFI			R6, R8, #0, #8		;Move either FF or 0 in to the low-order 8 bits of R6, which are the only bits that will be displayed
				MOV			R0, #1			;R0 is 10 in hex, so that the program has a delay of 1*0.1s = 0.1s
				B			SUB_RANDOM
				
				
				MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				STR 		R3, [r10, #0x20]
				MOV 		R3, #0x0000007C
				STR 		R3, [R10, #0x40] 	; Turn off five LEDs on Port 2
				
				
				B 			LOOP

DISPLAY_BYTE		STMFD		R13!,{R1, R2, R14}				
				MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				STR 		R3, [r10, #0x20]
				MOV 		R3, #0x0000007C
				STR 		R3, [R10, #0x40] 	; Turn off five LEDs on Port 2
				
				MOV			R1,	#0					;Clear R1 to 0
				MOV			R2, #0					;Clear R2 to 0
				
				BFI			R2, R6, #27, #5			;Move the 5 least significant bits of the byte into bit positions 27 to 31 in R2
				RBIT		R2,	R2					;Reverse the bits of R2
				EOR			R2, R2, #0x1F			;Invert the bits of R2
				LSL			R2,	R2,	#2				;Shift R2 to the left by 2 bit positions to prepare to store into P2.2 to P2.6
				STR 		R2, [r10, #0x40]		;Store R2 into the five port 2 LEDs
				
				LSR			R1, R6,#5				;Shift the 3 most significant bits of the byte to the 3 least significant bits of R1
				RBIT		R1,	R1					;Reverse the bits of R1
				ASR			R1,R1,#1				;Arthimetic shift right by 1 bit position (ASR instead of LSR because we want the MSB to remain the same)
				EOR			R1, R1, #0xB0000000		;EOR by B to invert the bits of R1 and because B = 1011 because we only want to keep bits 31, 29, and 28
				AND			R1,R1,#0xB0000000		;AND by B to keep bits 31,29,28
				STR 		R1, [r10, #0x20]		;Store R1 into the port 1 LEDs

				LDMFD		R13!,{R1, R2, R15}
;*------------------------------------------------------------------- 
; Subroutine RNG ... Generates a pseudo-Random Number in R11 
;*------------------------------------------------------------------- 
; R11 holds a random number as per the Linear feedback shift register (Fibonacci) on WikiPedia
; R11 MUST be initialized to a non-zero 16-bit value at the start of the program
; R11 can be read anywhere in the code but must only be written to by this subroutine
RNG 			STMFD		R13!,{R1-R3, R14} 	; Random Number Generator 
				AND			R1, R11, #0x8000
				AND			R2, R11, #0x2000
				LSL			R2, #2
				EOR			R3, R1, R2
				AND			R1, R11, #0x1000
				LSL			R1, #3
				EOR			R3, R3, R1
				AND			R1, R11, #0x0400
				LSL			R1, #5
				EOR			R3, R3, R1			; The new bit to go into the LSB is present
				LSR			R3, #15
				LSL			R11, #1
				ORR			R11, R11, R3
				LDMFD		R13!,{R1-R3, R15}

;*------------------------------------------------------------------- 
; Subroutine DELAY ... Causes a delay of 1ms * R0 times
;*------------------------------------------------------------------- 
; 		aim for better than 10% accuracy
DELAY			STMFD		R13!,{R5, R14}
MultipleDelay		TEQ		R0, #0		; test R0 to see if it's 0 - set Zero flag so you can use BEQ, BNE
		BEQ exitDelay
		MOV 		R5, #0xF018 	; Initialize R5 to F018 for a counter that implements a 0.1 second delay
		MOVT		R5, #0x0001
SmallDelay
		SUBS R5,#1					;Decrement counter
		BNE SmallDelay				; While R5 is not equal to 0, keep looping and decrementing R5
		SUBS R0,#1					; Decrement the multiple delay counter
		B MultipleDelay
exitDelay		LDMFD		R13!,{R5, R15}

; The Interrupt Service Routine MUST be in the startup file for simulation 
;   to work correctly.  Add it where there is the label "EINT3_IRQHandler
;
;*------------------------------------------------------------------- 
; Interrupt Service Routine (ISR) for EINT3_IRQHandler 
;*------------------------------------------------------------------- 
; This ISR handles the interrupt triggered when the INT0 push-button is pressed 
; with the assumption that the interrupt activation is done in the main program
EINT3_IRQHandler 	
				STMFD		R13!,{R4,R7,R14}				; Use this command if you need it  
				BL 			RNG
				MOV			R6, R11				;Get random number from R11 and move into R6
				MOV			R8, #0xFFFF
				MOVT		R8, #0x0000			
				AND			R6, R6, R8			;Only want the low-order 16 bits
				MOV			R7, #0x0148			;Scaling factor of 328 in hex
				SDIV		R6, R6, R7
				ADD			R6, R6, #0x32		;Offset of 50 in hex
				
				;Clear the cause of ISR on falling edge by using IO2IntClr
				LDR			R4, =IO2INTCLR
				MOV			R3, #0X0
				MOV			R3, #0X0400 ;Write 1 into bit 10 of IO2INTCLR
				STR			R3,	[R4]
				
				LDMFD		R13!,{R4,R7,R15}				; Use this command if you used STMFD (otherwise use BX LR) 


;*-------------------------------------------------------------------
; List of useful registers with their respective memory addresses.
;*------------------------------------------------------------------- 
LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
PINSEL3			EQU 	0x4002C00C 		; Pin Select Register 3 for P1[31:16]
PINSEL4			EQU 	0x4002C010 		; Pin Select Register 4 for P2[15:0], 
FIO1DIR			EQU		0x2009C020 		; Fast Input Output Direction Register for Port 1 
FIO2DIR			EQU		0x2009C040 		; Fast Input Output Direction Register for Port 2 
FIO1SET			EQU		0x2009C038 		; Fast Input Output Set Register for Port 1 
FIO2SET			EQU		0x2009C058 		; Fast Input Output Set Register for Port 2 
FIO1CLR			EQU		0x2009C03C 		; Fast Input Output Clear Register for Port 1 
FIO2CLR			EQU		0x2009C05C 		; Fast Input Output Clear Register for Port 2 
IO2IntEnf		EQU		0x400280B4		; GPIO Interrupt Enable for port 2 Falling Edge, INT0 is 10 
ISER0			EQU		0xE000E100		; Interrupt Set-Enable Register 0, ISE_EINT3 (external interrupt3 ) is bit 21, writing 1 enables interrupt
IO2INTCLR		EQU		0x400280AC		; Interrupt Port 2 Clear Register

				ALIGN 

				END 
