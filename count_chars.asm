; Program to count occurences of a character in a file
; Character to be input from the keyboard
; Result to be displayed on the monitor
; Program works only if no more than 9 occurences are found
;
;
; Initialization
;
	AND R2,R2,0 	; R2 is counter, initialize to 0
	LD R3,PTR		; R3 is pointer to characters
	TRAP 0x23		; R0 gets character input
	LDR R1,R3,0		; R1 gets the next character
;
; Test character for end of file
;
TEST:
	ADD R4,R1,-4 	; Test for EOT
	BRz OUTPUT		; If done, prepare the output
;
; Test character for match. If a match. Increment count.
;
	NOT R1,R1		
	ADD R1,R1,1		; R1 <-- -R1
	ADD R1,R1,R0	; R1 <-- R0 - R1. If R1=0, a match!
	BRnp GETCHAR	; If no match, do not increment
	ADD R1,R2,1
;
; Get next character from file
;
GETCHAR:
	ADD R3,R3,1		; Increment the pointer
	LDR R1,R3,0 	; R1 gets the next character to test
	BRnzp TEST
;
; Output the count.
;
OUTPUT:
	LD R0,ASCII		; Load the ASCII template
	ADD R0,R0,R1	; Convert binary to ASCII
	TRAP 0x21		; ASCII code in R0 is displayed
	TRAP 0x25		; Halt machine
;
; Storage for pointer and ASCII template
;
ASCII:
	#d 0x0030
PTR:
	#d 0x4000
