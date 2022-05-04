; *************************************************************************
;
;       LoCal low-calorie interpreter for the Z80 
;
;       Contains code written by Ken Boak, John Hardy and Craig Jones. 
;
;       GNU GENERAL PUBLIC LICENSE                   Version 3, 29 June 2007
;
;       see the LICENSE file in this repo for more information 
;
; *****************************************************************************

        DSIZE       EQU $80
        RSIZE       EQU $80
        LSIZE       EQU $80
        TIBSIZE     EQU $100		; 256 bytes , along line!
        TRUE        EQU 1		    ; not FF, for LoCal
        FALSE       EQU 0
        EMPTY       EQU 0		    ; for an empty macro, ctrl-<something>=macro, ie ctrl-h = backspace macros (in LoCal)

        DTASize EQU 26*2*2	; A..Z, a..z words

.macro LITDAT,len
        DB len
.endm

.macro REPDAT,len,data			; compress the command tables
        
        DB (len | $80)
        DB data
.endm

.macro ENDDAT
        DB 0
.endm

; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		

		.ORG ROMSTART + $180		; 0+180 put LoCal code from here	

; **************************************************************************
; Macros must be written in LoCal and end with ; 
; this code must not span pages
; **************************************************************************
macros:

; ***********************************************************************
; Initial values for user VARSS		
; ***********************************************************************		
iAltVars:			; value copied into tables
        DW dStack               ; a vS0 start of datastack			
        DW FALSE                ; b vBase16 
        DW 0                    ; c vTIBPtr an offset to the tib
        DW 0                    ; d 
        DW 65                   ; e vLastDef "A" last command u defined
        DW 0                    ; f 
        DW 0                    ; g 
        DW HEAP                 ; h vHeapPtr \h start of the free mem

iOpcodes:
        LITDAT 4		; macros for compression
        DB    lsb(exit_)        ;   NUL get least signif byte of address exit_
        DB    lsb(nop_)         ;   SOH 
        DB    lsb(nop_)         ;   STX 
        DB    lsb(etx_)         ;   ETX 

        REPDAT 29, lsb(nop_)

        LITDAT 15
        DB    lsb(nop_)         ;    !            
        DB    lsb(nop_)         ;    "
        DB    lsb(nop_)         ;    #
        DB    lsb(nop_)         ;    $            
        DB    lsb(nop_)         ;    %            
        DB    lsb(nop_)         ;    &
        DB    lsb(nop_)         ;    '
        DB    lsb(nop_)         ;    (        
        DB    lsb(nop_)         ;    )
        DB    lsb(nop_)         ;    *            
        DB    lsb(nop_)         ;    +
        DB    lsb(nop_)         ;    ,            
        DB    lsb(nop_)         ;    -
        DB    lsb(dot_)         ;    .
        DB    lsb(nop_)         ;    /	;/MOD

        REPDAT 10, lsb(num_)	;   1 2 3 ... 9 

        LITDAT 7
        DB    lsb(nop_)         ;    :        
        DB    lsb(nop_)         ;    ;
        DB    lsb(nop_)         ;    <
        DB    lsb(nop_)         ;    =            
        DB    lsb(nop_)         ;    >            
        DB    lsb(nop_)         ;    ?   ( -- val )  read a char from input
        DB    lsb(nop_)         ;    @    

        REPDAT 26, lsb(nop_)	; call a command A, B ....Z

        LITDAT 6
        DB    lsb(nop_)         ;    [
        DB    lsb(nop_)         ;    \
        DB    lsb(nop_)         ;    ]
        DB    lsb(nop_)         ;    ^
        DB    lsb(nop_)         ;    _
        DB    lsb(nop_)         ;    `    	; for printing `hello`        

        REPDAT 26, lsb(nop_)	; a b c .....z

        LITDAT 5
        DB    lsb(nop_)         ;    {
        DB    lsb(nop_)         ;    |            
        DB    lsb(nop_)         ;    }            
        DB    lsb(nop_)         ;    ~ ( a b c -- b c a ) rotate            
        DB    lsb(nop_)         ;    DEL	; eg 10000()

        LITDAT 17
        DB     lsb(EMPTY)       ; NUL ^@        
        DB     lsb(EMPTY)       ; SOH ^A  1
        DB     lsb(EMPTY)       ; STX ^B  2
        DB     lsb(EMPTY)       ; ETX ^C  3
        DB     lsb(EMPTY)       ; EOT ^D  4
        DB     lsb(EMPTY)       ; ENQ ^E  5
        DB     lsb(EMPTY)       ; ACK ^F  6
        DB     lsb(EMPTY)       ; BEL ^G  7 
        DB     lsb(EMPTY)       ; BS  ^H  8
        DB     lsb(EMPTY)       ; TAB ^I  9
        DB     lsb(EMPTY)       ; LF  ^J 10
        DB     lsb(EMPTY)       ; VT  ^K 11
        DB     lsb(EMPTY)       ; FF  ^L 12
        DB     lsb(EMPTY)       ; CR  ^M 13
        DB     lsb(EMPTY)       ; SO  ^N 14
        DB     lsb(EMPTY)       ; SI  ^O 15
        DB     lsb(EMPTY)       ; DLE ^P 16
        REPDAT 15, lsb(EMPTY)

        ENDDAT 

etx:                                ;=12
        LD HL,-DSTACK
        ADD HL,SP
        JR NC,etx1
        LD SP,DSTACK
etx1:
        JR interpret

start:
        LD SP,DSTACK		    ; start of LoCal
        CALL init		        ; setups
        CALL printStr		    ; prog count to stack, put code line 235 on stack then call print
        .cstr "LoCal V0.1\r\n"

interpret:
        call prompt

        LD BC,0                 ; load BC with offset into TIB, decide char into tib or execute or control         
        LD (vTIBPtr),BC

interpret2:                     ; calc nesting (a macro might have changed it)
        LD E,0                  ; initilize nesting value
        PUSH BC                 ; save offset into TIB, 
                                ; BC is also the count of chars in TIB
        LD HL,TIB               ; HL is start of TIB
        JR interpret4

interpret3:
        LD A,(HL)               ; A = char in TIB
        INC HL                  ; inc pointer into TIB
        DEC BC                  ; dec count of chars in TIB
        call nesting            ; update nesting value

interpret4:
        LD A,C                  ; is count zero?
        OR B
        JR NZ, interpret3       ; if not loop
        POP BC                  ; restore offset into TIB
; *******************************************************************         
; Wait for a character from the serial input (keyboard) 
; and store it in the text buffer. Keep accepting characters,
; increasing the instruction pointer BC - until a newline received.
; *******************************************************************

waitchar:   
        CALL getchar            ; loop around waiting for character from serial port
        CP $20			        ; compare to space
        JR NC,waitchar1		    ; if >= space, if below 20 set cary flag
        CP $0                   ; is it end of string? null end of string
        JR Z,waitchar4
        CP '\r'                 ; carriage return? ascii 13
        JR Z,waitchar3		    ; if anything else its macro/control 
        ; LD D,0
macro:                          ;
        LD (vTIBPtr),BC
        LD HL,ctrlCodes
        ADD A,L			        ;look up key of macros
        LD L,A
        LD E,(HL)
        LD A,E
        OR A
        JR Z,macro1
        LD D,msb(macros)
        PUSH DE
        call ENTER		        ;LoCal go operation and jump to it
        .cstr "\\^"
macro1:
        LD BC,(vTIBPtr)
        JR interpret2

waitchar1:
        LD HL,TIB
        ADD HL,BC
        LD (HL),A               ; store the character in textbuf
        INC BC
        CALL putchar            ; echo character to screen
        CALL nesting
        JR  waitchar            ; wait for next character

waitchar3:
        LD HL,TIB
        ADD HL,BC
        LD (HL),"\r"            ; store the crlf in textbuf
        INC HL
        LD (HL),"\n"            
        INC HL                  ; ????
        INC BC
        INC BC
        CALL crlf               ; echo character to screen
        LD A,E                  ; if zero nesting append and ETX after \r
        OR A
        JR NZ,waitchar
        LD (HL),$03             ; store end of text ETX in text buffer 
        INC BC

waitchar4:    
        LD (vTIBPtr),BC
        LD BC,TIB               ; Instructions stored on heap at address HERE, we pressed enter
        DEC BC

; ********************************************************************************
;
; Dispatch Routine.
;
; Get the next character and form a 1 byte jump address
;
; This target jump address is loaded into HL, and using JP (HL) to quickly 
; jump to the selected function.
;
; Individual handler routines will deal with each category:
;
; 1. Detect characters A-Z and jump to the User Command handler routine
;
; 2. Detect characters a-z and jump to the variable handler routine
;
; 3. All other characters are punctuation and cause a jump to the associated
; primitive code.
;
; Instruction Pointer IP BC is incremented
;
; *********************************************************************************

NEXT:                               ;=9 
        INC BC                      ;       Increment the IP
        LD A, (BC)                  ;       Get the next character and dispatch
        LD L,A                      ;       Index into table
        LD H,msb(opcodes)           ;       Start address of jump table         
        LD L,(HL)                   ;       get low jump address
        LD H,msb(page4)             ;       Load H with the 1st page address
        JP (HL)                     ;       Jump to routine

; ARRAY compilation routine
compNEXT:                       ;=20
        POP DE          	; DE = return address
        LD HL,(vHeapPtr)  	; load heap ptr
        LD (HL),E       	; store lsb
        LD A,(vByteMode)
        INC HL          
        OR A
        JR NZ,compNext1
        LD (HL),D
        INC HL
compNext1:
        LD (vHeapPtr),HL    ; save heap ptr
        JR NEXT

init:                           ;=68
        LD HL,LSTACK
        LD (vLoopSP),HL         ; Loop stack pointer stored in memory
        LD IX,RSTACK
        LD IY,NEXT		; IY provides a faster jump to NEXT
        LD HL,ialtVars
        LD DE,altVars
        LD BC,8 * 2
        LDIR
        
        LD HL,DTA          ; init namespaces to 0 using LDIR
        LD DE,HL
        INC DE
        LD (HL),0
        LD BC,DTASize
        LDIR

initOps:
        LD HL, iOpcodes
        LD DE, opcodes
        LD BC, 256

initOps1:
        LD A,(HL)
        INC HL
        SLA A                     
        RET Z
        JR C, initOps2
        SRL A
        LD C,A
        LD B,0
        LDIR
        JR initOps1
        
initOps2:        
        SRL A
        LD B,A
        LD A,(HL)
        INC HL
initOps2a:
        LD (DE),A
        INC DE
        DJNZ initOps2a
        JR initOps1

enter:                              ;=9
        LD HL,BC
        CALL rpush                  ; save Instruction Pointer
        POP BC
        DEC BC
        JP (IY)                    

crlf:                               ;=7
        call printStr
        .cstr "\r\n"
        RET

printStr:                       ;=14
        EX (SP),HL		; swap			
        CALL putStr		
        INC HL			; inc past null
        EX (SP),HL		; put it back	
        RET

lookupRef:
        LD D,0
lookupRef0:
        CP "a"
        JR NC,lookupRef2
lookupRef1:
        SUB "A"
        LD E,0
        JR lookupRef3        
lookupRef2:
        SUB "a"
        LD E,26*2
lookupRef3:
        ADD A,A
        ADD A,E
        LD HL,DTA
        ADD A,L
        LD L,A
        LD A,0
        ADC A,H
        LD H,A
        XOR A
        OR E                        ; sets Z flag if A-Z
        RET

printdec:                           ;=36
        LD DE,-10000			; LoCal ., 5th location of a dev number
        CALL printdec1			; text book method look it up
        LD DE,-1000
        CALL printdec1
        LD DE,-100
        CALL printdec1
        LD E,-10
        CALL printdec1
        LD E,-1
printdec1:	    
        LD A,'0'-1
printdec2:	    
        INC A
        ADD HL,DE
        JR C,printdec2
        SBC HL,DE
        JP putchar

printhex:                           ;=31  
                                    ; Display HL as a 16-bit number in hex.
        PUSH BC                     ; preserve the IP
        LD A,H
        CALL printhex2
        LD A,L
        CALL printhex2
        POP BC
        RET
printhex2:		                    
        LD	C,A
		RRA 
		RRA 
		RRA 
		RRA 
	    CALL printhex3
	    LD A,C
printhex3:		
        AND	0x0F
		ADD	A,0x90
		DAA
		ADC	A,0x40
		DAA
		JP putchar

; **************************************************************************             
; calculate nesting value
; A is char to be tested, 
; E is the nesting value (initially 0)
; E is increased by ( and [ 
; E is decreased by ) and ]
; E has its bit 7 toggled by `
; limited to 127 levels
; **************************************************************************             

nesting:                        ;=44
        CP '`'
        JR NZ,nesting1
        BIT 7,E
        JR Z,nesting1a
        RES 7,E
        RET
nesting1a: 
        SET 7,E
        RET
nesting1:
        BIT 7,E             
        RET NZ             
        CP ':'
        JR Z,nesting2
        CP '['
        JR Z,nesting2
        CP '('
        JR NZ,nesting3
nesting2:
        INC E
        RET
nesting3:
        CP ';'
        JR Z,nesting4
        CP ']'
        JR Z,nesting4
        CP ')'
        RET NZ
nesting4:
        DEC E
        RET 
        
; **********************************************************************			 
; Page 4 primitive routines 
; **********************************************************************
        .align $100
page4:

nop_:       
        JP NEXT             ; hardwire white space to always go to NEXT (important for arrays)

num_:   JP num

etx_:
        JP ETX
        
exit_:
        INC BC			; store offests into a table of bytes, smaller
        LD DE,BC                
        CALL rpop               ; Restore Instruction pointer
        LD BC,HL
        EX DE,HL
        JP (HL)

dot_:       
        POP HL
        CALL printdec
dot2:
        LD A,' '           
        CALL putChar
        JP (IY)


; ********************************************************************************
; Number Handling Routine - converts numeric ascii string to a 16-bit number in HL
; Read the first character. 
;			
; Number characters ($30 to $39) are converted to digits by subtracting $30
; and then added into the L register. (HL forms a 16-bit accumulator)
; Fetch the next character, if it is a number, multiply contents of HL by 10
; and then add in the next digit. Repeat this until a non-number character is 
; detected. Add in the final digit so that HL contains the converted number.
; Push HL onto the stack and proceed to the dispatch routine.
; ********************************************************************************
         
num:                                ;=23
		LD HL,$0000				    ;     Clear HL to accept the number
		LD A,(BC)				    ;     Get the character which is a numeral
        
num1:                               ; corrected KB 24/11/21

        SUB $30                     ;       Form decimal digit
        ADD A,L                     ;       Add into bottom of HL
        LD  L,A                     ;   
        LD A,00                     ;       Clear A
        ADC	A,H	                    ; Add with carry H-reg
	    LD	H,A	                    ; Put result in H-reg
      
        INC BC                      ;       Increment IP
        LD A, (BC)                  ;       and get the next character
        CP $30                      ;       Less than $30
        JR C, num2                  ;       Not a number / end of number
        CP $3A                      ;       Greater or equal to $3A
        JR NC, num2                 ;       Not a number / end of number
                                    ; Multiply digit(s) in HL by 10
        ADD HL,HL                   ;        2X
        LD  E,L                     ;        LD DE,HL
        LD  D,H                     ;    
        ADD HL,HL                   ;        4X
        ADD HL,HL                   ;        8X
        ADD HL,DE                   ;        2X  + 8X  = 10X
                                    ; 52t cycles

        JR  num1
                
num2:
        DEC BC
        PUSH HL                     ; Put the number on the stack
        JP (IY)                     ; and process the next character

hex:                                ;=26
	LD HL,0	    		    ; Clear HL to accept the number
hex1:
        INC BC
        LD A,(BC)		    ; Get the character which is a numeral
        BIT 6,A                     ; is it uppercase alpha?
        JR Z, hex2                  ; no a decimal
        SUB 7                       ; sub 7  to make $A - $F
hex2:
        SUB $30                     ; Form decimal digit
        JP C,num2
        CP $0F+1
        JP NC,num2
        ADD HL,HL                   ; 2X ; Multiply digit(s) in HL by 16
        ADD HL,HL                   ; 4X
        ADD HL,HL                   ; 8X
        ADD HL,HL                   ; 16X     
        ADD A,L                     ; Add into bottom of HL
        LD  L,A                     ;   
        JR  hex1

;*******************************************************************
; Subroutines
;*******************************************************************

prompt:                             ;=9
        call printStr
        .cstr "\r\n> "
        RET

putStr0:
        CALL putchar
        INC HL
putStr:
        LD A,(HL)
        OR A
        JR NZ,putStr0
        RET

rpush:                              ;=11
        DEC IX                  
        LD (IX+0),H
        DEC IX
        LD (IX+0),L
        RET

rpop:                               ;=11
        LD L,(IX+0)         
        INC IX              
        LD H,(IX+0)
        INC IX                  
rpop2:
        RET

