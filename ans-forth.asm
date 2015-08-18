;==============================================================================
;     _    _   _ ____    _____          _   _       _  ___  _  __
;    / \  | \ | / ___|  |  ___|__  _ __| |_| |__   ( )( _ )/ |/ /_
;   / _ \ |  \| \___ \  | |_ / _ \| '__| __| '_ \  |/ / _ \| | '_ \
;  / ___ \| |\  |___) | |  _| (_) | |  | |_| | | |   | (_) | | (_) |
; /_/   \_\_| \_|____/  |_|  \___/|_|   \__|_| |_|    \___/|_|\___/
;
; A Direct Threaded ANS Forth for the WDC 65C816
;------------------------------------------------------------------------------
; Copyright (C)2015 HandCoded Software Ltd.
; All rights reserved.
;
; This work is made available under the terms of the Creative Commons
; Attribution-NonCommercial-ShareAlike 4.0 International license. Open the
; following URL to see the details.
;
; http://creativecommons.org/licenses/by-nc-sa/4.0/
;
;==============================================================================
; Notes:
;
; This implementation is designed to run in the 65C816's native mode with both
; the accumulator and index registers in 16-bit mode except when the word needs
; 8-bit memory access.
;
; The Forth data stack is indexed using the X register witch values held at
; the same index offsets as they would be on the return stack. The Y register
; holds the forth instruction pointer.
;
;==============================================================================
;------------------------------------------------------------------------------

                pw      132
                inclist on

                chip    65816
                longi   off
                longa   off

                include "w65c816.inc"

;==============================================================================
; Macros
;------------------------------------------------------------------------------

HEADER          macro   LEN,NAME,TYPE,LAST
                dw      LAST
                db      LEN,NAME,TYPE
                endm

NORMAL          equ     0
IMMEDIATE       equ     1

DSTACK_SIZE     equ     128
DSTACK_INIT     equ     DSTACK+DSTACK_SIZE-1
RSTACK_INIT     equ     $01ff

;==============================================================================
; Data Areas
;------------------------------------------------------------------------------

                page0
                org     $00

WA              ds      2

DSTACK          ds      DSTACK_SIZE

                data
		org	$210

USER_AREA       ds      20

TIB_AREA        ds      128


;==============================================================================
;------------------------------------------------------------------------------

                code
                public  Start
Start:
                native                          ; Go to native mode
                long_ai                         ; And all 16-bit registers
                lda     #RSTACK_INIT
                tcs
                ldx     #DSTACK_INIT

                ldy     #COLD
                jmp     NEXT

COLD:
                dw      DO_S_QUOTE
                db      35,"HandCoded W65C816 ANS-Forth [15.08]"
                dw      TYPE
                dw      ABORT

;==============================================================================
; System/User Variables
;------------------------------------------------------------------------------

;
                HEADER  4,"#TIB",NORMAL,0
HASH_TIB:       jmp     DO_CONSTANT
                dw      $+2
                dw      80

                HEADER  3,">IN",NORMAL,0
TO_IN:


                HEADER  4,"BASE",NORMAL,0
BASE:           jmp     DO_USER
                dw      0

                HEADER  3,"BLK",NORMAL,0
BLK:

                HEADER  3,"SCR",NORMAL,0
SCR:

; SOURCE ( -- c-addr u )
;
; c-addr is the address of, and u is the number of characters in, the input
; buffer.
;
; In this implementation it is defined as
;
;   TIB #TIB @

                HEADER  6,"SOURCE",NORMAL,0
SOURCE:         jmp     DO_COLON
                dw      TIB
                dw      HASH_TIB,FETCH
                dw      EXIT

                HEADER  9,"SOURCE-ID",NORMAL,0
SOURCE_ID:

                HEADER  5,"STATE",NORMAL,0
STATE:          jmp     DO_USER
                dw      0

                HEADER  3,"TIB",NORMAL,0
TIB:

;==============================================================================
;------------------------------------------------------------------------------

QUERY_BRANCH:
                lda     DSTACK+1,x              ; Pull the top of stack value
                php                             ; Save the flags
                inx                             ; Drop top item
                inx
                plp
                beq     BRANCH                  ; Branch if top was zero
                iny                             ; Otherwise skip address
                iny
                jmp     NEXT                    ; Done

BRANCH:
                lda     0,y                     ; Load brancg address into IP
                tay
                jmp     NEXT                    ; Done

;==============================================================================
; Constants
;------------------------------------------------------------------------------

; 0 ( -- 0 )
;
; Push the constant value zero on the stack

                HEADER  1,"0",NORMAL,0
ZERO:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,X              ; And create a zero value
                jmp     NEXT                    ; Done

; BL ( -- char )
;
; char is the character value for a space.

                HEADER  2,"BL",NORMAL,ZERO
BL:
                dex                             ; Make space on the stack
                dex
                lda     #' '                    ; And save a space value
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; FALSE ( -- false )
;
; Return a false flag.

                HEADER  5,"FALSE",NORMAL,BL
FALSE:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,X              ; And create a false value
                jmp     NEXT                    ; Done

; TRUE ( -- true )
;
; Return a true flag, a single-cell value with all bits set.

                HEADER  4,"TRUE",NORMAL,FALSE
TRUE:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,x              ; And create a true value
                dec     DSTACK+1,x
                jmp     NEXT                    ; Done

;==============================================================================
; Memory Operations
;------------------------------------------------------------------------------

; ! ( x a-addr -- )
;
; Store x at a-addr.

                HEADER  1,"!",NORMAL,TRUE
STORE:
                phy
                ldy     DSTACK+1,x              ; Fetch the memory address
                lda     DSTACK+3,x              ; Fetch the data value
                sta     0,y                     ; Save it
                inx
                inx
                inx
                inx
                ply
                jmp     NEXT                    ; Done

; @ ( addr -- x )

                HEADER  1,"@",NORMAL,STORE
FETCH:
                phy
                ldy     DSTACK+1,x              ; Fetch the memory address
                lda     0,y                     ; Load the actual data
                sta     DSTACK+1,x              ; And replace the top value
                ply
                jmp     NEXT                    ; Done

; 2! ( x1 x2 a-addr -- )
;
; Store the cell pair x1 x2 at a-addr, with x2 at a-addr and x1 at the next
; consecutive cell. It is equivalent to the sequence SWAP OVER ! CELL+ !.

                HEADER  2,"2!",NORMAL,FETCH
TWO_STORE:
                jmp     DO_COLON
                dw      SWAP
                dw      OVER
                dw      STORE
                dw      CELL_PLUS
                dw      STORE
                dw      EXIT

; 2@ ( a-addr -- x1 x2 )
;
; Fetch the cell pair x1 x2 stored at a-addr. x2 is stored at a-addr and x1 at
; the next consecutive cell. It is equivalent to the sequence DUP CELL+ @ SWAP
; @.

                HEADER  2,"2@",NORMAL,TWO_STORE
TWO_FETCH:
                jmp     DO_COLON
                dw      DUP
                dw      CELL_PLUS
                dw      FETCH
                dw      SWAP
                dw      FETCH
                dw      EXIT

; C! ( x addr -- )

                HEADER  2,"C!",NORMAL,TWO_FETCH
C_STORE:
                phy
                ldy     DSTACK+1,x              ; Fetch the memory address
                lda     DSTACK+3,x              ; Fetch the data value
                short_a
                sta     0,y                     ; Save it
                long_a
                inx
                inx
                inx
                inx
                ply
                jmp     NEXT                    ; Done

; C@ ( addr -- x )

                HEADER  2,"C@",NORMAL,C_STORE
C_FETCH:
                phy
                ldy     DSTACK+1,x              ; Fetch the memory address
                short_a
                lda     0,y                     ; Load the actual data
                long_a
                and     #$00ff
                sta     DSTACK+1,x              ; And replace the top value
                ply
                jmp     NEXT                    ; Done

; CELL+

                HEADER  5,"CELL+",NORMAL,C_FETCH
CELL_PLUS:
                inc     DSTACK+1,x              ; Bump the address by two
                inc     DSTACK+1,X
                jmp     NEXT

;==============================================================================
; Stack Operations
;------------------------------------------------------------------------------

; 2DROP ( x1 x2 -- )
;
; Drop cell pair x1 x2 from the stack.

                HEADER  5,"2DROP",NORMAL,CELL_PLUS
TWO_DROP:
                inx
                inx
                inx
                inx
                jmp     NEXT

; 2DUP

; 2OVER

; ?DUP

		HEADER	4,"?DUP",NORMAL,0
QUERY_DUP:
		lda	DSTACK+1,x
		beq	QUERY_DUP_1
		dex
		dex
		sta	DSTACK+1,x
QUERY_DUP_1:	jmp	NEXT

; DROP ( x -- )
;
; Remove x from the stack.

                HEADER  4,"DROP",NORMAL,TWO_DROP
DROP:
                inx                             ; Drop the top value
                inx
                jmp     NEXT                    ; Done

; DUP ( x -- x x )
;
; Duplicate x.

                HEADER  3,"DUP",NORMAL,DROP
DUP:
                lda     DSTACK+1,x              ; Fetch top value
                dex                             ; And make a copy
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; OVER ( x1 x2 -- x1 x2 x1 )

                HEADER  4,"OVER",NORMAL,DUP
OVER:
                lda     DSTACK+3,x              ; Fetch second value
                dex                             ; And make a copy
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; SWAP ( x1 x2 -- x2 x1 )

                HEADER  4,"SWAP",NORMAL,OVER
SWAP:
                lda     DSTACK+1,x		; Fetch top of stack
                pha				; .. and save
                lda     DSTACK+3,x		; Exchange second
                sta     DSTACK+1,x		; .. and top
                pla				; Recover top
                sta     DSTACK+3,x		; .. and save as second
                jmp     NEXT			; Done

;==============================================================================
; Return Stack Operations
;------------------------------------------------------------------------------

                HEADER  2,">R",NORMAL,SWAP
TO_R:
                lda     DSTACK+1,x              ; Transfer top value
                pha                             ; .. to return stack
                inx
                inx
                jmp     NEXT                    ; Done

                HEADER  1,"I",NORMAL,0
I:
                lda     1,s
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT

                HEADER  1,"J",NORMAL,0
J:
                lda     3,s
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT


                HEADER  2,"R>",NORMAL,TO_R
R_FROM:
                pla                             ; Fetch return stack value
                dex                             ; And push
                dex
                sta     DSTACK+1,X
                jmp     NEXT                    ; Done

;==============================================================================
; Single Precision Arithmetic
;------------------------------------------------------------------------------

; + ( n1|u1 n2|u2 -- n3|u3 )
;
; Add n2|u2 to n1|u1, giving the sum n3|u3.

                HEADER  1,"+",NORMAL,R_FROM
PLUS:
                clc
                lda     DSTACK+1,x
                adc     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                JMP     NEXT                    ; Done

; - ( n1|u1 n2|u2 -- n3|u3 )
;
; Subtract n2|u2 from n1|u1, giving the difference n3|u3.

                HEADER  1,"-",NORMAL,PLUS
MINUS:
                sec
                lda     DSTACK+1,x
                sbc     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                jmp     NEXT

; 1+ ( n1|u1 -- n2|u2 )
;
; Add one (1) to n1|u1 giving the sum n2|u2.

                HEADER  2,"1+",NORMAL,MINUS
ONE_PLUS:
                inc     DSTACK+1,x
                jmp     NEXT                    ; Done


                HEADER  2,"1-",NORMAL,ONE_PLUS
ONE_MINUS:
                dec     DSTACK+1,x
                jmp     NEXT

; NEGATE ( n1 -- n2 )
;
; Negate n1, giving its arithmetic inverse n2.

                HEADER  6,"NEGATE",NORMAL,ONE_PLUS
NEGATE:
                sec
                lda     #0
                sbc     DSTACK+1,x
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

;==============================================================================
; Mixed Arithmetic
;------------------------------------------------------------------------------

;==============================================================================
; Comparisons
;------------------------------------------------------------------------------

; 0< ( n -- flag )
;
; flag is true if and only if n is less than zero.

                HEADER  2,"0<",NORMAL,0
ZERO_LESS:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bpl     $+5                     ; Was the value negative?
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT

; 0= ( x -- flag )
;
; flag is true if and only if x is equal to zero.

                HEADER  2,"0=",NORMAL,ZERO_LESS
ZERO_EQUAL:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bne     $+5                     ; Was the value zero?
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT

; 0> ( n -- flag )
;
; flag is true if and only if n is greater than zero.

                HEADER  2,"0>",NORMAL,ZERO_EQUAL
ZERO_GREATER:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bmi     $+7                     ; Was the value positive?
                beq     $+5                     ; .. but not zero
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT

;==============================================================================
; Logical Operations
;------------------------------------------------------------------------------

; AND ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit logical “and” of x1 with x2.

                HEADER  3,"AND",NORMAL,NEGATE
AND:
                lda     DSTACK+1,x
                and     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                jmp     NEXT

; INVERT ( x -- !x )

                HEADER  6,"INVERT",NORMAL,AND
INVERT:
                lda     DSTACK+1,x              ; Fetch top value
                eor     #$ffff                  ; Invert all the bits
                sta     DSTACK+1,x              ; And write back
                jmp     NEXT                    ; Done

; AND ( x1 x2 -- x3 )

                HEADER  2,"OR",NORMAL,INVERT
OR:
                lda     DSTACK+1,x
                ora     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                jmp     NEXT

; XOR ( x1 x2 -- x3 )

                HEADER  3,"XOR",NORMAL,OR
XOR:
                lda     DSTACK+1,x
                eor     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                jmp     NEXT

;==============================================================================
; Control Words
;------------------------------------------------------------------------------

; ABORT ( i*x -- ) ( R: j*x -- )
;
; Empty the data stack and perform the function of QUIT, which includes
; emptying the return stack, without displaying a message.

                HEADER  5,"ABORT",NORMAL,COLON
ABORT:          jmp     DO_COLON
                dw      DO_ABORT
                dw      QUIT

DO_ABORT:
                ldx     #DSTACK_INIT
                jmp     NEXT

; EXIT ( -- ) ( R: nest-sys -- )

                HEADER  4,"EXIT",NORMAL,ABORT
EXIT:
                ply
                jmp     NEXT

; QUIT ( -- ) ( R: i*x -- )
;
; Empty the return stack, store zero in SOURCE-ID if it is present, make the
; user input device the input source, and enter interpretation state. Do not
; display a message. Repeat the following:
; – Accept a line from the input source into the input buffer, set >IN to zero,
;   and interpret.
; – Display the implementation-defined system prompt if in interpretation state,
;   all processing has been completed, and no ambiguous condition exists.
;
; In this implementation it is defined as:
;
;   DO_QUIT 0 STATE !
;   BEGIN
;       REFILL
;       WHILE SOURCE EVALUATE
;       STATE @ 0= IF CR S" OK" THEN
;   AGAIN ;

                HEADER  4,"QUIT",NORMAL,EXIT
QUIT:           jmp     DO_COLON
                dw      DO_QUIT
                dw      ZERO,STATE,STORE


DO_QUIT:
                lda     #RSTACK_INIT            ; Reset the return stack
                tcs
                jmp     NEXT                    ; Done

;==============================================================================
; Compiling Words
;------------------------------------------------------------------------------

                HEADER  10,"(CONSTANT)",NORMAL,0
DO_CONSTANT:

                HEADER  4,"(DO)",NORMAL,0
DO_DO:
                lda     DSTACK+3,x
                pha
                lda     DSTACK+1,x
                pha
                inx
                inx
                inx
                inx
                jmp     NEXT

                HEADER  10,"(LITERAL)",NORMAL,0
DO_LITERAL:

                HEADER  6,"(LOOP)",NORMAL,0
DO_LOOP
                lda     1,s                     ; Add one to loop counter
                inc	a
		sta	1,s
                cmp     3,s
		
		jmp	NEXT

                HEADER  10,"(USER)",NORMAL,0
DO_USER:

                HEADER  10,"(VARIABLE)",NORMAL,0
DO_VARIABLE:


; : ()

                HEADER  1,":",IMMEDIATE,0
COLON:


DO_COLON:
                phy                             ; Save the instruction pointer
                tay                             ; Calculate the new IP
                iny
                iny
                iny
NEXT:
                lda     0,y                     ; Fetch the next word address
                sta     WA
                iny                             ; Bump the instruction pointer
                iny
                jmp     (!WA)                   ; And execute word


                HEADER  8,"CONSTANT",IMMEDIATE,0
CONSTANT:



                HEADER  2,"IF",IMMEDIATE,0
IF:



                HEADER  2,"S""",IMMEDIATE,0
S_QUOTE:

; (S") ( -- c-addr u )

DO_S_QUOTE:
                dex                             ; Reserve space for values
                dex
                dex
                dex
                short_a
                lda     0,y                     ; Fetch the length
                long_a
                and     #$00ff
                sta     DSTACK+1,x
                iny                             ; Save the text address
                sty     DSTACK+3,x
                clc                             ; And update IP
                adc     DSTACK+3,X
                tay
                jmp     NEXT                    ; Done

;==============================================================================
; I/O Operations
;------------------------------------------------------------------------------

; CR ( -- )
;
; Cause subsequent output to appear at the beginning of the next line.
;
; In this implementation it is defined as
;
;   13 EMIT 10 EMIT

                HEADER  2,"CR",NORMAL,0
CR:             jmp     DO_COLON
                dw      DO_LITERAL,13,EMIT
                dw      DO_LITERAL,10,EMIT
                dw      EXIT

; EMIT ( x -- )
;
; If x is a graphic character in the implementation-defined character set,
; display x. The effect of EMIT for all other values of x is implementation
; -defined.

                HEADER  4,"EMIT",NORMAL,CR
                extern  UartTx
EMIT:
                lda     DSTACK+1,X              ; Fetch character from stack
                jsr     UartTx                  ; .. and transmit
                inx                             ; Drop the character
                inx
                jmp     NEXT                    ; Done

; KEY ( -- char )
;
; Receive one character char, a member of the implementation-defined character
; set. Keyboard events that do not correspond to such characters are discarded
; until a valid character is received, and those events are subsequently
; unavailable.
;
; All standard characters can be received. Characters received by KEY are not
; displayed.

                HEADER  3,"KEY",NORMAL,EMIT
                extern  UartRx
KEY:
                jsr     UartRx                  ; Receive a character
                and     #$00ff                  ; Ensure in ASCII range
                dex                             ; And push to stack
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; SPACE ( -- )
;
; Display one space.
;
; In this implementation it is defined as
;
;   SPACE EMIT

                HEADER  5,"SPACE",NORMAL,KEY
SPACE:          jmp     DO_COLON
                dw      BL
                dw      EMIT
                dw      EXIT

; SPACES ( n -- )
;
; If n is greater than zero, display n spaces.
;
; In this implementation it is defined as
;
;   BEGIN DUP 0> WHILE SPACE 1- REPEAT DROP

                HEADER  6,"SPACES",NORMAL,SPACE
SPACES:         jmp     DO_COLON
SPACES_1:       dw      DUP,ZERO_GREATER,QUERY_BRANCH,SPACES_2
                dw      SPACE,ONE_MINUS,BRANCH,SPACES_1
SPACES_2:       dw      DROP,EXIT

; TYPE ( c-addr u -- )
;
; If u is greater than zero, display the character string specified by c-addr
; and u.
;
; In this implementation it is defined as
;
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN

                HEADER  4,"TYPE",NORMAL,SPACES
TYPE:           jmp     DO_COLON
                dw      QUERY_DUP,QUERY_BRANCH,TYPE_2
                dw      OVER,PLUS,SWAP,DO_DO
TYPE_1:         dw      I,C_FETCH,EMIT,DO_LOOP,TYPE_1
                dw      BRANCH,TYPE_3
TYPE_2          dw      DROP
TYPE_3          dw      EXIT

;================================================================================
;--------------------------------------------------------------------------------

                end