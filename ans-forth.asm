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

COUNT           set     0
WORD0           equ     0

HEADER          macro   LEN,NAME,TYPE
WORD@<COUNT+1>:
                dw      WORD@<COUNT>
                db      TYPE,LEN,NAME
COUNT           set     COUNT+1
                endm

NORMAL          equ     $00
IMMEDIATE       equ     $80

;==============================================================================
; Definitions
;------------------------------------------------------------------------------

DSTACK_SIZE     equ     128
DSTACK_INIT     equ     DSTACK+DSTACK_SIZE-1
RSTACK_INIT     equ     $01ff

USER_SIZE       equ     20

TO_IN_OFFSET    equ     0
BASE_OFFSET     equ     2
BLK_OFFSET      equ     4
DP_OFFSET       equ     6
LATEST_OFFSET   equ     8
SCR_OFFSET      equ     10
SOURCEID_OFFSET equ     12
STATE_OFFSET    equ     14

TIB_SIZE        equ     128

;==============================================================================
; Data Areas
;------------------------------------------------------------------------------

                page0
                org     $00

WA              ds      2                       ; Word address

DSTACK          ds      DSTACK_SIZE             ; The data stack (indexed by X)

                data
                org     $210

USER_AREA       ds      USER_SIZE               ; User Variables

TIB_AREA        ds      TIB_SIZE                ; Terminal Input Buffer

;==============================================================================
; Forth Entry Point
;------------------------------------------------------------------------------

                code
                public  Start
Start:
                native                          ; Go to native mode
                long_ai                         ; And all 16-bit registers
                lda     #RSTACK_INIT            ; Initialise return stack
                tcs
                ldx     #DSTACK_INIT            ; .. and data stack

                ldy     #COLD                   ; Then perform COLD start
                jmp     NEXT

COLD:
                dw      DECIMAL
                dw      ZERO,BLK,STORE
                dw      FALSE,STATE,STORE
                dw      DO_S_QUOTE
                db      35,"HandCoded W65C816 ANS-Forth [15.08]"
                dw      TYPE
                dw      ABORT

;==============================================================================
; System/User Variables
;------------------------------------------------------------------------------

; #TIB ( -- a-addr )
;
; a-addr is the address of a cell containing the number of characters in the
; terminal input buffer.

                HEADER  4,"#TIB",NORMAL
HASH_TIB:       jmp     DO_CONSTANT
                dw      $+2
                dw      TIB_SIZE-2

; >IN ( -- a-addr )
;
; a-addr is the address of a cell containing the offset in characters from the
; start of the input buffer to the start of the parse area.

                HEADER  3,">IN",NORMAL
TO_IN:          jmp     DO_USER
                dw      TO_IN_OFFSET

; BASE ( -- a-addr )
;
; a-addr is the address of a cell containing the current number-conversion
; radix {{2...36}}.

                HEADER  4,"BASE",NORMAL
BASE:           jmp     DO_USER
                dw      BASE_OFFSET

; BLK ( -- a-addr )
;
; a-addr is the address of a cell containing zero or the number of the mass-
; storage block being interpreted. If BLK contains zero, the input source is
; not a block and can be identified by SOURCE-ID, if SOURCE-ID is available. An
; ambiguous condition exists if a program directly alters the contents of BLK.

                HEADER  3,"BLK",NORMAL
BLK:            jmp     DO_USER
                dw      BLK_OFFSET

; DP ( -- a-addr )

                HEADER  2,"DP",NORMAL
DP:             jmp     DO_USER
                dw      DP_OFFSET

; SCR ( -- a-addr )
;
; a-addr is the address of a cell containing the block number of the block most
; recently LISTed.

                HEADER  3,"SCR",NORMAL
SCR:            jmp     DO_USER
                dw      SCR_OFFSET

; SOURCE-ID ( -- 0 | -1 )
;
; Identifies the input source: -1 if string (via EVALUATE), 0 if user input
; device.

                HEADER  9,"SOURCE-ID",NORMAL
SOURCE_ID:      jmp     DO_USER
                dw      SOURCEID_OFFSET

; STATE ( -- a-addr )
;
; a-addr is the address of a cell containing the compilation-state flag. STATE
; is true when in compilation state, false otherwise. The true value in STATE
; is non-zero, but is otherwise implementation-defined.

                HEADER  5,"STATE",NORMAL
STATE:          jmp     DO_USER
                dw      STATE_OFFSET

; TIB ( -- c-addr )
;
; c-addr is the address of the terminal input buffer.

                HEADER  3,"TIB",NORMAL
TIB:            jmp     DO_CONSTANT
                dw      TIB_AREA

;==============================================================================
; Branches
;------------------------------------------------------------------------------

; (?BRANCH) ( flag -- )
;
; If flag is false then cause the IP to be loaded with the word following the
; link to this word, otherwise skip over it.

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

; (BRANCH) ( --)
;
; Cause the IP to be loaded with the word following the link to this word.

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

                HEADER  1,"0",NORMAL
ZERO:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,X              ; And create a zero value
                jmp     NEXT                    ; Done

; BL ( -- char )
;
; char is the character value for a space.

                HEADER  2,"BL",NORMAL
BL:
                dex                             ; Make space on the stack
                dex
                lda     #' '                    ; And save a space value
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; FALSE ( -- false )
;
; Return a false flag.

                HEADER  5,"FALSE",NORMAL
FALSE:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,X              ; And create a false value
                jmp     NEXT                    ; Done

; TRUE ( -- true )
;
; Return a true flag, a single-cell value with all bits set.

                HEADER  4,"TRUE",NORMAL
TRUE:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,x              ; And create a true value
                dec     DSTACK+1,x
                jmp     NEXT                    ; Done

;==============================================================================
; Radix
;------------------------------------------------------------------------------

; DECIMAL ( -- )
;
; Set the numeric conversion radix to ten (decimal).

                HEADER  7,"DECIMAL",NORMAL
DECIMAL:        jmp     DO_COLON
                dw      DO_LITERAL,10,BASE,STORE
                dw      EXIT

; HEX ( -- )
;
; Set contents of BASE to sixteen.

                HEADER  3,"HEX",NORMAL
HEX:            jmp     DO_COLON
                dw      DO_LITERAL,16,BASE,STORE
                dw      EXIT

;==============================================================================
; Memory Operations
;------------------------------------------------------------------------------

; ! ( x a-addr -- )
;
; Store x at a-addr.

                HEADER  1,"!",NORMAL
STORE:
                phy
                ldy     DSTACK+1,x              ; Fetch the memory address
                lda     DSTACK+3,x              ; Fetch the data value
                sta     0,y                     ; Save it
                inx                             ; Clean up data stack
                inx
                inx
                inx
                ply
                jmp     NEXT                    ; Done

; +! ( n|u a-addr -- )
;
; Add n|u to the single-cell number at a-addr.

                HEADER  2,"+!",NORMAL
PLUS_STORE:
                phy
                ldy     DSTACK+1,x              ; Fetch the memory address
                lda     DSTACK+3,x              ; Fetch the data value
                clc                             ; Add data to memory
                adc     0,y
                sta     0,y                     ; And save result
                inx                             ; Clean up data stacl
                inx
                inx
                inx
                ply
                jmp     NEXT                    ; Done

; , ( x -- )
;
; Reserve one cell of data space and store x in the cell. If the data-space
; pointer is aligned when , begins execution, it will remain aligned when ,
; finishes execution. An ambiguous condition exists if the data-space pointer
; is not aligned prior to execution of ,.
;
;   HERE ! 1 CELLS ALLOT

                HEADER  1,$2c,NORMAL
COMMA:          jmp     DO_COLON
                dw      HERE,STORE
                dw      DO_LITERAL,1,CELLS,ALLOT
                dw      EXIT

; 2! ( x1 x2 a-addr -- )
;
; Store the cell pair x1 x2 at a-addr, with x2 at a-addr and x1 at the next
; consecutive cell. It is equivalent to the sequence SWAP OVER ! CELL+ !.

                HEADER  2,"2!",NORMAL
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

                HEADER  2,"2@",NORMAL
TWO_FETCH:
                jmp     DO_COLON
                dw      DUP
                dw      CELL_PLUS
                dw      FETCH
                dw      SWAP
                dw      FETCH
                dw      EXIT

; @ ( a-addr -- x )
;
; x is the value stored at a-addr.

                HEADER  1,"@",NORMAL
FETCH:
                phy
                ldy     DSTACK+1,x              ; Fetch the memory address
                lda     0,y                     ; Load the actual data
                sta     DSTACK+1,x              ; And replace the top value
                ply
                jmp     NEXT                    ; Done

; ALLOT ( n -- )
;
; If n is greater than zero, reserve n address units of data space. If n is
; less than zero, release |n| address units of data space. If n is zero, leave
; the data-space pointer unchanged.
;
; In this implementation its is defined as:
;
;   DP +!

                HEADER  5,"ALLOT",NORMAL
ALLOT:          jmp     DO_COLON
                dw      DP,PLUS_STORE
                dw      EXIT

; C! ( char c-addr -- )
;
; Store char at c-addr. When character size is smaller than cell size, only the
; number of low-order bits corresponding to character size are transferred.

                HEADER  2,"C!",NORMAL
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

; C, ( char -- )
;
; Reserve space for one character in the data space and store char in the
; space. If the data-space pointer is character aligned when C, begins
; execution, it will remain character aligned when C, finishes execution.
; An ambiguous condition exists if the data-space pointer is not character-
; aligned prior to execution of C,
;
;   HERE C! 1 CHARS ALLOT

;               HEADER  2,"C,",NORMAL
                dw      C_STORE
                db      2,"C,",NORMAL
C_COMMA:        jmp     DO_COLON
                dw      HERE,C_STORE
                dw      DO_LITERAL,1,CHARS,ALLOT
                dw      EXIT

; C@ ( c-addr -- char )
;
; Fetch the character stored at c-addr. When the cell size is greater than
; character size, the unused high-order bits are all zeroes.

                HEADER  2,"C@",NORMAL
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

; HERE

                HEADER  4,"HERE",NORMAL
HERE:           jmp     DO_COLON
                dw      DP,FETCH
                dw      EXIT

;==============================================================================
; Alignment
;------------------------------------------------------------------------------

; ALIGN ( -- )
;
; If the data-space pointer is not aligned, reserve enough space to align it.

                HEADER  5,"ALIGN",NORMAL
ALIGN:
                jmp     NEXT                    ; Done

; ALIGNED ( addr -- a-addr )
;
; a-addr is the first aligned address greater than or equal to addr.

                HEADER  7,"ALIGNED",NORMAL
ALIGNED:
                jmp     NEXT                    ; Done

; CELL+ ( a-addr1 -- a-addr2 )
;
; Add the size in address units of a cell to a-addr1, giving a-addr2.

                HEADER  5,"CELL+",NORMAL
CELL_PLUS:
                inc     DSTACK+1,x              ; Bump the address by two
                inc     DSTACK+1,X
                jmp     NEXT

; CELLS ( n1 -- n2 )
;
; n2 is the size in address units of n1 cells.

                HEADER  5,"CELLS",NORMAL
CELLS:
                asl     DSTACK+1,x              ; Two bytes per cell
                jmp     NEXT

; CHAR+ ( c-addr1 -- c-addr2 )
;
; Add the size in address units of a character to c-addr1, giving c-addr2.

                HEADER  5,"CHAR+",NORMAL
CHAR_PLUS:
                inc     DSTACK+1,x              ; Bump the address by one
                jmp     NEXT

; CHARS ( n1 -- n2 )
;
; n2 is the size in address units of n1 characters.

                HEADER  5,"CHARS",NORMAL
CHARS:
                jmp     NEXT

;==============================================================================
; Stack Operations
;------------------------------------------------------------------------------

; 2DROP ( x1 x2 -- )
;
; Drop cell pair x1 x2 from the stack.

                HEADER  5,"2DROP",NORMAL
TWO_DROP:
                inx
                inx
                inx
                inx
                jmp     NEXT

; 2DUP

; 2OVER

; ?DUP

                HEADER  4,"?DUP",NORMAL
QUERY_DUP:
                lda     DSTACK+1,x
                beq     QUERY_DUP_1
                dex
                dex
                sta     DSTACK+1,x
QUERY_DUP_1:    jmp     NEXT

; DROP ( x -- )
;
; Remove x from the stack.

                HEADER  4,"DROP",NORMAL
DROP:
                inx                             ; Drop the top value
                inx
                jmp     NEXT                    ; Done

; DUP ( x -- x x )
;
; Duplicate x.

                HEADER  3,"DUP",NORMAL
DUP:
                lda     DSTACK+1,x              ; Fetch top value
                dex                             ; And make a copy
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; OVER ( x1 x2 -- x1 x2 x1 )

                HEADER  4,"OVER",NORMAL
OVER:
                lda     DSTACK+3,x              ; Fetch second value
                dex                             ; And make a copy
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; SWAP ( x1 x2 -- x2 x1 )

                HEADER  4,"SWAP",NORMAL
SWAP:
                lda     DSTACK+1,x              ; Fetch top of stack
                pha                             ; .. and save
                lda     DSTACK+3,x              ; Exchange second
                sta     DSTACK+1,x              ; .. and top
                pla                             ; Recover top
                sta     DSTACK+3,x              ; .. and save as second
                jmp     NEXT                    ; Done

;==============================================================================
; Return Stack Operations
;------------------------------------------------------------------------------

                HEADER  2,">R",NORMAL
TO_R:
                lda     DSTACK+1,x              ; Transfer top value
                pha                             ; .. to return stack
                inx
                inx
                jmp     NEXT                    ; Done

                HEADER  1,"I",NORMAL
I:
                lda     1,s
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT

                HEADER  1,"J",NORMAL
J:
                lda     3,s
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT


                HEADER  2,"R>",NORMAL
R_FROM:
                pla                             ; Fetch return stack value
                dex                             ; And push
                dex
                sta     DSTACK+1,X
                jmp     NEXT                    ; Done

;==============================================================================
; Single Precision Arithmetic
;------------------------------------------------------------------------------

; * ( n1|u1 n2|u2 -- n3|u3 )
;
; Multiply n1|u1 by n2|u2 giving the product n3|u3.

                HEADER  1,"*",NORMAL
STAR:
                jmp     NEXT                    ; Done

; */
; */MOD


; + ( n1|u1 n2|u2 -- n3|u3 )
;
; Add n2|u2 to n1|u1, giving the sum n3|u3.

                HEADER  1,"+",NORMAL
PLUS:
                clc                             ; Add top two values
                lda     DSTACK+1,x
                adc     DSTACK+3,x
                sta     DSTACK+3,x              ; Save result
                inx                             ; Clean up data stack
                inx
                JMP     NEXT                    ; Done

; - ( n1|u1 n2|u2 -- n3|u3 )
;
; Subtract n2|u2 from n1|u1, giving the difference n3|u3.

                HEADER  1,"-",NORMAL
MINUS:
                sec                             ; Subtract top two values
                lda     DSTACK+1,x
                sbc     DSTACK+3,x
                sta     DSTACK+3,x              ; Save result
                inx                             ; Clean up data stack
                inx
                jmp     NEXT                    ; Done

; /
; /MOD

; 1+ ( n1|u1 -- n2|u2 )
;
; Add one (1) to n1|u1 giving the sum n2|u2.

                HEADER  2,"1+",NORMAL
ONE_PLUS:
                inc     DSTACK+1,x              ; Increment top of stack
                jmp     NEXT                    ; Done

; 1- ( n1|u1 -- n2|u2 )
;
; Subtract one (1) from n1|u1 giving the difference n2|u2.

                HEADER  2,"1-",NORMAL
ONE_MINUS:
                dec     DSTACK+1,x              ; Decrement top of stack
                jmp     NEXT                    ; Done

; 2* ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the most-significant bit,
; filling the vacated least-significant bit with zero.

                HEADER  2,"2*",NORMAL
TWO_STAR:
                asl     DSTACK+1,x              ; Multiply top value by two
                jmp     NEXT                    ; Done

; 2/ ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the least-significant bit,
; leaving the most-significant bit unchanged.

                HEADER  2,"2/",NORMAL
TWO_SLASH:
                lda     DSTACK+1,x              ; Load the top value
                rol     a                       ; Extract the top bit
                ror     DSTACK+1,x              ; And shift back into value
                jmp     NEXT



; NEGATE ( n1 -- n2 )
;
; Negate n1, giving its arithmetic inverse n2.

                HEADER  6,"NEGATE",NORMAL
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

                HEADER  2,"0<",NORMAL
ZERO_LESS:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bpl     $+5                     ; Was the value negative?
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT                    ; Done

; 0<> ( x -- flag )
;
; flag is true if and only if x is not equal to zero.

                HEADER  3,"0<>",NORMAL
ZERO_NOT_EQUAL:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                beq     $+5                     ; Was the value non-zero?
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT                    ; Done

; 0= ( x -- flag )
;
; flag is true if and only if x is equal to zero.

                HEADER  2,"0=",NORMAL
ZERO_EQUAL:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bne     $+5                     ; Was the value zero?
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT                    ; Done

; 0> ( n -- flag )
;
; flag is true if and only if n is greater than zero.

                HEADER  2,"0>",NORMAL
ZERO_GREATER:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bmi     $+7                     ; Was the value positive?
                beq     $+5                     ; .. but not zero
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT                    ; Done

;==============================================================================
; Logical Operations
;------------------------------------------------------------------------------

; AND ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit logical “and” of x1 with x2.

                HEADER  3,"AND",NORMAL
AND:
                lda     DSTACK+1,x
                and     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                jmp     NEXT

; INVERT ( x -- !x )

                HEADER  6,"INVERT",NORMAL
INVERT:
                lda     DSTACK+1,x              ; Fetch top value
                eor     #$ffff                  ; Invert all the bits
                sta     DSTACK+1,x              ; And write back
                jmp     NEXT                    ; Done

; AND ( x1 x2 -- x3 )

                HEADER  2,"OR",NORMAL
OR:
                lda     DSTACK+1,x
                ora     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                jmp     NEXT

; XOR ( x1 x2 -- x3 )

                HEADER  3,"XOR",NORMAL
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

                HEADER  5,"ABORT",NORMAL
ABORT:          jmp     DO_COLON
                dw      DO_ABORT
                dw      QUIT

DO_ABORT:
                ldx     #DSTACK_INIT
                jmp     NEXT

; EXIT ( -- ) ( R: nest-sys -- )

                HEADER  4,"EXIT",NORMAL
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

                HEADER  4,"QUIT",NORMAL
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

                HEADER  10,"(CONSTANT)",NORMAL
DO_CONSTANT:
                phy                             ; Save IP
                tay                             ; Fetch constant after WA
                lda     3,y
                dex                             ; And push
                dex
                sta     DSTACK+1,x
                ply
                jmp     NEXT                    ; Done

                HEADER  4,"(DO)",NORMAL
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

                HEADER  10,"(LITERAL)",NORMAL
DO_LITERAL:
                lda     0,y                     ; Fetch constant from IP
                iny
                iny
                dex                             ; And push
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

                HEADER  6,"(LOOP)",NORMAL
DO_LOOP
                lda     1,s                     ; Add one to loop counter
                inc     a
                sta     1,s
                cmp     3,s

                jmp     NEXT

                HEADER  6,"(USER)",NORMAL
DO_USER:
                phy                             ; Save the IP
                tay                             ; Fetch offset after WA
                lda     3,y
                clc                             ; Work out address in user area
                adc     #USER_AREA
                dex                             ; Push on data stack
                dex
                sta     DSTACK+1,x
                ply                             ; Restore IP
                jmp     NEXT                    ; Done

                HEADER  10,"(VARIABLE)",NORMAL
DO_VARIABLE:


; : ()

                HEADER  1,":",IMMEDIATE
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


                HEADER  8,"CONSTANT",IMMEDIATE
CONSTANT:



                HEADER  2,"IF",IMMEDIATE
IF:



                HEADER  2,"S""",IMMEDIATE
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

                HEADER  2,"CR",NORMAL
CR:             jmp     DO_COLON
                dw      DO_LITERAL,13,EMIT
                dw      DO_LITERAL,10,EMIT
                dw      EXIT

; EMIT ( x -- )
;
; If x is a graphic character in the implementation-defined character set,
; display x. The effect of EMIT for all other values of x is implementation
; -defined.

                HEADER  4,"EMIT",NORMAL
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

                HEADER  3,"KEY",NORMAL
                extern  UartRx
KEY:
                jsr     UartRx                  ; Receive a character
                and     #$00ff                  ; Ensure in ASCII range
                dex                             ; And push to stack
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; SOURCE ( -- c-addr u )
;
; c-addr is the address of, and u is the number of characters in, the input
; buffer.
;
; In this implementation it is defined as
;
;   TIB #TIB @

                HEADER  6,"SOURCE",NORMAL
SOURCE:         jmp     DO_COLON
                dw      TIB
                dw      HASH_TIB,FETCH
                dw      EXIT

; SPACE ( -- )
;
; Display one space.
;
; In this implementation it is defined as
;
;   SPACE EMIT

                HEADER  5,"SPACE",NORMAL
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

                HEADER  6,"SPACES",NORMAL
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

                HEADER  4,"TYPE",NORMAL
TYPE:           jmp     DO_COLON
                dw      QUERY_DUP,QUERY_BRANCH,TYPE_2
                dw      OVER,PLUS,SWAP,DO_DO
TYPE_1:         dw      I,C_FETCH,EMIT,DO_LOOP,TYPE_1
                dw      BRANCH,TYPE_3
TYPE_2          dw      DROP
TYPE_3          dw      EXIT

;================================================================================
;--------------------------------------------------------------------------------

NEXT_WORD:

                end