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
                maclist off

                chip    65816
                longi   off
                longa   off

                include "w65c816.inc"

;==============================================================================
; Macros
;------------------------------------------------------------------------------

COUNT           set     0                       ; Word counter
WORD0           equ     0                       ; Null address for first word

HEADER          macro   TYPE
WORD@<COUNT+1>:
                dw      WORD@<COUNT>
                db      TYPE
COUNT           set     COUNT+1
                endm

NORMAL          equ     $00
IMMEDIATE       equ     $80

TRAILER         macro
LAST_WORD       equ     WORD@<COUNT>
                endm

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
SOURCEID_OFFSET equ     12                      ; Input source flag
STATE_OFFSET    equ     14                      ; Compiling/Interpreting flag
BUFFER_OFFSET   equ     16                      ; Address of the input buffer
LENGTH_OFFSET   equ     18                      ; Length of the input buffer

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

FORTH           section OFFSET $0400

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
                dw      CR,CR,DO_S_QUOTE
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

                HEADER  NORMAL
                db      4,"#TIB"
HASH_TIB:       jmp     DO_CONSTANT
                dw      $+2
                dw      TIB_SIZE-2

; >IN ( -- a-addr )
;
; a-addr is the address of a cell containing the offset in characters from the
; start of the input buffer to the start of the parse area.

                HEADER  NORMAL
                db      3,">IN"
TO_IN:          jmp     DO_USER
                dw      TO_IN_OFFSET

; BASE ( -- a-addr )
;
; a-addr is the address of a cell containing the current number-conversion
; radix {{2...36}}.

                HEADER  NORMAL
                db      4,"BASE"
BASE:           jmp     DO_USER
                dw      BASE_OFFSET

; BLK ( -- a-addr )
;
; a-addr is the address of a cell containing zero or the number of the mass-
; storage block being interpreted. If BLK contains zero, the input source is
; not a block and can be identified by SOURCE-ID, if SOURCE-ID is available. An
; ambiguous condition exists if a program directly alters the contents of BLK.

                HEADER  NORMAL
                db      3,"BLK"
BLK:            jmp     DO_USER
                dw      BLK_OFFSET

; (BUFFER)

                HEADER  NORMAL
                db      8,"(BUFFER)"
BUFFER:         jmp     DO_USER
                dw      BUFFER_OFFSET

; DP ( -- a-addr )

                HEADER  NORMAL
                db      2,"DP"
DP:             jmp     DO_USER
                dw      DP_OFFSET

; LATEST ( -- a-addr )

                HEADER  NORMAL
                db      6,"LATEST"
LATEST:         jmp     DO_USER
                dw      LATEST_OFFSET

; (LENGTH)

                HEADER  NORMAL
                db      8,"(LENGTH)"
LENGTH:         jmp     DO_USER
                dw      LENGTH_OFFSET

; SCR ( -- a-addr )
;
; a-addr is the address of a cell containing the block number of the block most
; recently LISTed.

                HEADER  NORMAL
                db      3,"SCR"
SCR:            jmp     DO_USER
                dw      SCR_OFFSET

; (SOURCE-ID)

                HEADER  NORMAL
                db      11,"(SOURCE-ID)"
SOURCEID:       jmp     DO_USER
                dw      SOURCEID_OFFSET

; STATE ( -- a-addr )
;
; a-addr is the address of a cell containing the compilation-state flag. STATE
; is true when in compilation state, false otherwise. The true value in STATE
; is non-zero, but is otherwise implementation-defined.

                HEADER  NORMAL
                db      5,"STATE"
STATE:          jmp     DO_USER
                dw      STATE_OFFSET

; TIB ( -- c-addr )
;
; c-addr is the address of the terminal input buffer.

                HEADER  NORMAL
                db      3,"TIB"
TIB:            jmp     DO_CONSTANT
                dw      TIB_AREA

;==============================================================================
; Constants
;------------------------------------------------------------------------------

; 0 ( -- 0 )
;
; Push the constant value zero on the stack

                HEADER  NORMAL
                db      1,"0"
ZERO:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,X              ; And create a zero value
                jmp     NEXT                    ; Done

; BL ( -- char )
;
; char is the character value for a space.

                HEADER  NORMAL
                db      2,"BL"
BL:
                dex                             ; Make space on the stack
                dex
                lda     #' '                    ; And save a space value
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; FALSE ( -- false )
;
; Return a false flag.

                HEADER  NORMAL
                db      5,"FALSE"
FALSE:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,X              ; And create a false value
                jmp     NEXT                    ; Done

; TRUE ( -- true )
;
; Return a true flag, a single-cell value with all bits set.

                HEADER  NORMAL
                db      4,"TRUE"
TRUE:
                dex                             ; Make space on the stack
                dex
                lda     #$ffff                  ; And create a true value
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

;==============================================================================
; Radix
;------------------------------------------------------------------------------

; DECIMAL ( -- )
;
; Set the numeric conversion radix to ten (decimal).

                HEADER  NORMAL
                db      7,"DECIMAL"
DECIMAL:        jmp     DO_COLON
                dw      DO_LITERAL,10,BASE,STORE
                dw      EXIT

; HEX ( -- )
;
; Set contents of BASE to sixteen.

                HEADER  NORMAL
                db      3,"HEX"
HEX:            jmp     DO_COLON
                dw      DO_LITERAL,16,BASE,STORE
                dw      EXIT

;==============================================================================
; Memory Operations
;------------------------------------------------------------------------------

; ! ( x a-addr -- )
;
; Store x at a-addr.

                HEADER  NORMAL
                db      1,"!"
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

                HEADER  NORMAL
                db      2,"+!"
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

                HEADER  NORMAL
                db      1,","
COMMA:          jmp     DO_COLON
                dw      HERE,STORE
                dw      DO_LITERAL,1,CELLS,ALLOT
                dw      EXIT

; 2! ( x1 x2 a-addr -- )
;
; Store the cell pair x1 x2 at a-addr, with x2 at a-addr and x1 at the next
; consecutive cell. It is equivalent to the sequence SWAP OVER ! CELL+ !.

                HEADER  NORMAL
                db      2,"2!"
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

                HEADER  NORMAL
                db      2,"2@"
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

                HEADER  NORMAL
                db      1,"@"
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

                HEADER  NORMAL
                db      5,"ALLOT"
ALLOT:          jmp     DO_COLON
                dw      DP,PLUS_STORE
                dw      EXIT

; C! ( char c-addr -- )
;
; Store char at c-addr. When character size is smaller than cell size, only the
; number of low-order bits corresponding to character size are transferred.

                HEADER  NORMAL
                db      2,"C!"
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

                HEADER  NORMAL
                db      2,"C,"
C_COMMA:        jmp     DO_COLON
                dw      HERE,C_STORE
                dw      DO_LITERAL,1,CHARS,ALLOT
                dw      EXIT

; C@ ( c-addr -- char )
;
; Fetch the character stored at c-addr. When the cell size is greater than
; character size, the unused high-order bits are all zeroes.

                HEADER  NORMAL
                db      2,"C@"
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

                HEADER  NORMAL
                db      4,"HERE"
HERE:           jmp     DO_COLON
                dw      DP,FETCH
                dw      EXIT

;==============================================================================
; Alignment
;------------------------------------------------------------------------------

; ALIGN ( -- )
;
; If the data-space pointer is not aligned, reserve enough space to align it.

                HEADER  NORMAL
                db      5,"ALIGN"
ALIGN:
                jmp     NEXT                    ; Done

; ALIGNED ( addr -- a-addr )
;
; a-addr is the first aligned address greater than or equal to addr.

                HEADER  NORMAL
                db      7,"ALIGNED"
ALIGNED:
                jmp     NEXT                    ; Done

; CELL+ ( a-addr1 -- a-addr2 )
;
; Add the size in address units of a cell to a-addr1, giving a-addr2.

                HEADER  NORMAL
                db      5,"CELL+"
CELL_PLUS:
                inc     DSTACK+1,x              ; Bump the address by two
                inc     DSTACK+1,X
                jmp     NEXT

; CELLS ( n1 -- n2 )
;
; n2 is the size in address units of n1 cells.

                HEADER  NORMAL
                db      5,"CELLS"
CELLS:
                asl     DSTACK+1,x              ; Two bytes per cell
                jmp     NEXT

; CHAR+ ( c-addr1 -- c-addr2 )
;
; Add the size in address units of a character to c-addr1, giving c-addr2.

                HEADER  NORMAL
                db      5,"CHAR+"
CHAR_PLUS:
                inc     DSTACK+1,x              ; Bump the address by one
                jmp     NEXT

; CHARS ( n1 -- n2 )
;
; n2 is the size in address units of n1 characters.

                HEADER  NORMAL
                db      5,"CHARS"
CHARS:
                jmp     NEXT

;==============================================================================
; Stack Operations
;------------------------------------------------------------------------------

; 2DROP ( x1 x2 -- )
;
; Drop cell pair x1 x2 from the stack.

                HEADER  NORMAL
                db      5,"2DROP"
TWO_DROP:
                inx
                inx
                inx
                inx
                jmp     NEXT

; 2DUP ( x1 x2 -- x1 x2 x1 x2 )
;
; Duplicate cell pair x1 x2.

                HEADER  NORMAL
                db      4,"2DUP"
TWO_DUP:
                lda     DSTACK+1,x              ; Fetch the top value
                dex                             ; Make space for new values
                dex
                dex
                dex
                sta     DSTACK+1,x              ; Make two copies
                sta     DSTACK+3,x
                jmp     NEXT

; 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
;
; Copy cell pair x1 x2 to the top of the stack.

                HEADER  NORMAL
                db      5,"2OVER"
TWO_OVER:
                lda     DSTACK+7,x              ; Copy over x1
                dex
                dex
                sta     DSTACK+1,x
                lda     DSTACK+7,x              ; Copy over x2
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; 2ROT

; 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
;
; Exchange the top two cell pairs.

                HEADER  NORMAL
                db      5,"2SWAP"
TWO_SWAP:
                lda     DSTACK+3,x              ; Save x3
                pha
                lda     DSTACK+1,x              ; Save x4
                lda     DSTACK+7,x              ; Move x1
                sta     DSTACK+3,x
                lda     DSTACK+5,x              ; Move x2
                sta     DSTACK+1,x
                pla                             ; Move x4
                sta     DSTACK+5,x
                pla                             ; Move x3
                sta     DSTACK+7,x
                jmp     NEXT                    ; Done

; ?DUP ( x -- 0 | x x )
;
; Duplicate x if it is non-zero.

                HEADER  NORMAL
                db      4,"?DUP"
QUERY_DUP:
                lda     DSTACK+1,x              ; Fetch top value
                beq     QUERY_DUP_1             ; Non-zero value?
                dex
                dex
                sta     DSTACK+1,x              ; Push a copy
QUERY_DUP_1:    jmp     NEXT                    ; Done

; DROP ( x -- )
;
; Remove x from the stack.

                HEADER  NORMAL
                db      4,"DROP"
DROP:
                inx                             ; Drop the top value
                inx
                jmp     NEXT                    ; Done

; DUP ( x -- x x )
;
; Duplicate x.

                HEADER  NORMAL
                db      3,"DUP"
DUP:
                lda     DSTACK+1,x              ; Fetch top value
                dex                             ; And make a copy
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; NIP ( x1 x2 -- x2 )
;
; Drop the first item below the top of stack.

                HEADER  NORMAL
                db      3,"NIP"
NIP:
                lda     DSTACK+1,x              ; Copy x2 over x1
                sta     DSTACK+3,x
                inx                             ; Clean up the stack
                inx
                jmp     NEXT

; OVER ( x1 x2 -- x1 x2 x1 )
;
; Place a copy of x1 on top of the stack.

                HEADER  NORMAL
                db      4,"OVER"
OVER:
                lda     DSTACK+3,x              ; Fetch second value
                dex                             ; And make a copy
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; SWAP ( x1 x2 -- x2 x1 )
;
; Exchange the top two stack items.

                HEADER  NORMAL
                db      4,"SWAP"
SWAP:
                lda     DSTACK+1,x              ; Fetch top of stack
                pha                             ; .. and save
                lda     DSTACK+3,x              ; Exchange second
                sta     DSTACK+1,x              ; .. and top
                pla                             ; Recover top
                sta     DSTACK+3,x              ; .. and save as second
                jmp     NEXT                    ; Done

; ROT
; ROLL

; TUCK ( x1 x2 -- x2 x1 x2 )
;
; Copy the first (top) stack item below the second stack item.

                HEADER  NORMAL
                db      4,"TUCK"
TUCK:           jmp     DO_COLON
                dw      SWAP
                dw      OVER
                dw      EXIT

;==============================================================================
; Return Stack Operations
;------------------------------------------------------------------------------

; 2>R ( x1 x2 -- ) ( R: -- x1 x2 )
;
; Transfer cell pair x1 x2 to the return stack. Semantically equivalent to
; SWAP >R >R.

                HEADER  NORMAL
                db      3,"2>R"
TWO_TO_R:
                lda     DSTACK+3,x              ; Transfer x1
                pha
                lda     DSTACK+1,x              ; Transfer x2
                pha
                inx                             ; Clean up data stack
                inx
                inx
                inx
                jmp     NEXT                    ; Done

; 2R> ( -- x1 x2 ) ( R: x1 x2 -- )
;
; Transfer cell pair x1 x2 from the return stack. Semantically equivalent to R>
; R> SWAP.

                HEADER  NORMAL
                db      3,"2R>"
TWO_R_FROM:
                dex                             ; Make space for values
                dex
                dex
                dex
                pla                             ; Transfer x2
                sta     DSTACK+1,x
                pla                             ; Transfer x1
                sta     DSTACK+3,x
                jmp     NEXT                    ; Done

; 2R@ ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
;
; Copy cell pair x1 x2 from the return stack. Semantically equivalent to R> R>
; 2DUP >R >R SWAP.

                HEADER  NORMAL
                db      3,"2R@"
TWO_R_FETCH:
                dex                             ; Make space for values
                dex
                dex
                dex
                lda     1,s                     ; Transfer x2
                sta     DSTACK+1,x
                lda     3,s                     ; Transfer x1
                sta     DSTACK+3,x
                jmp     NEXT                    ; Done

; >R ( x -- ) ( R: -- x )
;
; Move x to the return stack.

                HEADER  NORMAL
                db      2,">R"
TO_R:
                lda     DSTACK+1,x              ; Transfer top value
                pha                             ; .. to return stack
                inx
                inx
                jmp     NEXT                    ; Done

; I ( -- n|u ) ( R: loop-sys -- loop-sys )
;
; n|u is a copy of the current (innermost) loop index. An ambiguous condition
; exists if the loop control parameters are unavailable.

                HEADER  NORMAL
                db      1,"I"
I:
                lda     1,s
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT

; J ( -- n|u ) ( R: loop-sys1 loop-sys2 -- loop-sys1 loop-sys2 )
;
; n|u is a copy of the next-outer loop index. An ambiguous condition exists if
; the loop control parameters of the next-outer loop, loop-sys1, are
; unavailable.

                HEADER  NORMAL
                db      1,"J"
J:
                lda     3,s
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT

; R> ( -- x ) ( R: x -- )
;
; Move x from the return stack to the data stack.

                HEADER  NORMAL
                db      2,"R>"
R_FROM:
                pla                             ; Fetch return stack value
                dex                             ; And push
                dex
                sta     DSTACK+1,X
                jmp     NEXT                    ; Done

; R@ ( -- x ) ( R: x -- x )
;
; Copy x from the return stack to the data stack.

                HEADER  NORMAL
                db      2,"R@"
R_FETCH:
                lda     1,s
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT

; ROT ( x1 x2 x3 -- x2 x3 x1 )
;
; Rotate the top three stack entries.

                HEADER  NORMAL
                db      3,"ROT"
ROT:            lda     DSTACK+5,x              ; Save x1
                pha
                lda     DSTACK+3,x              ; Move x2,x3 up
                sta     DSTACK+5,x
                lda     DSTACK+1,x
                sta     DSTACK+3,x
                pla                             ; Recover x1
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

;==============================================================================
; Single Precision Arithmetic
;------------------------------------------------------------------------------

; * ( n1|u1 n2|u2 -- n3|u3 )
;
; Multiply n1|u1 by n2|u2 giving the product n3|u3.

                HEADER  NORMAL
                db      1,"*"
STAR:
                jmp     NEXT                    ; Done

; */
; */MOD


; + ( n1|u1 n2|u2 -- n3|u3 )
;
; Add n2|u2 to n1|u1, giving the sum n3|u3.

                HEADER  NORMAL
                db      1,"+"
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

                HEADER  NORMAL
                db      1,"-"
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

                HEADER  NORMAL
                db      2,"1+"
ONE_PLUS:
                inc     DSTACK+1,x              ; Increment top of stack
                jmp     NEXT                    ; Done

; 1- ( n1|u1 -- n2|u2 )
;
; Subtract one (1) from n1|u1 giving the difference n2|u2.

                HEADER  NORMAL
                db      2,"1-"
ONE_MINUS:
                dec     DSTACK+1,x              ; Decrement top of stack
                jmp     NEXT                    ; Done

; 2* ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the most-significant bit,
; filling the vacated least-significant bit with zero.

                HEADER  NORMAL
                db      2,"2*"
TWO_STAR:
                asl     DSTACK+1,x              ; Multiply top value by two
                jmp     NEXT                    ; Done

; 2/ ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the least-significant bit,
; leaving the most-significant bit unchanged.

                HEADER  NORMAL
                db      2,"2/"
TWO_SLASH:
                lda     DSTACK+1,x              ; Load the top value
                rol     a                       ; Extract the top bit
                ror     DSTACK+1,x              ; And shift back into value
                jmp     NEXT

; ABS ( n -- u )
;
; u is the absolute value of n.

                HEADER  NORMAL
                db      3,"ABS"
ABS:
                sec                             ; Assume to is negative
                lda     #0
                sbc     DSTACK+1,x              ; Invert it
                bmi     ABS_1                   ; Correct assumption?
                sta     DSTACK+1,x              ; Yes, same the result
ABS_1           jmp     NEXT                    ; Done

; MAX
; MIN
; MOD

; NEGATE ( n1 -- n2 )
;
; Negate n1, giving its arithmetic inverse n2.

                HEADER  NORMAL
                db      6,"NEGATE"
NEGATE:
                sec                             ; Negate the top of stack
                lda     #0
                sbc     DSTACK+1,x
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; UMAX

                HEADER  NORMAL
                db      4,"UMAX"
UMAX:
                lda     DSTACK+1,x              ; Compare the top values
                cmp     DSTACK+3,x
                bcs     $+5                     ; Is x2 biggest?
                jmp     DROP                    ; No, x1 is
                jmp     NIP

; UMIN

                HEADER  NORMAL
                db      4,"UMIN"
UMIN:
                lda     DSTACK+1,x              ; Compare the top values
                cmp     DSTACK+3,x
                bcc     $+5                     ; Is x2 smallest?
                jmp     DROP                    ; No, x1 is
                jmp     NIP

;==============================================================================
; Double Precision Arithmetic
;------------------------------------------------------------------------------

; D+
; D-
; DNEGATE

;==============================================================================
; Mixed Arithmetic
;------------------------------------------------------------------------------


; D>S ( d -- n )
;
; n is the equivalent of d. An ambiguous condition exists if d lies outside the
; range of a signed single-cell number.

                HEADER  NORMAL
                db      3,"D>S"
D_TO_S:
                inx                             ; Drop the high word
                inx
                jmp     NEXT

; S>D ( n -- d )
;
; Convert the number n to the double-cell number d with the same numerical
; value.

                HEADER  NORMAL
                db      3,"S>D"
S_TO_D:
                dex                             ; Assume n is positive
                dex
                stz     DSTACK+1,x              ; .. push a zero value
                bit     DSTACK+3,x              ; Test the number
                bpl     S_TO_D_1
                dec     DSTACK+1,x              ; Make top -1 if negative
S_TO_D_1        jmp     NEXT                    ; Done

;==============================================================================
; Comparisons
;------------------------------------------------------------------------------

; 0< ( n -- flag )
;
; flag is true if and only if n is less than zero.

                HEADER  NORMAL
                db      2,"0<"
ZERO_LESS:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bpl     $+5                     ; Was the value negative?
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT                    ; Done

; 0<> ( x -- flag )
;
; flag is true if and only if x is not equal to zero.

                HEADER  NORMAL
                db      3,"0<>"
ZERO_NOT_EQUAL:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                beq     $+5                     ; Was the value non-zero?
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT                    ; Done

; 0= ( x -- flag )
;
; flag is true if and only if x is equal to zero.

                HEADER  NORMAL
                db      2,"0="
ZERO_EQUAL:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bne     $+5                     ; Was the value zero?
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT                    ; Done

; 0> ( n -- flag )
;
; flag is true if and only if n is greater than zero.

                HEADER  NORMAL
                db      2,"0>"
ZERO_GREATER:
                lda     DSTACK+1,x              ; Test top of stack
                stz     DSTACK+1,x              ; Assume false result
                bmi     $+7                     ; Was the value positive?
                beq     $+5                     ; .. but not zero
                dec     DSTACK+1,x              ; Yes, make true result
                jmp     NEXT                    ; Done

; <

; <>

                HEADER  NORMAL
                db      2,"<>"
NOT_EQUAL:
                lda     DSTACK+1,x              ; Pull x2 from stack
                inx
                inx
                cmp     DSTACK+1,x              ; Compare with x1
                stz     DSTACK+1,x              ; Assume equal
                beq     $+5                     ; Test flags
                dec     DSTACK+1,x              ; Make result true
                jmp     NEXT                    ; Done

; = ( x1 x2 -- flag )
;
; flag is true if and only if x1 is bit-for-bit the same as x2.

                HEADER  NORMAL
                db      1,"="
EQUAL:
                lda     DSTACK+1,x              ; Pull x2 from stack
                inx
                inx
                cmp     DSTACK+1,x              ; Compare with x1
                stz     DSTACK+1,x              ; Assume not equal
                bne     $+5                     ; Test the flags
                inc     DSTACK+1,x              ; Make result true
                jmp     NEXT                    ; Done

; >

; U<
; U>

;==============================================================================
; Logical Operations
;------------------------------------------------------------------------------

; AND ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit logical “and” of x1 with x2.

                HEADER  NORMAL
                db      3,"AND"
AND:
                lda     DSTACK+1,x
                and     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                jmp     NEXT

; INVERT ( x1 -- x2 )
;
; Invert all bits of x1, giving its logical inverse x2.

                HEADER  NORMAL
                db      6,"INVERT"
INVERT:
                lda     DSTACK+1,x              ; Fetch top value
                eor     #$ffff                  ; Invert all the bits
                sta     DSTACK+1,x              ; .. and write back
                jmp     NEXT                    ; Done

; LSHIFT ( x1 u -- x2 )
;
; Perform a logical left shift of u bit-places on x1, giving x2. Put zeroes
; into the least significant bits vacated by the shift. An ambiguous condition
; exists if u is greater than or equal to the number of bits in a cell.

                HEADER  NORMAL
                db      6,"LSHIFT"
LSHIFT:
                lda     DSTACK+1,x              ; Pull bit count
                php
                inx                             ; .. from the stack
                inx
                plp
                beq     LSHIFT_0                ; Zero shift?
                cmp     #16                     ; Shifting by 16+ bits
                bcs     LSHIFT_2                ; Yes, result will be zero
LSHIFT_1        asl     DSTACK+1,x              ; Shift one bit left
                dec     a                       ; Update count
                bne     LSHIFT_1                ; .. and repeat as needed
LSHIFT_0        jmp     NEXT                    ; Done
LSHIFT_2        stz     DSTACK+1,x              ; Clear top value
                jmp     NEXT                    ; Done

; OR ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit inclusive-or of x1 with x2.

                HEADER  NORMAL
                db      2,"OR"
OR:
                lda     DSTACK+1,x
                ora     DSTACK+3,x
                sta     DSTACK+3,x
                inx
                inx
                jmp     NEXT

; RSHIFT ( x1 u -- x2 )
;
; Perform a logical right shift of u bit-places on x1, giving x2. Put zeroes
; into the most significant bits vacated by the shift. An ambiguous condition
; exists if u is greater than or equal to the number of bits in a cell.

                HEADER  NORMAL
                db      6,"RSHIFT"
RSHIFT:
                lda     DSTACK+1,x              ; Pull bit count
                php
                inx                             ; .. from the stack
                inx
                plp
                beq     RSHIFT_0                ; Zero shift?
                cmp     #16                     ; Shifting by 16+ bits
                bcs     RSHIFT_2                ; Yes, result will be zero
RSHIFT_1        lsr     DSTACK+1,x              ; Shift one bit left
                dec     a                       ; Update count
                bne     RSHIFT_1                ; .. and repeat as needed
RSHIFT_0        jmp     NEXT                    ; Done
RSHIFT_2        stz     DSTACK+1,x              ; Clear top value
                jmp     NEXT                    ; Done

; XOR ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit exclusive-or of x1 with x2.

                HEADER  NORMAL
                db      3,"XOR"
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

                HEADER  NORMAL
                db      5,"ABORT"
ABORT:          jmp     DO_COLON
                dw      DO_ABORT
                dw      QUIT

DO_ABORT:
                ldx     #DSTACK_INIT
                jmp     NEXT

; (BUILD) ( dtc-addr -- )
;
; Adds a jump the to exection function for the new word.

                HEADER  NORMAL
                db      7,"(BUILD)"
BUILD:          jmp     DO_COLON
                dw      DO_LITERAL,$4c,C_COMMA
                dw      COMMA,EXIT

; CREATE ( -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below. If the data-
; space pointer is not aligned, reserve enough data space to align it. The new
; data-space pointer defines name’s data field. CREATE does not allocate data
; space in name’s data field.

                HEADER  NORMAL
                db      6,"CREATE"
CREATE:         jmp     DO_COLON
                ; parse
                dw      HERE,LATEST,FETCH,COMMA
                dw      ZERO,C_COMMA,LATEST,STORE
                ; move name
                dw      EXIT
                
; EXECUTE

                HEADER  NORMAL
                db      7,"EXECUTE"
EXECUTE:
                lda     DSTACK+1,x
                inx
                inx
                dec     a
                pha
                rts

; EXIT ( -- ) ( R: nest-sys -- )

                HEADER  NORMAL
                db      4,"EXIT"
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
;   0 (SOURCE-ID) !
;   BEGIN
;       REFILL
;       WHILE SOURCE EVALUATE
;       STATE @ 0= IF CR S" OK" TYPE THEN
;   AGAIN ;

                HEADER  NORMAL
                db      4,"QUIT"
QUIT:           jmp     DO_COLON
                dw      DO_QUIT
                dw      ZERO,STATE,STORE
                dw      ZERO,SOURCEID,STORE
QUIT_1:         dw      REFILL,QUERY_BRANCH,QUIT_2
                dw      INTERPRET
QUIT_2:         dw      STATE,FETCH,ZERO_EQUAL
                dw      QUERY_BRANCH,QUIT_3
                dw      DO_S_QUOTE
                db      2,"Ok",TYPE
QUIT_3:         dw      BRANCH,QUIT_1

DO_QUIT:
                lda     #RSTACK_INIT            ; Reset the return stack
                tcs
                jmp     NEXT                    ; Done

;==============================================================================
; Parser & Interpreter
;------------------------------------------------------------------------------

; ?NUMBER

                HEADER  NORMAL
                db      7,"?NUMBER"
QUERY_NUMBER:   db      DO_COLON

                dw      EXIT

; ACCEPT ( c-addr +n1 -- +n2 )
;
; Receive a string of at most +n1 characters. An ambiguous condition exists if
; +n1 is zero or greater than 32,767. Display graphic characters as they are
; received. A program that depends on the presence or absence of non-graphic
; characters in the string has an environmental dependency. The editing
; functions, if any, that the system performs in order to construct the string
; are implementation-defined.
;
; Input terminates when an implementation-defined line terminator is received.
; When input terminates, nothing is appended to the string, and the display is
; maintained in an implementation-defined way.
;
; +n2 is the length of the string stored at c-addr.
;
;   OVER + 1- OVER      -- sa ea a
;   BEGIN KEY           -- sa ea a c
;   DUP 0D <> WHILE
;       DUP EMIT        -- sa ea a c
;       DUP 8 = IF  DROP 1-
;             >R OVER R> UMAX
;             ELSE  OVER C! 1+ OVER UMIN
;       THEN            -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - ;

                HEADER  NORMAL
                db      6,"ACCEPT"
ACCEPT:         jmp     DO_COLON
                dw      OVER,PLUS,ONE_MINUS,OVER
ACCEPT_1:       dw      KEY,DUP,DO_LITERAL,$0D,NOT_EQUAL
                dw      QUERY_BRANCH,ACCEPT_4
                dw      DUP,EMIT
                dw      DUP,DO_LITERAL,$08,EQUAL
                dw      OVER,DO_LITERAL,$7f,EQUAL,OR
                dw      QUERY_BRANCH,ACCEPT_2
                dw      DROP,ONE_MINUS
                dw      TO_R,OVER,R_FROM,UMAX,BRANCH,ACCEPT_3
ACCEPT_2:       dw      OVER,C_STORE,ONE_PLUS,OVER,UMIN
ACCEPT_3:       dw      BRANCH,ACCEPT_1
ACCEPT_4:       dw      DROP,NIP,SWAP,MINUS,EXIT

; EVALUATE ( i*x c-addr u -- j*x )
;
; Save the current input source specification. Store minus-one (-1) in
; SOURCE-ID if it is present. Make the string described by c-addr and u both
; the input source and input buffer, set >IN to zero, and interpret. When the
; parse area is empty, restore the prior input source specification. Other
; stack effects are due to the words EVALUATEd.
;
;   >R >R SAVE-INPUT
;   -1 (SOURCE-ID) !
;   0 >IN ! (LENGTH) ! (BUFFER) !
;   INTERPRET
;   RESTORE-INPUT DROP

                HEADER  NORMAL
                db      8,"EVALUATE"
EVALUATE:       jmp     DO_COLON
                dw      TO_R,TO_R,SAVE_INPUT
                dw      R_FROM,R_FROM
                dw      TRUE,SOURCEID,STORE
                dw      ZERO,TO_IN,STORE
                dw      LENGTH,STORE
                dw      BUFFER,STORE
                dw      INTERPRET
                dw      RESTORE_INPUT,DROP
                dw      EXIT

; INTERPRET ( -- )
;
;
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       FIND                    -- a 0/1/-1
;       ?DUP IF                 -- xt 1/-1
;           1+ STATE @ 0= OR    immed or interp?
;           IF EXECUTE ELSE ,XT THEN
;       ELSE                    -- textadr
;           ?NUMBER
;           IF POSTPONE LITERAL     converted ok
;           ELSE COUNT TYPE 3F EMIT CR ABORT  err
;           THEN
;       THEN
;   REPEAT DROP ;

                HEADER  INTERPRET
                dw      9,"INTERPRET"
INTERPRET:      jmp     DO_COLON
INTERPRET_1:    dw      BL,WORD,DUP,C_FETCH,QUERY_BRANCH,INTERPRET_7
                dw      FIND,QUERY_DUP,QUERY_BRANCH,INTERPRET_4
                dw      ONE_PLUS,STATE,FETCH,ZERO_EQUAL,OR
                dw      QUERY_BRANCH,INTERPRET_2,EXECUTE,BRANCH,INTERPRET_3
INTERPRET_2:    dw      COMMA
INTERPRET_3:    dw      BRANCH,INTERPRET_6
INTERPRET_4:    dw      QUERY_NUMBER,QUERY_BRANCH,INTERPRET_5
                dw      LITERAL,BRANCH,INTERPRET_6
INTERPRET_5:    dw      COUNT,TYPE,DO_LITERAL,$3f,EMIT,CR,ABORT
INTERPRET_6     dw      BRANCH,INTERPRET_1
INTERPRET_7:    dw      DROP,EXIT

; FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
;
; Find the definition named in the counted string at c-addr. If the definition
; is not found, return c-addr and zero. If the definition is found, return its
; execution token xt. If the definition is immediate, also return one (1),
; otherwise also return minus-one (-1). For a given string, the values returned
; by FIND while compiling may differ from those returned while not compiling.
;
;   LATEST @ BEGIN             -- a nfa
;       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
;       N=                     -- a nfa f
;       DUP IF
;           DROP
;           NFA>LFA H@ DUP     -- a link link
;       THEN
;   0= UNTIL                   -- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA        -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;   THEN ;

                HEADER  NORMAL
                db      4,"FIND"
FIND:           jmp     DO_COLON



                dw      EXIT

; REFILL ( -- flag )
;
; Attempt to fill the input buffer from the input source, returning a true flag
; if successful.
;
; When the input source is the user input device, attempt to receive input into
; the terminal input buffer. If successful, make the result the input buffer,
; set >IN to zero, and return true. Receipt of a line containing no characters
; is considered successful. If there is no input available from the current
; input source, return false.
;
; When the input source is a string from EVALUATE, return false and perform no
; other action.
;
;   SOURCE-ID 0= IF
;    TIB DUP #TIB @ ACCEPT SPACE
;    LENGTH ! BUFFER !
;    0 >IN ! TRUE EXIT
;   THEN
;   FALSE

                HEADER  NORMAL
                db      6,"REFILL"
REFILL:         jmp     DO_COLON
                dw      SOURCEID,ZERO_EQUAL,QUERY_BRANCH,REFILL_1
                dw      TIB,DUP,HASH_TIB,FETCH,ACCEPT,SPACE
                dw      LENGTH,STORE,BUFFER,STORE
                dw      ZERO,TO_IN,STORE,TRUE,EXIT
REFILL_1:       dw      FALSE,EXIT

; RESTORE-INPUT

                HEADER  NORMAL
                db      13,"RESTORE-INPUT"
RESTORE_INPUT   jmp     DO_COLON
                dw      TO_IN,STORE
                dw      LENGTH,STORE
                dw      BUFFER,STORE
                dw      SOURCEID,STORE
                dw      TRUE,EXIT

; SAVE-INPUT

                HEADER  NORMAL
                db      10,"SAVE-INPUT"
SAVE_INPUT:     jmp     DO_COLON
                dw      SOURCEID,FETCH
                dw      BUFFER,FETCH
                dw      LENGTH,FETCH
                dw      TO_IN,FETCH,EXIT

; SOURCE ( -- c-addr u )
;
; c-addr is the address of, and u is the number of characters in, the input
; buffer.
;
; In this implementation it is defined as
;
;   BUFFER @ LENGTH @

                HEADER  NORMAL
                db      6,"SOURCE"
SOURCE:         jmp     DO_COLON
                dw      BUFFER,FETCH
                dw      LENGTH,FETCH
                dw      EXIT

; SOURCE-ID ( -- 0 | -1 )
;
; Identifies the input source: -1 if string (via EVALUATE), 0 if user input
; device.

                HEADER  NORMAL
                db      9,"SOURCE-ID"
SOURCE_ID:      jmp     DO_COLON
                dw      SOURCEID,FETCH,EXIT

; WORD
;
;   DUP  SOURCE >IN @ /STRING   -- c c adr n
;   DUP >R   ROT SKIP           -- c adr' n'
;   OVER >R  ROT SCAN           -- adr" n"
;   DUP IF CHAR- THEN        skip trailing delim.
;   R> R> ROT -   >IN +!        update >IN offset
;   TUCK -                      -- adr' N
;   HERE >counted               --
;   HERE                        -- a
;   BL OVER COUNT + C! ;    append trailing blank

                HEADER  NORMAL
                db      4,"WORD"
WORD:           jmp     DO_COLON
                dw      DUP,SOURCE,TO_IN,FETCH,SLASH_STRING
                dw      DUP,TO_R,ROT,SKIP
                dw      OVER,TO_R,ROT,SCAN

                dw      EXIT

SKIP:           jmp     DO_COLON

                dw      EXIT

SCAN:           jmp     DO_COLON

                dw      EXIT

;==============================================================================
; String Words
;------------------------------------------------------------------------------

; -TRAILING

                HEADER  NORMAL
                db      9,"-TRAILING"
DASH_TRAILING:  jmp     DO_COLON

; /STRING ( c-addr1 u1 n -- c-addr2 u2 )
;
; Adjust the character string at c-addr1 by n characters. The resulting
; character string, specified by c-addr2 u2, begins at c-addr1 plus n;
; characters and is u1 minus n characters long.
;
;   ROT OVER + ROT ROT -

                HEADER  NORMAL
                db      7,"/STRING"
SLASH_STRING:   jmp     DO_COLON
                dw      ROT,OVER,PLUS
                dw      ROT,ROT,MINUS,EXIT

; BLANK

; CMOVE ( c-addr1 c-addr2 u -- )
;
; If u is greater than zero, copy u consecutive characters from the data space
; starting at c-addr1 to that starting at c-addr2, proceeding character-by-
; character from lower addresses to higher addresses.

		HEADER	NORMAL
		db	5,"CMOVE"
CMOVE:
		lda	DSTACK+1,x		; Any data to move?
		bne	$+5
		jmp	NEXT			; Done
		short_a
		lda	(DSTACK+5,x)		; Move a byte
		sta	(DSTACK+3,x)
		long_a
		inc	DSTACK+5,x		; Adjust the address
		inc	DSTACK+3,x
		dec	DSTACK+1,x		; Adjust the count
		bra	CMOVE			; And try again

; CMOVE> ( c-addr1 c-addr2 u -- )
;
; If u is greater than zero, copy u consecutive characters from the data space
; starting at c-addr1 to that starting at c-addr2, proceeding character-by-
; character from higher addresses to lower addresses.

		HEADER	NORMAL
		db	6,"CMOVE>"
CMOVE_GREATER:
		jmp	NEXT

; COMPARE

; SEARCH


;==============================================================================
; Compiling Words
;------------------------------------------------------------------------------

; +LOOP ( -- )

                HEADER  IMMEDIATE
                db      5,"+LOOP"
PLUS_LOOP:      jmp     DO_COLON

                dw      EXIT

; (+LOOP)

                HEADER  NORMAL
                db      7,"(+LOOP)"
DO_PLUS_LOOP:

                jmp     NEXT

; : ( -- )

                HEADER  NORMAL
                db      1,":"
COLON:          jmp     DO_COLON

                dw      EXIT

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

; AGAIN

                HEADER  IMMEDIATE
                dw      5,"AGAIN"
AGAIN:          jmp     DO_COLON

                dw      EXIT

; BEGIN

                HEADER  IMMEDIATE
                dw      5,"BEGIN"
BEGIN:          jmp     DO_COLON

                dw      EXIT

; CONSTANT ( x “<spaces>name” -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below.

                HEADER  NORMAL
                db      8,"CONSTANT"
CONSTANT:       jmp     DO_COLON
                dw      CREATE
                dw      DO_LITERAL,DO_CONSTANT,BUILD
                dw      COMMA,EXIT

; (CONSTANT) ( -- x )

                HEADER  NORMAL
                db      10,"(CONSTANT)"
DO_CONSTANT:
                phy                             ; Save IP
                tay                             ; Fetch constant after WA
                lda     3,y
                dex                             ; And push
                dex
                sta     DSTACK+1,x
                ply
                jmp     NEXT                    ; Done

; DO ( -- )

                HEADER  IMMEDIATE
                db      2,"DO"
DO:             jmp     DO_COLON

                dw      EXIT

; (DO) ( -- )

                HEADER  NORMAL
                db      4,"(DO)"
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

; ELSE

                HEADER  IMMEDIATE
                db      4,"ELSE"
ELSE:           jmp     DO_COLON

                dw      EXIT

; (BRANCH) ( -- )
;
; Cause the IP to be loaded with the word following the link to this word.

BRANCH:
                lda     0,y                     ; Load branch address into IP
                tay
                jmp     NEXT                    ; Done



; IF

                HEADER  IMMEDIATE
                db      2,"IF"
IF:             jmp     DO_COLON

                dw      EXIT

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

; LITERAL ( x -- )
;
; Append the run-time semantics given below to the current definition.

                HEADER  IMMEDIATE
                db      7,"LITERAL"
LITERAL:        jmp     DO_COLON
                dw      DO_LITERAL,DO_LITERAL,COMMA
                dw      COMMA,EXIT

; (LITERAL) ( -- x )
;
; Place x on the stack.

                HEADER  NORMAL
                db      10,"(LITERAL)"
DO_LITERAL:
                lda     0,y                     ; Fetch constant from IP
                iny
                iny
                dex                             ; And push
                dex
                sta     DSTACK+1,x
                jmp     NEXT                    ; Done

; LOOP

                HEADER  IMMEDIATE
                db      4,"LOOP"
LOOP:           jmp     DO_COLON

                dw      EXIT

; (LOOP)

                HEADER  NORMAL
                db      6,"(LOOP)"
DO_LOOP
                lda     1,s                     ; Add one to loop counter
                inc     a
                sta     1,s
                cmp     3,s                     ; Reached limit?
                bcs     DO_LOOP_END             ; Yes
                lda     0,y                     ; No, branch back to start
                tay
                jmp     NEXT                    ; Done
DO_LOOP_END:    iny                             ; Skip over address
                iny
                jmp     NEXT                    ; Done

; USER

                HEADER  NORMAL
                db      4,"USER"
USER:           jmp     DO_COLON

                dw      EXIT

                HEADER  NORMAL
                db      6,"(USER)"
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

; VARIABLE

                HEADER  NORMAL
                db      8,"VARIABLE"
VARIABLE:       jmp     DO_COLON

                dw      EXIT

                HEADER  NORMAL
                db      10,"(VARIABLE)"
DO_VARIABLE:
                clc
                adc     #3
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT





                HEADER  IMMEDIATE
                db      2,"S",'"'
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

                HEADER  NORMAL
                db      2,"CR"
CR:             jmp     DO_COLON
                dw      DO_LITERAL,13,EMIT
                dw      DO_LITERAL,10,EMIT
                dw      EXIT

; EMIT ( x -- )
;
; If x is a graphic character in the implementation-defined character set,
; display x. The effect of EMIT for all other values of x is implementation
; -defined.

                HEADER  NORMAL
                db      4,"EMIT"
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

                HEADER  NORMAL
                db      3,"KEY"
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

                HEADER  NORMAL
                db      5,"SPACE"
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

                HEADER  NORMAL
                db      6,"SPACES"
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

                HEADER  NORMAL
                db      4,"TYPE"
TYPE:           jmp     DO_COLON
                dw      QUERY_DUP,QUERY_BRANCH,TYPE_2
                dw      OVER,PLUS,SWAP,DO_DO
TYPE_1:         dw      I,C_FETCH,EMIT,DO_LOOP,TYPE_1
                dw      BRANCH,TYPE_3
TYPE_2          dw      DROP
TYPE_3          dw      EXIT

;================================================================================
;--------------------------------------------------------------------------------

; #
; #>
; #S
; SIGN

;================================================================================
;--------------------------------------------------------------------------------

                TRAILER
NEXT_WORD:

                end