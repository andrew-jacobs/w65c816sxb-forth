;==============================================================================
;     _    _   _ ____    _____          _   _       _  ___  _  __
;    / \  | \ | / ___|  |  ___|__  _ __| |_| |__   ( )( _ )/ |/ /_
;   / _ \ |  \| \___ \  | |_ / _ \| '__| __| '_ \  |/ / _ \| | '_ \
;  / ___ \| |\  |___) | |  _| (_) | |  | |_| | | |   | (_) | | (_) |
; /_/   \_\_| \_|____/  |_|  \___/|_|   \__|_| |_|    \___/|_|\___/
;
; A Direct Threaded ANS Forth for the WDC 65C816
;------------------------------------------------------------------------------
; Copyright (C)2015-2016 HandCoded Software Ltd.
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
; The Forth data stack is DP
;
; The Y register holds the forth instruction pointer and the direct page
; register is used to access the word address pointer and user variables.
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

USER_SIZE       equ     20
DSTACK_SIZE     equ     128
RSTACK_SIZE     equ     128

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

USER_AREA       ds      USER_SIZE               ; User Variables


DSTACK_START	equ	$0100
DSTACK_END	equ	DSTACK_START+DSTACK_SIZE

RSTACK_START	equ	$0180
RSTACK_END	equ	RSTACK_START+RSTACK_SIZE


                data
                org     $0200

TIB_AREA        ds      TIB_SIZE                ; Terminal Input Buffer

;==============================================================================
; Forth Entry Point
;------------------------------------------------------------------------------

FORTH           section OFFSET $0400

                public  Start
Start:
                native                          ; Go to native mode
                long_ai                         ; And all 16-bit registers
                lda     #RSTACK_END-1           ; Initialise return stack
                tcs
                lda     #DSTACK_END-1           ; .. and data stack
                tcd

                ldy     #COLD                   ; Then perform COLD start
                jmp     NEXT

COLD:
                dw      DECIMAL
                dw      ZERO,BLK,STORE
                dw      FALSE,STATE,STORE
                dw      CR,CR,DO_S_QUOTE
                db      35,"HandCoded W65C816 ANS-Forth [16.02]"
                dw      TYPE,CR,CR
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
HASH_TIB:       jsr     DO_CONSTANT
                dw      $+2
                dw      TIB_SIZE-2

; >IN ( -- a-addr )
;
; a-addr is the address of a cell containing the offset in characters from the
; start of the input buffer to the start of the parse area.

                HEADER  NORMAL
                db      3,">IN"
TO_IN:          jsr     DO_USER
                dw      TO_IN_OFFSET

; BASE ( -- a-addr )
;
; a-addr is the address of a cell containing the current number-conversion
; radix {{2...36}}.

                HEADER  NORMAL
                db      4,"BASE"
BASE:           jsr     DO_USER
                dw      BASE_OFFSET

; BLK ( -- a-addr )
;
; a-addr is the address of a cell containing zero or the number of the mass-
; storage block being interpreted. If BLK contains zero, the input source is
; not a block and can be identified by SOURCE-ID, if SOURCE-ID is available. An
; ambiguous condition exists if a program directly alters the contents of BLK.

                HEADER  NORMAL
                db      3,"BLK"
BLK:            jsr     DO_USER
                dw      BLK_OFFSET

; (BUFFER)

                HEADER  NORMAL
                db      8,"(BUFFER)"
BUFFER:         jsr     DO_USER
                dw      BUFFER_OFFSET

; DP ( -- a-addr )

                HEADER  NORMAL
                db      2,"DP"
DP:             jsr     DO_USER
                dw      DP_OFFSET

; LATEST ( -- a-addr )

                HEADER  NORMAL
                db      6,"LATEST"
LATEST:         jsr     DO_USER
                dw      LATEST_OFFSET

; (LENGTH)

                HEADER  NORMAL
                db      8,"(LENGTH)"
LENGTH:         jsr     DO_USER
                dw      LENGTH_OFFSET

; SCR ( -- a-addr )
;
; a-addr is the address of a cell containing the block number of the block most
; recently LISTed.

                HEADER  NORMAL
                db      3,"SCR"
SCR:            jsr     DO_USER
                dw      SCR_OFFSET

; (SOURCE-ID)

                HEADER  NORMAL
                db      11,"(SOURCE-ID)"
SOURCEID:       jsr     DO_USER
                dw      SOURCEID_OFFSET

; STATE ( -- a-addr )
;
; a-addr is the address of a cell containing the compilation-state flag. STATE
; is true when in compilation state, false otherwise. The true value in STATE
; is non-zero, but is otherwise implementation-defined.

                HEADER  NORMAL
                db      5,"STATE"
STATE:          jsr     DO_USER
                dw      STATE_OFFSET

; TIB ( -- c-addr )
;
; c-addr is the address of the terminal input buffer.

                HEADER  NORMAL
                db      3,"TIB"
TIB:            jsr     DO_CONSTANT
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
                tdc
                dec     a                       ; Make space on the stack
                dec     a
                tcd
                stz     <1                      ; And create a zero value
                jmp     NEXT                    ; Done

; BL ( -- char )
;
; char is the character value for a space.

                HEADER  NORMAL
                db      2,"BL"
BL:
                tdc
                dec     a                       ; Make space on the stack
                dec     a
                tcd
                lda     #' '                    ; And save a space value
                sta     <1
                jmp     NEXT                    ; Done

; FALSE ( -- false )
;
; Return a false flag.

                HEADER  NORMAL
                db      5,"FALSE"
FALSE:
                tdc
                dec     a                       ; Make space on the stack
                dec     a
                tcd
                stz     <1                      ; And create a false value
                jmp     NEXT                    ; Done

; TRUE ( -- true )
;
; Return a true flag, a single-cell value with all bits set.

                HEADER  NORMAL
                db      4,"TRUE"
TRUE:
                tdc
                dec     a                       ; Make space on the stack
                dec     a
                tcd
                stz     <1                      ; And create a true value
                dec     <1
                jmp     NEXT                    ; Done

;==============================================================================
; Radix
;------------------------------------------------------------------------------

; DECIMAL ( -- )
;
; Set the numeric conversion radix to ten (decimal).

                HEADER  NORMAL
                db      7,"DECIMAL"
DECIMAL:        jsr     DO_COLON
                dw      DO_LITERAL,10,BASE,STORE
                dw      EXIT

; HEX ( -- )
;
; Set contents of BASE to sixteen.

                HEADER  NORMAL
                db      3,"HEX"
HEX:            jsr     DO_COLON
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
                lda     <3                      ; Fetch data value
                sta     (1)                     ; .. and store
                tdc                             ; Clean up data stack
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                jmp     NEXT                    ; Done

; +! ( n|u a-addr -- )
;
; Add n|u to the single-cell number at a-addr.

                HEADER  NORMAL
                db      2,"+!"
PLUS_STORE:
                clc
                lda     <3                      ; Fetch data value
                adc     (1)
                sta     (1)
                tdc                             ; Clean up data stack
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
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
COMMA:          jsr     DO_COLON
                dw      HERE,STORE
                dw      DO_LITERAL,1,CELLS,ALLOT
                dw      EXIT

; 2! ( x1 x2 a-addr -- )
;
; Store the cell pair x1 x2 at a-addr, with x2 at a-addr and x1 at the next
; consecutive cell. It is equivalent to the sequence SWAP OVER ! CELL+ !.

                HEADER  NORMAL
                db      2,"2!"
TWO_STORE:      jsr     DO_COLON
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
TWO_FETCH:      jsr     DO_COLON
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
                lda     (1)                     ; Fetch from memory
                sta     <1                      ; .. and replace top value
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
ALLOT:          jsr     DO_COLON
                dw      DP,PLUS_STORE
                dw      EXIT

; C! ( char c-addr -- )
;
; Store char at c-addr. When character size is smaller than cell size, only the
; number of low-order bits corresponding to character size are transferred.

                HEADER  NORMAL
                db      2,"C!"
C_STORE:
                lda     <3                      ; Fetch the data value
                short_a
                sta     (1)                     ; And store it
                long_a
                tdc                             ; Clean up the stack
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
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
C_COMMA:        jsr     DO_COLON
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
                short_a
                lda     (1)                     ; Fetch the data byte
                sta     <1                      ; .. and replace stack value
                stz     <2
                long_a
                jmp     NEXT                    ; Done

; HERE

                HEADER  NORMAL
                db      4,"HERE"
HERE:           jsr     DO_COLON
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
                inc     <1                      ; Bump the address by two
                inc     <1
                jmp     NEXT

; CELLS ( n1 -- n2 )
;
; n2 is the size in address units of n1 cells.

                HEADER  NORMAL
                db      5,"CELLS"
CELLS:
                asl     <1                      ; Two bytes per cell
                jmp     NEXT

; CHAR+ ( c-addr1 -- c-addr2 )
;
; Add the size in address units of a character to c-addr1, giving c-addr2.

                HEADER  NORMAL
                db      5,"CHAR+"
CHAR_PLUS:
                inc     <1                      ; Bump the address by one
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
                tdc                             ; Removed two words from stack
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                jmp     NEXT                    ; Done

; 2DUP ( x1 x2 -- x1 x2 x1 x2 )
;
; Duplicate cell pair x1 x2.

                HEADER  NORMAL
                db      4,"2DUP"
TWO_DUP:
                tdc                             ; Make space for new value
                dec     a
                dec     a
                dec     a
                dec     a
                tcd
                lda     <5                      ; Copy top two values
                sta     <1
                lda     <7
                sta     <3
                jmp     NEXT                    ; Done

; 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
;
; Copy cell pair x1 x2 to the top of the stack.

                HEADER  NORMAL
                db      5,"2OVER"
TWO_OVER:
                tdc                             ; Make space for new value
                dec     a
                dec     a
                dec     a
                dec     a
                tcd
                lda     <9                      ; Ciopy top two values
                sta     <1
                lda     <11
                sta     <3
                jmp     NEXT                    ; Done

; 2ROT

; TODO

; 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
;
; Exchange the top two cell pairs.

                HEADER  NORMAL
                db      5,"2SWAP"
TWO_SWAP:
                lda     <3                      ; Save x3
                pha
                lda     <1                      ; Save x4
                pha
                lda     <7                      ; Move x1
                sta     <3
                lda     <5                      ; Move x2
                sta     <1
                pla                             ; Move x4
                sta     <5
                pla                             ; Move x3
                sta     <7
                jmp     NEXT                    ; Done

; ?DUP ( x -- 0 | x x )
;
; Duplicate x if it is non-zero.

                HEADER  NORMAL
                db      4,"?DUP"
QUERY_DUP:
                lda     <1                      ; Fetch top value
                bne     DUP                     ; Non-zero value?
                jmp     NEXT                    ; Done

; DROP ( x -- )
;
; Remove x from the stack.

                HEADER  NORMAL
                db      4,"DROP"
DROP:
                tdc                             ; Drop the top value
                inc     a
                inc     a
                tcd
                jmp     NEXT                    ; Done

; DUP ( x -- x x )
;
; Duplicate x.

                HEADER  NORMAL
                db      3,"DUP"
DUP:
                tdc
                dec     a
                dec     a
                tcd
                lda     <3                      ; Fetch top value
                sta     <1                      ; And make a copy
                jmp     NEXT                    ; Done

; NIP ( x1 x2 -- x2 )
;
; Drop the first item below the top of stack.

                HEADER  NORMAL
                db      3,"NIP"
NIP:
                lda     <1                      ; Copy x2 over x1
                sta     <3
                bra     DROP

; OVER ( x1 x2 -- x1 x2 x1 )
;
; Place a copy of x1 on top of the stack.

                HEADER  NORMAL
                db      4,"OVER"
OVER:
                tdc
                dec     a
                dec     a
                tcd
                lda     <5                      ; Fetch second value
                sta     <1                      ; And make a copy
                jmp     NEXT                    ; Done

; SWAP ( x1 x2 -- x2 x1 )
;
; Exchange the top two stack items.

                HEADER  NORMAL
                db      4,"SWAP"
SWAP:
                lda     <1                      ; Switch top two words
                ldx     <3
                sta     <3
                stx     <1
                jmp     NEXT                    ; Done

; ROT ( x1 x2 x3 -- x2 x3 x1 )
;
; Rotate the top three stack entries.

                HEADER  NORMAL
                db      3,"ROT"
ROT:
                ldx     <5                      ; Save x1
                lda     <3                      ; Move x2
                sta     <5
                lda     <1                      ; Move x3
                sta     <3
                stx     <1                      ; Restore x1
                jmp     NEXT

; ROLL [TODO]

; TUCK ( x1 x2 -- x2 x1 x2 )
;
; Copy the first (top) stack item below the second stack item.

                HEADER  NORMAL
                db      4,"TUCK"
TUCK:           jsr     DO_COLON
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
                lda     <3                      ; Transfer x1
                pha
                lda     <1                      ; Transfer x2
                pha
                tdc
                inc     a                       ; Clean up data stack
                inc     a
                inc     a
                inc     a
                tcd
                jmp     NEXT                    ; Done

; 2R> ( -- x1 x2 ) ( R: x1 x2 -- )
;
; Transfer cell pair x1 x2 from the return stack. Semantically equivalent to R>
; R> SWAP.

                HEADER  NORMAL
                db      3,"2R>"
TWO_R_FROM:
                tdc
                dec     a                       ; Make space for values
                dec     a
                dec     a
                dec     a
                tcd
                pla                             ; Transfer x2
                sta     <1
                pla                             ; Transfer x1
                sta     <3
                jmp     NEXT                    ; Done

; 2R@ ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
;
; Copy cell pair x1 x2 from the return stack. Semantically equivalent to R> R>
; 2DUP >R >R SWAP.

                HEADER  NORMAL
                db      3,"2R@"
TWO_R_FETCH:
                tdc
                dec     a                       ; Make space for values
                dec     a
                dec     a
                dec     a
                tcd
                lda     1,s                     ; Transfer x2
                sta     <1
                lda     3,s                     ; Transfer x1
                sta     <3
                jmp     NEXT                    ; Done

; >R ( x -- ) ( R: -- x )
;
; Move x to the return stack.

                HEADER  NORMAL
                db      2,">R"
TO_R:
                lda     <1                      ; Transfer top value
                pha                             ; .. to return stack
                tdc
                inc     a
                inc     a
                tcd
                jmp     NEXT                    ; Done

; I ( -- n|u ) ( R: loop-sys -- loop-sys )
;
; n|u is a copy of the current (innermost) loop index. An ambiguous condition
; exists if the loop control parameters are unavailable.

                HEADER  NORMAL
                db      1,"I"
I:
                tdc
                dec     a
                dec     a
                tcd
                lda     1,s
                sta     <1
                jmp     NEXT

; J ( -- n|u ) ( R: loop-sys1 loop-sys2 -- loop-sys1 loop-sys2 )
;
; n|u is a copy of the next-outer loop index. An ambiguous condition exists if
; the loop control parameters of the next-outer loop, loop-sys1, are
; unavailable.

                HEADER  NORMAL
                db      1,"J"
J:
                tdc
                dec     a
                dec     a
                tcd
                lda     5,s
                sta     <1
                jmp     NEXT

; R> ( -- x ) ( R: x -- )
;
; Move x from the return stack to the data stack.

                HEADER  NORMAL
                db      2,"R>"
R_FROM:
                tdc
                dec     a
                dec     a
                tcd
                pla                             ; Fetch return stack value
                sta     <1
                jmp     NEXT                    ; Done

; R@ ( -- x ) ( R: x -- x )
;
; Copy x from the return stack to the data stack.

                HEADER  NORMAL
                db      2,"R@"
R_FETCH:
                tdc
                dec     a
                dec     a
                tcd
                lda     1,s
                sta     <1
                jmp     NEXT

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
                lda     <3
                adc     <1
                sta     <3                      ; Save result
                tdc
                inc     a                       ; Clean up data stack
                inc     a
                tcd
                jmp     NEXT                    ; Done

; - ( n1|u1 n2|u2 -- n3|u3 )
;
; Subtract n2|u2 from n1|u1, giving the difference n3|u3.

                HEADER  NORMAL
                db      1,"-"
MINUS:
                sec                             ; Subtract top two values
                lda     <3
                sbc     <1
                sta     <3                      ; Save result
                tdc
                inc     a                       ; Clean up data stack
                inc     a
                tcd
                jmp     NEXT                    ; Done

; /
; /MOD

; 1+ ( n1|u1 -- n2|u2 )
;
; Add one (1) to n1|u1 giving the sum n2|u2.

                HEADER  NORMAL
                db      2,"1+"
ONE_PLUS:
                inc     <1                      ; Increment top of stack
                jmp     NEXT                    ; Done

; 1- ( n1|u1 -- n2|u2 )
;
; Subtract one (1) from n1|u1 giving the difference n2|u2.

                HEADER  NORMAL
                db      2,"1-"
ONE_MINUS:
                dec     <1                      ; Decrement top of stack
                jmp     NEXT                    ; Done

; 2* ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the most-significant bit,
; filling the vacated least-significant bit with zero.

                HEADER  NORMAL
                db      2,"2*"
TWO_STAR:
                asl     <1                      ; Multiply top value by two
                jmp     NEXT                    ; Done

; 2/ ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the least-significant bit,
; leaving the most-significant bit unchanged.

                HEADER  NORMAL
                db      2,"2/"
TWO_SLASH:
                lda     <1                      ; Load the top value
                rol     a                       ; Extract the top bit
                ror     <1                      ; And shift back into value
                jmp     NEXT

; ABS ( n -- u )
;
; u is the absolute value of n.

                HEADER  NORMAL
                db      3,"ABS"
ABS:
                lda     <1
                bmi     NEGATE
                jmp     NEXT                    ; Done

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
                sbc     <1
                sta     <1
                jmp     NEXT                    ; Done

; UMAX ( x1 x2 -- x1|x2 )

                HEADER  NORMAL
                db      4,"UMAX"
UMAX:
                lda     <1                      ; Compare the top values
                cmp     <3
                bcs     UMAX_EXIT               ; Is x2 biggest?
                jmp     DROP                    ; No, x1 is
UMAX_EXIT:      jmp     NIP

; UMIN ( x1 x2 -- x1|x2 )

                HEADER  NORMAL
                db      4,"UMIN"
UMIN:
                lda     <1                      ; Compare the top values
                cmp     <3
                bcc     UMIN_EXIT               ; Is x2 smallest?
                jmp     DROP                    ; No, x1 is
UMIN_EXIT:      jmp     NIP

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
                tdc
                inc     a                       ; Drop the high word
                inc     a
                tcd
                jmp     NEXT

; S>D ( n -- d )
;
; Convert the number n to the double-cell number d with the same numerical
; value.

                HEADER  NORMAL
                db      3,"S>D"
S_TO_D:
                tdc
                dec     a                       ; Assume n is positive
                dec     a
                tcd
                stz     <1                      ; .. push a zero value
                lda     <3                      ; Test the number
                bpl     S_TO_D_1
                dec     <1                      ; Make top -1 if negative
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
                lda     <1                      ; Test top of stack
                stz     <1                      ; Assume false result
                bpl     ZERO_LT_1               ; Was the value negative?
                dec     <1                      ; Yes, make true result
ZERO_LT_1:      jmp     NEXT                    ; Done

; 0<> ( x -- flag )
;
; flag is true if and only if x is not equal to zero.

                HEADER  NORMAL
                db      3,"0<>"
ZERO_NOT_EQUAL:
                lda     <1                      ; Test top of stack
                stz     <1                      ; Assume false result
                beq     ZERO_NE_1               ; Was the value non-zero?
                dec     <1                      ; Yes, make true result
ZERO_NE_1:      jmp     NEXT                    ; Done

; 0= ( x -- flag )
;
; flag is true if and only if x is equal to zero.

                HEADER  NORMAL
                db      2,"0="
ZERO_EQUAL:
                lda     <1                      ; Test top of stack
                stz     <1                      ; Assume false result
                bne     ZERO_EQ_1               ; Was the value zero?
                dec     <1                      ; Yes, make true result
ZERO_EQ_1:      jmp     NEXT                    ; Done

; 0> ( n -- flag )
;
; flag is true if and only if n is greater than zero.

                HEADER  NORMAL
                db      2,"0>"
ZERO_GREATER:
                lda     <1                      ; Test top of stack
                stz     <1                      ; Assume false result
                bmi     ZERO_GT_EXIT            ; Was the value positive?
                beq     ZERO_GT_EXIT            ; .. but not zero
                dec     <1                      ; Yes, make true result
ZERO_GT_EXIT:   jmp     NEXT                    ; Done

; <

; <>

                HEADER  NORMAL
                db      2,"<>"
NOT_EQUAL:
                ldx     <1                      ; Pull x2 from stack
                tdc
                inc     a
                inc     a
                tcd
                cpx     <1                      ; Compare with x1
                stz     <1                      ; Assume equal
                beq     NE_EXIT                 ; Test flags
                dec     <1                      ; Make result true
NE_EXIT:        jmp     NEXT                    ; Done

; = ( x1 x2 -- flag )
;
; flag is true if and only if x1 is bit-for-bit the same as x2.

                HEADER  NORMAL
                db      1,"="
EQUAL:
                ldx     <1                      ; Pull x2 from stack
                tdc
                inc     a
                inc     a
                tcd
                cpx     <1                      ; Compare with x1
                stz     <1                      ; Assume not equal
                bne     EQ_EXIT                 ; Test the flags
                inc     <1                      ; Make result true
EQ_EXIT:        jmp     NEXT                    ; Done

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
                lda     <1
                and     <3
                sta     <3
                tdc
                inc     a
                inc     a
                tcd
                jmp     NEXT

; INVERT ( x1 -- x2 )
;
; Invert all bits of x1, giving its logical inverse x2.

                HEADER  NORMAL
                db      6,"INVERT"
INVERT:
                lda     <1                      ; Fetch top value
                eor     #$ffff                  ; Invert all the bits
                sta     <1                      ; .. and write back
                jmp     NEXT                    ; Done

; LSHIFT ( x1 u -- x2 )
;
; Perform a logical left shift of u bit-places on x1, giving x2. Put zeroes
; into the least significant bits vacated by the shift. An ambiguous condition
; exists if u is greater than or equal to the number of bits in a cell.

                HEADER  NORMAL
                db      6,"LSHIFT"
LSHIFT:
                ldx     <1                      ; Pull bit count
                php
                tdc
                inc     a                       ; .. from the stack
                inc     a
                tcd
                plp
                beq     LSHIFT_0                ; Zero shift?
                cpx     #16                     ; Shifting by 16+ bits
                bcs     LSHIFT_2                ; Yes, result will be zero
LSHIFT_1        asl     <1                      ; Shift one bit left
                dex                             ; Update count
                bne     LSHIFT_1                ; .. and repeat as needed
LSHIFT_0        jmp     NEXT                    ; Done
LSHIFT_2        stz     <1                      ; Clear top value
                jmp     NEXT                    ; Done

; OR ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit inclusive-or of x1 with x2.

                HEADER  NORMAL
                db      2,"OR"
OR:
                lda     <1
                ora     <3
                sta     <3
                tdc
                inc     a
                inc     a
                tcd
                jmp     NEXT

; RSHIFT ( x1 u -- x2 )
;
; Perform a logical right shift of u bit-places on x1, giving x2. Put zeroes
; into the most significant bits vacated by the shift. An ambiguous condition
; exists if u is greater than or equal to the number of bits in a cell.

                HEADER  NORMAL
                db      6,"RSHIFT"
RSHIFT:
                ldx     <1                      ; Pull bit count
                php
                tdc
                inc     a                       ; .. from the stack
                inc     a
                tcd
                plp
                beq     RSHIFT_0                ; Zero shift?
                cpx     #16                     ; Shifting by 16+ bits
                bcs     RSHIFT_2                ; Yes, result will be zero
RSHIFT_1        lsr     <1                      ; Shift one bit left
                dex                             ; Update count
                bne     RSHIFT_1                ; .. and repeat as needed
RSHIFT_0        jmp     NEXT                    ; Done
RSHIFT_2        stz     <1                      ; Clear top value
                jmp     NEXT                    ; Done

; XOR ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit exclusive-or of x1 with x2.

                HEADER  NORMAL
                db      3,"XOR"
XOR:
                lda     <1
                eor     <3
                sta     <3
                tdc
                inc     a
                inc     a
                tcd
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
ABORT:          jsr     DO_COLON
                dw      DO_ABORT
                dw      QUIT

DO_ABORT:
                lda     #DSTACK_END-1
		tcd
                jmp     NEXT

; (BUILD) ( dtc-addr -- )
;
; Adds a jump the to exection function for the new word.

                HEADER  NORMAL
                db      7,"(BUILD)"
BUILD:          jsr     DO_COLON
                dw      DO_LITERAL,$4c,C_COMMA
                dw      COMMA,EXIT

; CREATE ( -- ) [TODO]
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below. If the data-
; space pointer is not aligned, reserve enough data space to align it. The new
; data-space pointer defines name’s data field. CREATE does not allocate data
; space in name’s data field.

                HEADER  NORMAL
                db      6,"CREATE"
CREATE:         jsr     DO_COLON
                ; parse
                dw      HERE,LATEST,FETCH,COMMA
                dw      ZERO,C_COMMA,LATEST,STORE
                ; move name
                dw      EXIT

; EXECUTE

                HEADER  NORMAL
                db      7,"EXECUTE"
EXECUTE:
                ldx     <1
                tdc
                inc     a
                inc     a
                tcd
                dex
                phx
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
QUIT:           jsr     DO_COLON
                dw      DO_QUIT
                dw      ZERO,STATE,STORE
                dw      ZERO,SOURCEID,STORE
QUIT_1:         dw      REFILL,QUERY_BRANCH,QUIT_2
                dw      INTERPRET
QUIT_2:         dw      STATE,FETCH,ZERO_EQUAL
                dw      QUERY_BRANCH,QUIT_3
                dw      DO_S_QUOTE
                db      2,"Ok"
                dw      TYPE
QUIT_3:         dw      BRANCH,QUIT_1

DO_QUIT:
                lda     #RSTACK_END-1           ; Reset the return stack
                tcs
                jmp     NEXT                    ; Done

;==============================================================================
; Parser & Interpreter
;------------------------------------------------------------------------------

; ?NUMBER

                HEADER  NORMAL
                db      7,"?NUMBER"
QUERY_NUMBER:   jsr     DO_COLON

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
;       DUP 8 = OVER 127 = OR IF
;             DROP 1-
;             >R OVER R> UMAX
;             8 EMIT SPACE 8 EMIT
;       ELSE
;           DUP EMIT    -- sa ea a c
;           OVER C! 1+ OVER UMIN
;       THEN            -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - ;

                HEADER  NORMAL
                db      6,"ACCEPT"
ACCEPT:         jsr     DO_COLON
                dw      OVER,PLUS,ONE_MINUS,OVER
ACCEPT_1:       dw      KEY,DUP,DO_LITERAL,$0D,NOT_EQUAL
                dw      QUERY_BRANCH,ACCEPT_4
                dw      DUP,DO_LITERAL,$08,EQUAL
                dw      OVER,DO_LITERAL,$7f,EQUAL,OR
                dw      QUERY_BRANCH,ACCEPT_2
                dw      DROP,ONE_MINUS
                dw      TO_R,OVER,R_FROM,UMAX
                dw      DO_LITERAL,8,EMIT,SPACE
                dw      DO_LITERAL,8,EMIT,BRANCH,ACCEPT_3
ACCEPT_2:       dw      DUP,EMIT
                dw      OVER,C_STORE,ONE_PLUS,OVER,UMIN
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
EVALUATE:       jsr     DO_COLON
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
INTERPRET:      jsr     DO_COLON
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
FIND:           jsr     DO_COLON



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
REFILL:         jsr     DO_COLON
                dw      SOURCE_ID,ZERO_EQUAL,QUERY_BRANCH,REFILL_1
                dw      TIB,DUP,HASH_TIB,FETCH,ACCEPT,SPACE
                dw      LENGTH,STORE,BUFFER,STORE
                dw      ZERO,TO_IN,STORE,TRUE,EXIT
REFILL_1:       dw      FALSE,EXIT

; RESTORE-INPUT

                HEADER  NORMAL
                db      13,"RESTORE-INPUT"
RESTORE_INPUT   jsr     DO_COLON
                dw      TO_IN,STORE
                dw      LENGTH,STORE
                dw      BUFFER,STORE
                dw      SOURCEID,STORE
                dw      TRUE,EXIT

; SAVE-INPUT

                HEADER  NORMAL
                db      10,"SAVE-INPUT"
SAVE_INPUT:     jsr     DO_COLON
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
SOURCE:         jsr     DO_COLON
                dw      BUFFER,FETCH
                dw      LENGTH,FETCH
                dw      EXIT

; SOURCE-ID ( -- 0 | -1 )
;
; Identifies the input source: -1 if string (via EVALUATE), 0 if user input
; device.

                HEADER  NORMAL
                db      9,"SOURCE-ID"
SOURCE_ID:      jsr     DO_COLON
                dw      SOURCEID,FETCH
                dw      EXIT

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
WORD:           jsr     DO_COLON
                dw      DUP,SOURCE,TO_IN,FETCH,SLASH_STRING
                dw      DUP,TO_R,ROT,SKIP
                dw      OVER,TO_R,ROT,SCAN

                ; TODO

                dw      EXIT

; SKIP ( c-addr n c == c-addr' n' )

SKIP:
SKIP_1:         lda     <3                      ; Any data left to skip over?
                beq     SKIP_2                  ; No.
                lda     <1                      ; Fetch and compare with skip
                short_a
                cmp     (5)
                long_a
                bne     SKIP_2                  ; Cannot be skipped
                inc     <5                      ; Bump data address
                dec     <3                      ; and update length
                bra     SKIP_1                  ; And repeat

SKIP_2:
                jmp     DROP                    ; Drop the character

; SKIP ( c-addr n c == c-addr' n' )

SCAN:           jsr     DO_COLON

                dw      EXIT

;==============================================================================
; String Words
;------------------------------------------------------------------------------

; -TRAILING

                HEADER  NORMAL
                db      9,"-TRAILING"
DASH_TRAILING:  jsr     DO_COLON

                dw      EXIT

; /STRING ( c-addr1 u1 n -- c-addr2 u2 )
;
; Adjust the character string at c-addr1 by n characters. The resulting
; character string, specified by c-addr2 u2, begins at c-addr1 plus n;
; characters and is u1 minus n characters long.
;
;   ROT OVER + ROT ROT -

                HEADER  NORMAL
                db      7,"/STRING"
SLASH_STRING:   jsr     DO_COLON
                dw      ROT,OVER,PLUS
                dw      ROT,ROT,MINUS
                dw      EXIT

; BLANK

; CMOVE ( c-addr1 c-addr2 u -- )
;
; If u is greater than zero, copy u consecutive characters from the data space
; starting at c-addr1 to that starting at c-addr2, proceeding character-by-
; character from lower addresses to higher addresses.

                HEADER  NORMAL
                db      5,"CMOVE"
CMOVE:
                phy                             ; Save IP
                phx                             ; Save stack pointer
                lda     <1                      ; Fetch length and save
                pha
                ldy     <3                      ; Fetch target address
                lda     <5                      ; Fetch source address
                tax
                pla                             ; Recover length
                beq     CMOVE_1                 ; .. No bytes to move
                dec     a                       ; Adjust count for MVN
                mvn     0,0                     ; Perform the move
CMOVE_1:        plx
                inx                             ; Clean up the stack
                inx
                inx
                inx
                inx
                inx
                ply                             ; Restore IP
                jmp     NEXT

; CMOVE> ( c-addr1 c-addr2 u -- )
;
; If u is greater than zero, copy u consecutive characters from the data space
; starting at c-addr1 to that starting at c-addr2, proceeding character-by-
; character from higher addresses to lower addresses.

                HEADER  NORMAL
                db      6,"CMOVE>"
CMOVE_GREATER:
                jmp     NEXT

; COMPARE

; SEARCH


;==============================================================================
; Compiling Words
;------------------------------------------------------------------------------

; +LOOP ( -- )

                HEADER  IMMEDIATE
                db      5,"+LOOP"
PLUS_LOOP:      jsr     DO_COLON

                dw      EXIT

; (+LOOP)

                HEADER  NORMAL
                db      7,"(+LOOP)"
DO_PLUS_LOOP:

                jmp     NEXT

; : ( -- )

                HEADER  NORMAL
                db      1,":"
COLON:          jsr     DO_COLON

                dw      EXIT

DO_COLON:
                plx
                phy
                inx
                txy

NEXT:
                tyx                             ; Copy IP to X
                iny
                iny
                jmp     (0,x)                   ; Then execute word

; AGAIN

                HEADER  IMMEDIATE
                dw      5,"AGAIN"
AGAIN:          jsr     DO_COLON

                dw      EXIT

; BEGIN

                HEADER  IMMEDIATE
                dw      5,"BEGIN"
BEGIN:          jsr     DO_COLON

                dw      EXIT

; CONSTANT ( x “<spaces>name” -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below.

                HEADER  NORMAL
                db      8,"CONSTANT"
CONSTANT:       jsr     DO_COLON
                dw      CREATE
                dw      DO_LITERAL,DO_CONSTANT,BUILD
                dw      COMMA,EXIT

; (CONSTANT) ( -- x )

                HEADER  NORMAL
                db      10,"(CONSTANT)"
DO_CONSTANT:
                plx
                tdc
                dec     a
                dec     a
                tcd
                lda     !1,x
                sta     <1
                jmp     NEXT                    ; Done

; DO ( -- )

                HEADER  IMMEDIATE
                db      2,"DO"
DO:             jsr     DO_COLON

                dw      EXIT

; (DO) ( -- )

                HEADER  NORMAL
                db      4,"(DO)"
DO_DO:
                lda     <3
                pha
                lda     <1
                pha
                tdc
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                jmp     NEXT

; ELSE

                HEADER  IMMEDIATE
                db      4,"ELSE"
ELSE:           jsr     DO_COLON

                dw      EXIT

; (BRANCH) ( -- )
;
; Cause the IP to be loaded with the word following the link to this word.

BRANCH:
                lda     !0,y                    ; Load branch address into IP
                tay
                jmp     NEXT                    ; Done

; IF

                HEADER  IMMEDIATE
                db      2,"IF"
IF:             jsr     DO_COLON

                dw      EXIT

; (?BRANCH) ( flag -- )
;
; If flag is false then cause the IP to be loaded with the word following the
; link to this word, otherwise skip over it.

QUERY_BRANCH:
                ldx     <1                      ; Pull the top of stack value
                php                             ; Save the flags
                tdc
                inc     a                       ; Drop top item
                inc     a
                tcd
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
LITERAL:        jsr     DO_COLON
                dw      DO_LITERAL,DO_LITERAL,COMMA
                dw      COMMA,EXIT

; (LITERAL) ( -- x )
;
; Place x on the stack.

                HEADER  NORMAL
                db      10,"(LITERAL)"
DO_LITERAL:
                tdc
                dec     a
                dec     a
                tcd
                lda     !0,y                    ; Fetch constant from IP
                iny
                iny
                sta     <1
                jmp     NEXT                    ; Done

; LOOP

                HEADER  IMMEDIATE
                db      4,"LOOP"
LOOP:           jsr     DO_COLON

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
                lda     !0,y                    ; No, branch back to start
                tay
                jmp     NEXT                    ; Done
DO_LOOP_END:    iny                             ; Skip over address
                iny
                pla                             ; Drop loop variables
                pla
                jmp     NEXT                    ; Done

; USER

                HEADER  NORMAL
                db      4,"USER"
USER:           jsr     DO_COLON

                dw      EXIT

                HEADER  NORMAL
                db      6,"(USER)"
DO_USER:
                tdc
                dec     a                       ; Push on data stack
                dec     a
                tcd
                plx
                clc
                lda     !1,x
                adc     #USER_AREA
                sta     <1
                jmp     NEXT                    ; Done

; VARIABLE

                HEADER  NORMAL
                db      8,"VARIABLE"
VARIABLE:       jsr     DO_COLON

                dw      EXIT

                HEADER  NORMAL
                db      10,"(VARIABLE)"
DO_VARIABLE:
                tdc
                dec     a
                dec     a
                tcd
                pla
                inc     a
                sta     <1
                jmp     NEXT

; S"

                HEADER  IMMEDIATE
                db      2,"S",'"'
S_QUOTE:

; (S") ( -- c-addr u )

DO_S_QUOTE:
                tdc
                dec     a                       ; Reserve space for values
                dec     a
                dec     a
                dec     a
                tcd
                short_a
                lda     !0,y                    ; Fetch the length
                sta     <1
                stz     <2
                long_a
                iny
                tya                             ; Save the text address
                sta     <3
                clc                             ; And update IP
                adc     <1
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
CR:             jsr     DO_COLON
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
                lda     <1                      ; Fetch character from stack
                jsr     UartTx                  ; .. and transmit
                tdc
                inc     a                       ; Drop the character
                inc     a
                tcd
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
                tax
                tdc
                dec     a                       ; And push to stack
                dec     a
                tcd
                stx     <1
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
SPACE:          jsr     DO_COLON
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
SPACES:         jsr     DO_COLON
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
TYPE:           jsr     DO_COLON
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