;===============================================================================
;     _    _   _ ____    _____          _   _       _  ___  _  __
;    / \  | \ | / ___|  |  ___|__  _ __| |_| |__   ( )( _ )/ |/ /_
;   / _ \ |  \| \___ \  | |_ / _ \| '__| __| '_ \  |/ / _ \| | '_ \
;  / ___ \| |\  |___) | |  _| (_) | |  | |_| | | |   | (_) | | (_) |
; /_/   \_\_| \_|____/  |_|  \___/|_|   \__|_| |_|    \___/|_|\___/
;
; A Direct Threaded ANS Forth for the WDC 65C816
;-------------------------------------------------------------------------------
; Copyright (C)2015-2016 HandCoded Software Ltd.
; All rights reserved.
;
; This work is made available under the terms of the Creative Commons
; Attribution-NonCommercial-ShareAlike 4.0 International license. Open the
; following URL to see the details.
;
; http://creativecommons.org/licenses/by-nc-sa/4.0/
;
;===============================================================================
; Notes:
;
; This implementation is designed to run in the 65C816's native mode with both
; the accumulator and index registers in 16-bit mode except when the word needs
; 8-bit memory access.
;
; The DP register is used for the Forth data stack is values can be accessed
; using the direct-page addressing modes. The code uses the same offsets as
; would be used with the stack relative instructions (i.e <1, <3, etc.).
;
; The Y register holds the forth instruction pointer leaving X free for general
; use in words. Some words push Y if they need an extra register.
;
; Some of the high-level definitions are based on Bradford J. Rodriguez's
; CamelForth implementations.
;
;-------------------------------------------------------------------------------

                pw      132
                inclist on
                maclist off

                chip    65816
                longi   off
                longa   off

                include "w65c816.inc"

;===============================================================================
; Macros
;-------------------------------------------------------------------------------

; The LINK macro deposits the link section of a word header automatically
; linking the new word to the last.

WORDZ           set     0                       ; Word counter
WORD0           equ     0                       ; Null address for first word

LINK            macro   TYPE
                dw      WORD@<WORDZ>            ; Link
                db      TYPE                    ; Type
WORDZ           set     WORDZ+1
WORD@<WORDZ>:
                endm

; Deposits a word header containing the name which is linked back to the
; previous word.
;
; The WDC assembler does not handle string parameters to macros very well,
; stopping at the first comma or space in them, so some headers must be
; manually constructed.

NORMAL          equ     $00
IMMEDIATE       equ     $80

HEADER          macro   LEN,NAME,TYPE
                LINK    TYPE
                db      LEN,NAME
                endm

; The CONTINUE macro is used at the end of a native word to invoke the next
; word pointer.

CONTINUE        macro
                tyx                             ; Copy IP to X
                iny
                iny
                jmp     (0,x)                   ; Then execute word
                endm

TRAILER         macro
LAST_WORD       equ     WORD@<WORDZ>
                endm

;===============================================================================
; Definitions
;-------------------------------------------------------------------------------

USER_SIZE       equ     22
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
HP_OFFSET       equ     20

TIB_SIZE        equ     128
PAD_SIZE        equ     48

;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

                page0
                org     $00

USER_AREA       ds      USER_SIZE               ; User Variables


DSTACK_START    equ     $0100
DSTACK_END      equ     DSTACK_START+DSTACK_SIZE

RSTACK_START    equ     $0180
RSTACK_END      equ     RSTACK_START+RSTACK_SIZE


                data
                org     $0200

TIB_AREA:       ds      TIB_SIZE                ; Terminal Input Buffer
                ds      PAD_SIZE                ; Pad area
PAD_AREA:       ds      0

;===============================================================================
; Forth Entry Point
;-------------------------------------------------------------------------------

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
                CONTINUE

COLD:
                dw      DECIMAL
                dw      ZERO
                dw      BLK
                dw      STORE
                dw      FALSE
                dw      STATE
                dw      STORE
                dw      DO_LITERAL,NEXT_WORD
                dw      DP
                dw      STORE
                dw      DO_LITERAL,LAST_WORD
                dw      LATEST
                dw      STORE
                dw      CR
                dw      CR
                dw      DO_TITLE
                dw      TYPE
                dw      CR
                dw      CR
                dw      ABORT

;===============================================================================
; System/User Variables
;-------------------------------------------------------------------------------

; #TIB ( -- a-addr )
;
; a-addr is the address of a cell containing the number of characters in the
; terminal input buffer.

                HEADER  4,"#TIB",NORMAL
HASH_TIB:       jsr     DO_CONSTANT
                dw      $+2
                dw      TIB_SIZE-2

; >IN ( -- a-addr )
;
; a-addr is the address of a cell containing the offset in characters from the
; start of the input buffer to the start of the parse area.

                HEADER  3,">IN",NORMAL
TO_IN:          jsr     DO_USER
                dw      TO_IN_OFFSET

; BASE ( -- a-addr )
;
; a-addr is the address of a cell containing the current number-conversion
; radix {{2...36}}.

                HEADER  4,"BASE",NORMAL
BASE:           jsr     DO_USER
                dw      BASE_OFFSET

; BLK ( -- a-addr )
;
; a-addr is the address of a cell containing zero or the number of the mass-
; storage block being interpreted. If BLK contains zero, the input source is
; not a block and can be identified by SOURCE-ID, if SOURCE-ID is available. An
; ambiguous condition exists if a program directly alters the contents of BLK.

                HEADER  3,"BLK",NORMAL
BLK:            jsr     DO_USER
                dw      BLK_OFFSET

; (BUFFER)

BUFFER:         jsr     DO_USER
                dw      BUFFER_OFFSET

; DP ( -- a-addr )
;
; Dictionary Pointer

                HEADER  2,"DP",NORMAL
DP:             jsr     DO_USER
                dw      DP_OFFSET

; HP ( -- a-addr )
;
; Hold Pointer

HP:             jsr     DO_USER
                dw      HP_OFFSET

; LATEST ( -- a-addr )

                HEADER  6,"LATEST",NORMAL
LATEST:         jsr     DO_USER
                dw      LATEST_OFFSET

; (LENGTH)

LENGTH:         jsr     DO_USER
                dw      LENGTH_OFFSET

; SCR ( -- a-addr )
;
; a-addr is the address of a cell containing the block number of the block most
; recently LISTed.

                HEADER  3,"SCR",NORMAL
SCR:            jsr     DO_USER
                dw      SCR_OFFSET

; (SOURCE-ID)

SOURCEID:       jsr     DO_USER
                dw      SOURCEID_OFFSET

; STATE ( -- a-addr )
;
; a-addr is the address of a cell containing the compilation-state flag. STATE
; is true when in compilation state, false otherwise. The true value in STATE
; is non-zero, but is otherwise implementation-defined.

                HEADER  5,"STATE",NORMAL
STATE:          jsr     DO_USER
                dw      STATE_OFFSET

; TIB ( -- c-addr )
;
; c-addr is the address of the terminal input buffer.

                HEADER  3,"TIB",NORMAL
TIB:            jsr     DO_CONSTANT
                dw      TIB_AREA

;===============================================================================
; Constants
;-------------------------------------------------------------------------------

; 0 ( -- 0 )
;
; Push the constant value zero on the stack

                HEADER  1,"0",NORMAL
ZERO:
                tdc
                dec     a                       ; Make space on the stack
                dec     a
                tcd
                stz     <1                      ; And create a zero value
                CONTINUE                        ; Done

; BL ( -- char )
;
; char is the character value for a space.

                HEADER  2,"BL",NORMAL
BL:
                tdc
                dec     a                       ; Make space on the stack
                dec     a
                tcd
                lda     #' '                    ; And save a space value
                sta     <1
                CONTINUE                        ; Done

; FALSE ( -- false )
;
; Return a false flag.

                HEADER  5,"FALSE",NORMAL
FALSE:
                tdc
                dec     a                       ; Make space on the stack
                dec     a
                tcd
                stz     <1                      ; And create a false value
                CONTINUE                        ; Done

; TRUE ( -- true )
;
; Return a true flag, a single-cell value with all bits set.

                HEADER  4,"TRUE",NORMAL
TRUE:
                tdc
                dec     a                       ; Make space on the stack
                dec     a
                tcd
                stz     <1                      ; And create a true value
                dec     <1
                CONTINUE                        ; Done

;===============================================================================
; Radix
;-------------------------------------------------------------------------------

; DECIMAL ( -- )
;
; Set the numeric conversion radix to ten (decimal).

                HEADER  7,"DECIMAL",NORMAL
DECIMAL:        jsr     DO_COLON
                dw      DO_LITERAL,10
                dw      BASE
                dw      STORE
                dw      EXIT

; HEX ( -- )
;
; Set contents of BASE to sixteen.

                HEADER  3,"HEX",NORMAL
HEX:            jsr     DO_COLON
                dw      DO_LITERAL,16
                dw      BASE
                dw      STORE
                dw      EXIT

;===============================================================================
; Memory Operations
;-------------------------------------------------------------------------------

; ! ( x a-addr -- )
;
; Store x at a-addr.

                HEADER  1,"!",NORMAL
STORE:
                lda     <3                      ; Fetch data value
                sta     (1)                     ; .. and store
                tdc                             ; Clean up data stack
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                CONTINUE                        ; Done

; +! ( n|u a-addr -- )
;
; Add n|u to the single-cell number at a-addr.

                HEADER  2,"+!",NORMAL
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
                CONTINUE                        ; Done

; , ( x -- )
;
; Reserve one cell of data space and store x in the cell. If the data-space
; pointer is aligned when , begins execution, it will remain aligned when ,
; finishes execution. An ambiguous condition exists if the data-space pointer
; is not aligned prior to execution of ,.
;
; In this implementation is its defined as:
;
;   HERE ! 1 CELLS ALLOT

                LINK    NORMAL
                db      1,","
COMMA:          jsr     DO_COLON
                dw      HERE
                dw      STORE
                dw      DO_LITERAL,1
                dw      CELLS
                dw      ALLOT
                dw      EXIT

; 2! ( x1 x2 a-addr -- )
;
; Store the cell pair x1 x2 at a-addr, with x2 at a-addr and x1 at the next
; consecutive cell.
;
; In this implementation is its defined as:
;
;   SWAP OVER ! CELL+ !.

                HEADER  2,"2!",NORMAL
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
; the next consecutive cell.
;
; In this implementation is its defined as:
;
;   DUP CELL+ @ SWAP @

                HEADER  2,"2@",NORMAL
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

                HEADER  1,"@",NORMAL
FETCH:
                lda     (1)                     ; Fetch from memory
                sta     <1                      ; .. and replace top value
                CONTINUE                        ; Done

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
ALLOT:          jsr     DO_COLON
                dw      DP
                dw      PLUS_STORE
                dw      EXIT

; C! ( char c-addr -- )
;
; Store char at c-addr. When character size is smaller than cell size, only the
; number of low-order bits corresponding to character size are transferred.

                HEADER  2,"C!",NORMAL
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
                CONTINUE                        ; Done

; C, ( char -- )
;
; Reserve space for one character in the data space and store char in the
; space. If the data-space pointer is character aligned when C, begins
; execution, it will remain character aligned when C, finishes execution.
; An ambiguous condition exists if the data-space pointer is not character-
; aligned prior to execution of C,
;
;   HERE C! 1 CHARS ALLOT

                LINK    NORMAL
                db      2,"C,"
C_COMMA:        jsr     DO_COLON
                dw      HERE
                dw      C_STORE
                dw      DO_LITERAL,1
                dw      CHARS
                dw      ALLOT
                dw      EXIT

; C@ ( c-addr -- char )
;
; Fetch the character stored at c-addr. When the cell size is greater than
; character size, the unused high-order bits are all zeroes.

                HEADER  2,"C@",NORMAL
C_FETCH:
                short_a
                lda     (1)                     ; Fetch the data byte
                sta     <1                      ; .. and replace stack value
                stz     <2
                long_a
                CONTINUE                        ; Done

; HERE ( -- addr )
;
; addr is the data-space pointer.

                HEADER  4,"HERE",NORMAL
HERE:           jsr     DO_COLON
                dw      DP
                dw      FETCH
                dw      EXIT

;===============================================================================
; Alignment
;-------------------------------------------------------------------------------

; ALIGN ( -- )
;
; If the data-space pointer is not aligned, reserve enough space to align it.

                HEADER  5,"ALIGN",NORMAL
ALIGN:
                CONTINUE                        ; Done

; ALIGNED ( addr -- a-addr )
;
; a-addr is the first aligned address greater than or equal to addr.

                HEADER  7,"ALIGNED",NORMAL
ALIGNED:
                CONTINUE                        ; Done

; CELL+ ( a-addr1 -- a-addr2 )
;
; Add the size in address units of a cell to a-addr1, giving a-addr2.

                HEADER  5,"CELL+",NORMAL
CELL_PLUS:
                inc     <1                      ; Bump the address by two
                inc     <1
                CONTINUE                        ; Done

; CELLS ( n1 -- n2 )
;
; n2 is the size in address units of n1 cells.

                HEADER  5,"CELLS",NORMAL
CELLS:
                asl     <1                      ; Two bytes per cell
                CONTINUE                        ; Done

; CHAR+ ( c-addr1 -- c-addr2 )
;
; Add the size in address units of a character to c-addr1, giving c-addr2.

                HEADER  5,"CHAR+",NORMAL
CHAR_PLUS:
                inc     <1                      ; Bump the address by one
                CONTINUE                        ; Done

; CHAR- ( c-addr1 -- c-addr2 )
;
; Subtract the size in address units of a character to c-addr1, giving c-addr2.

                HEADER  5,"CHAR-",NORMAL
CHAR_MINUS:
                dec     <1
                CONTINUE                        ; Done

; CHARS ( n1 -- n2 )
;
; n2 is the size in address units of n1 characters.

                HEADER  5,"CHARS",NORMAL
CHARS:
                CONTINUE                        ; Done

;===============================================================================
; Stack Operations
;-------------------------------------------------------------------------------

; 2DROP ( x1 x2 -- )
;
; Drop cell pair x1 x2 from the stack.

                HEADER  5,"2DROP",NORMAL
TWO_DROP:
                tdc                             ; Removed two words from stack
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                CONTINUE                        ; Done

; 2DUP ( x1 x2 -- x1 x2 x1 x2 )
;
; Duplicate cell pair x1 x2.

                HEADER  4,"2DUP",NORMAL
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
                CONTINUE                        ; Done

; 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
;
; Copy cell pair x1 x2 to the top of the stack.

                HEADER  5,"2OVER",NORMAL
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
                CONTINUE                        ; Done

; 2ROT ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
;
; Rotate the top three cell pairs on the stack bringing cell pair x1 x2 to
; the top of the stack.

                HEADER  4,"2ROT",NORMAL
TWO_ROT:        jsr     DO_COLON
                lda     <11                     ; Save x1
                pha
                lda     <9                      ; Save x2
                pha
                lda     <7                      ; Move x3
                sta     <11
                lda     <5                      ; Move x4
                sta     <9
                lda     <3                      ; Move x5
                sta     <7
                lda     <1                      ; Move x6
                sta     <5
                pla                             ; Restore x2
                sta     <1
                pla                             ; Restore x1
                sta     <3
                CONTINUE                        ; Done

; 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
;
; Exchange the top two cell pairs.

                HEADER  5,"2SWAP",NORMAL
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
                CONTINUE                        ; Done

; ?DUP ( x -- 0 | x x )
;
; Duplicate x if it is non-zero.

                HEADER  4,"?DUP",NORMAL
QUERY_DUP:
                lda     <1                      ; Fetch top value
                bne     DUP                     ; Non-zero value?
                CONTINUE                        ; Done

; DEPTH ( -- +n )
;
; +n is the number of single-cell values contained in the data stack before +n
; was placed on the stack.

                HEADER  5,"DEPTH",NORMAL
DEPTH:          jsr     DO_COLON
                dw      AT_DP
                dw      DO_LITERAL,DSTACK_END-1
                dw      SWAP
                dw      MINUS
                dw      TWO_SLASH
                dw      EXIT

; DROP ( x -- )
;
; Remove x from the stack.

                HEADER  4,"DROP",NORMAL
DROP:
                tdc                             ; Drop the top value
                inc     a
                inc     a
                tcd
                CONTINUE                        ; Done

; DUP ( x -- x x )
;
; Duplicate x.

                HEADER  3,"DUP",NORMAL
DUP:
                tdc
                dec     a
                dec     a
                tcd
                lda     <3                      ; Fetch top value
                sta     <1                      ; And make a copy
                CONTINUE                        ; Done

; NIP ( x1 x2 -- x2 )
;
; Drop the first item below the top of stack.

                HEADER  3,"NIP",NORMAL
NIP:
                lda     <1                      ; Copy x2 over x1
                sta     <3
                bra     DROP

; OVER ( x1 x2 -- x1 x2 x1 )
;
; Place a copy of x1 on top of the stack.

                HEADER  4,"OVER",NORMAL
OVER:
                tdc
                dec     a
                dec     a
                tcd
                lda     <5                      ; Fetch second value
                sta     <1                      ; And make a copy
                CONTINUE                        ; Done

; PICK ( xu ... x1 x0 u -- xu ... x1 x0 xu )
;
; Remove u. Copy the xu to the top of the stack. An ambiguous condition exists
; if there are less than u+2 items on the stack before PICK is executed.

                HEADER  4,"PICK",NORMAL
PICK:
                lda     <1                      ; Fetch the index
                asl     a
                tax
                lda     <3,x                    ; Load the target value
                sta     <1                      ; .. and save
                CONTINUE                        ; Done

; ROLL ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
;
; Remove u. Rotate u+1 items on the top of the stack. An ambiguous condition
; exists if there are less than u+2 items on the stack before ROLL is executed.

                HEADER  4,"ROLL",NORMAL
ROLL:
                asl     <1                      ; Convert count to index
                ldx     <1
                beq     ROLL_2                  ; Zero? Nothing to do
                lda     <3,x                    ; Save the final value
                pha
ROLL_1:         lda     <1,x                    ; Move x-1 to x
                sta     <3,x
                dex                             ; And repeat
                dex
                bne     ROLL_1
                pla                             ; Recover the new top value
                sta     <3
ROLL_2:         jmp     DROP                    ; Drop the count

; ROT ( x1 x2 x3 -- x2 x3 x1 )
;
; Rotate the top three stack entries.

                HEADER  3,"ROT",NORMAL
ROT:
                ldx     <5                      ; Save x1
                lda     <3                      ; Move x2
                sta     <5
                lda     <1                      ; Move x3
                sta     <3
                stx     <1                      ; Restore x1
                CONTINUE

; SWAP ( x1 x2 -- x2 x1 )
;
; Exchange the top two stack items.

                HEADER  4,"SWAP",NORMAL
SWAP:
                lda     <1                      ; Switch top two words
                ldx     <3
                sta     <3
                stx     <1
                CONTINUE                        ; Done

; TUCK ( x1 x2 -- x2 x1 x2 )
;
; Copy the first (top) stack item below the second stack item.

                HEADER  4,"TUCK",NORMAL
TUCK:           jsr     DO_COLON
                dw      SWAP
                dw      OVER
                dw      EXIT

;===============================================================================
; Return Stack Operations
;-------------------------------------------------------------------------------

; 2>R ( x1 x2 -- ) ( R: -- x1 x2 )
;
; Transfer cell pair x1 x2 to the return stack. Semantically equivalent to
; SWAP >R >R.

                HEADER  3,"2>R",NORMAL
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
                CONTINUE                        ; Done

; 2R> ( -- x1 x2 ) ( R: x1 x2 -- )
;
; Transfer cell pair x1 x2 from the return stack. Semantically equivalent to R>
; R> SWAP.

                HEADER  3,"2R>",NORMAL
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
                CONTINUE                        ; Done

; 2R@ ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
;
; Copy cell pair x1 x2 from the return stack. Semantically equivalent to R> R>
; 2DUP >R >R SWAP.

                HEADER  3,"2R@",NORMAL
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
                CONTINUE                        ; Done

; >R ( x -- ) ( R: -- x )
;
; Move x to the return stack.

                HEADER  2,">R",NORMAL
TO_R:
                lda     <1                      ; Transfer top value
                pha                             ; .. to return stack
                tdc
                inc     a
                inc     a
                tcd
                CONTINUE                        ; Done

; I ( -- n|u ) ( R: loop-sys -- loop-sys )
;
; n|u is a copy of the current (innermost) loop index. An ambiguous condition
; exists if the loop control parameters are unavailable.

                HEADER  1,"I",NORMAL
I:
                tdc
                dec     a
                dec     a
                tcd
                lda     1,s
                sta     <1
                CONTINUE

; J ( -- n|u ) ( R: loop-sys1 loop-sys2 -- loop-sys1 loop-sys2 )
;
; n|u is a copy of the next-outer loop index. An ambiguous condition exists if
; the loop control parameters of the next-outer loop, loop-sys1, are
; unavailable.

                HEADER  1,"J",NORMAL
J:
                tdc
                dec     a
                dec     a
                tcd
                lda     5,s
                sta     <1
                CONTINUE

; R> ( -- x ) ( R: x -- )
;
; Move x from the return stack to the data stack.

                HEADER  2,"R>",NORMAL
R_FROM:
                tdc
                dec     a
                dec     a
                tcd
                pla                             ; Fetch return stack value
                sta     <1
                CONTINUE                        ; Done

; R@ ( -- x ) ( R: x -- x )
;
; Copy x from the return stack to the data stack.

                HEADER  2,"R@",NORMAL
R_FETCH:
                tdc
                dec     a
                dec     a
                tcd
                lda     1,s
                sta     <1
                CONTINUE

;===============================================================================
; Single Precision Arithmetic
;-------------------------------------------------------------------------------

; * ( n1|u1 n2|u2 -- n3|u3 )
;
; Multiply n1|u1 by n2|u2 giving the product n3|u3.
;
; In this implementation it is defined as:
;
;   M* DROP

                HEADER  1,"*",NORMAL
STAR:           jsr     DO_COLON
                dw      M_STAR
                dw      DROP
                dw      EXIT

; */ ( n1 n2 n3 -- n4 )
;
; Multiply n1 by n2 producing the intermediate double-cell result d. Divide d
; by n3 giving the single-cell quotient n4. An ambiguous condition exists if
; n3 is zero or if the quotient n4 lies outside the range of a signed number.
; If d and n3 differ in sign, the implementation-defined result returned will
; be the same as that returned by either the phrase >R M* R> FM/MOD SWAP DROP
; or the phrase >R M* R> SM/REM SWAP DROP.
;
; In this implementation it is defined as:
;
;   >R M* R> FM/MOD SWAP DROP

                HEADER  2,"*/",NORMAL
STAR_SLASH:     jsr     DO_COLON
                dw      TO_R
                dw      M_STAR
                dw      R_FROM
                dw      FM_SLASH_MOD
                dw      SWAP
                dw      DROP
                dw      EXIT

; */MOD ( n1 n2 n3 -- n4 n5 )
;
; Multiply n1 by n2 producing the intermediate double-cell result d. Divide d
; by n3 producing the single-cell remainder n4 and the single-cell quotient n5.
; An ambiguous condition exists if n3 is zero, or if the quotient n5 lies
; outside the range of a single-cell signed integer. If d and n3 differ in
; sign, the implementation-defined result returned will be the same as that
; returned by either the phrase >R M* R> FM/MOD or the phrase >R M* R> SM/REM.
;
; In this implementation it is defined as:
;
;   >R M* R> FM/MOD

                HEADER  5,"*/MOD",NORMAL
STAR_SLASH_MOD: jsr     DO_COLON
                dw      TO_R
                dw      M_STAR
                dw      R_FROM
                dw      FM_SLASH_MOD
                dw      EXIT

; + ( n1|u1 n2|u2 -- n3|u3 )
;
; Add n2|u2 to n1|u1, giving the sum n3|u3.

                HEADER  1,"+",NORMAL
PLUS:
                clc                             ; Add top two values
                lda     <3
                adc     <1
                sta     <3                      ; Save result
                tdc
                inc     a                       ; Clean up data stack
                inc     a
                tcd
                CONTINUE                        ; Done

; - ( n1|u1 n2|u2 -- n3|u3 )
;
; Subtract n2|u2 from n1|u1, giving the difference n3|u3.

                HEADER  1,"-",NORMAL
MINUS:
                sec                             ; Subtract top two values
                lda     <3
                sbc     <1
                sta     <3                      ; Save result
                tdc
                inc     a                       ; Clean up data stack
                inc     a
                tcd
                CONTINUE                        ; Done

; / ( n1 n2 -- n3 )
;
; Divide n1 by n2, giving the single-cell quotient n3. An ambiguous condition
; exists if n2 is zero. If n1 and n2 differ in sign, the implementation-defined
; result returned will be the same as that returned by either the phrase >R S>D
; R> FM/MOD SWAP DROP or the phrase >R S>D R> SM/REM SWAP DROP.
;
; In this implementatio it is defined as:
;
;   >R S>D R> FM/MOD SWAP DROP

                HEADER  1,"/",NORMAL
SLASH:          jsr     DO_COLON
                dw      TO_R
                dw      S_TO_D
                dw      R_FROM
                dw      FM_SLASH_MOD
                dw      SWAP
                dw      DROP
                dw      EXIT

; /MOD ( n1 n2 -- n3 n4 )
;
; Divide n1 by n2, giving the single-cell remainder n3 and the single-cell
; quotient n4. An ambiguous condition exists if n2 is zero. If n1 and n2 differ
; in sign, the implementation-defined result returned will be the same as that
; returned by either the phrase >R S>D R> FM/MOD or the phrase >R S>D R> SM/REM.
;
; In this implementation it is defined as:
;
;   >R S>D R> FM/MOD

                HEADER  4,"/MOD",NORMAL
SLASH_MOD:      jsr     DO_COLON
                dw      TO_R
                dw      S_TO_D
                dw      R_FROM
                dw      FM_SLASH_MOD
                dw      EXIT

; 1+ ( n1|u1 -- n2|u2 )
;
; Add one (1) to n1|u1 giving the sum n2|u2.

                HEADER  2,"1+",NORMAL
ONE_PLUS:
                inc     <1                      ; Increment top of stack
                CONTINUE                        ; Done

; 1- ( n1|u1 -- n2|u2 )
;
; Subtract one (1) from n1|u1 giving the difference n2|u2.

                HEADER  2,"1-",NORMAL
ONE_MINUS:
                dec     <1                      ; Decrement top of stack
                CONTINUE                        ; Done

; 2* ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the most-significant bit,
; filling the vacated least-significant bit with zero.

                HEADER  2,"2*",NORMAL
TWO_STAR:
                asl     <1                      ; Multiply top value by two
                CONTINUE                        ; Done

; 2/ ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the least-significant bit,
; leaving the most-significant bit unchanged.

                HEADER  2,"2/",NORMAL
TWO_SLASH:
                lda     <1                      ; Load the top value
                rol     a                       ; Extract the top bit
                ror     <1                      ; And shift back into value
                CONTINUE

; ?NEGATE ( x sign -- x/-x)
;
; If the sign value is negative then negate the value of x to match.
;
; In this implementation it is defined as:
;
;   0< IF NEGATE THEN

QUERY_NEGATE:   jsr     DO_COLON
                dw      ZERO_LESS
                dw      QUERY_BRANCH,QUERY_NEGATE_1
                dw      NEGATE
QUERY_NEGATE_1: dw      EXIT

; ABS ( n -- u )
;
; u is the absolute value of n.

                HEADER  3,"ABS",NORMAL
ABS:
                lda     <1
                bpl     ABS_1
                jmp     NEGATE
ABS_1:          CONTINUE                        ; Done

; FM/MOD ( n1 n2 -- n3 n4 )
;
; Divide n1 by n2, giving the single-cell remainder n3 and the single-cell
; quotient n4. An ambiguous condition exists if n2 is zero. If n1 and n2 differ
; in sign, the implementation-defined result returned will be the same as that
; returned by either the phrase >R S>D R> FM/MOD or the phrase >R S>D R> SM/REM.
;
; In this implementation it is defined as:
;
;   DUP >R                      divisor
;   2DUP XOR >R                 sign of quotient
;   >R                          divisor
;   DABS R@ ABS UM/MOD
;   SWAP R> ?NEGATE SWAP        apply sign to remainder
;   R> 0< IF                    if quotient negative,
;       NEGATE
;       OVER IF                 if remainder nonzero,
;       R@ ROT - SWAP 1-        adjust rem,quot
;       THEN
;   THEN  R> DROP ;

                HEADER  6,"FM/MOD",NORMAL
FM_SLASH_MOD:   jsr     DO_COLON
                dw      DUP
                dw      TO_R
                dw      TWO_DUP
                dw      XOR
                dw      TO_R
                dw      TO_R
                dw      DABS
                dw      R_FETCH
                dw      ABS
                dw      UM_SLASH_MOD
                dw      SWAP
                dw      R_FROM
                dw      QUERY_NEGATE
                dw      SWAP
                dw      R_FROM
                dw      ZERO_LESS
                dw      QUERY_BRANCH,FM_SLASH_MOD_1
                dw      NEGATE
                dw      OVER
                dw      QUERY_BRANCH,FM_SLASH_MOD_1
                dw      R_FETCH
                dw      ROT
                dw      MINUS
                dw      SWAP
                dw      ONE_MINUS
FM_SLASH_MOD_1: dw      R_FROM
                dw      DROP
                dw      EXIT

; MAX ( n1 n2 -- n3 )
;
; n3 is the greater of n1 and n2.

                HEADER  3,"MAX",NORMAL
MAX:            jsr     DO_COLON
                dw      TWO_DUP
                dw      LESS
                dw      QUERY_BRANCH,MAX_1
                dw      SWAP
MAX_1:          dw      DROP
                dw      EXIT

; MIN ( n1 n2 -- n3 )
;
; n3 is the lesser of n1 and n2.

                HEADER  3,"MIN",NORMAL
MIN:            jsr     DO_COLON
                dw      TWO_DUP
                dw      GREATER
                dw      QUERY_BRANCH,MIN_1
                dw      SWAP
MIN_1:          dw      DROP
                dw      EXIT

; MOD ( n1 n2 -- n3 )
;
; Divide n1 by n2, giving the single-cell remainder n3. An ambiguous condition
; exists if n2 is zero. If n1 and n2 differ in sign, the implementation-defined
; result returned will be the same as that returned by either the phrase >R S>D
; R> FM/MOD DROP or the phrase >R S>D R> SM/REM DROP.
;
; In this implementation it is defined as:
;
;   >R S>D R> FM/MOD DROP

                HEADER  3,"MOD",NORMAL
MOD:            jsr     DO_COLON
                dw      TO_R
                dw      S_TO_D
                dw      R_FROM
                dw      FM_SLASH_MOD
                dw      DROP
                dw      EXIT

; NEGATE ( n1 -- n2 )
;
; Negate n1, giving its arithmetic inverse n2.

                HEADER  6,"NEGATE",NORMAL
NEGATE:
                sec                             ; Negate the top of stack
                lda     #0
                sbc     <1
                sta     <1
                CONTINUE                        ; Done

; UMAX ( x1 x2 -- x3 )
;
; x3 is the greater of x1 and x2.

                HEADER  4,"UMAX",NORMAL
UMAX:
                lda     <1                      ; Compare the top values
                cmp     <3
                bcs     UMAX_EXIT               ; Is x2 biggest?
                jmp     DROP                    ; No, x1 is
UMAX_EXIT:      jmp     NIP

; UMIN ( x1 x2 -- x3 )
;
; x3 is the lesser of x1 and x2.

                HEADER  4,"UMIN",NORMAL
UMIN:
                lda     <1                      ; Compare the top values
                cmp     <3
                bcc     UMIN_EXIT               ; Is x2 smallest?
                jmp     DROP                    ; No, x1 is
UMIN_EXIT:      jmp     NIP

;===============================================================================
; Double Precision Arithmetic
;-------------------------------------------------------------------------------

; ?DNEGATE ( d1 sign -- d1/-d1 )
;
; If sign is less than zero than negate d1 otherwise leave it unchanged.

QUERY_DNEGATE:  jsr     DO_COLON
                dw      ZERO_LESS
                dw      QUERY_BRANCH,QUERY_DNEG_1
                dw      DNEGATE
QUERY_DNEG_1:   dw      EXIT

; D+ ( d1|ud1 d2|ud2 -- d3|ud3 )
;
; Add d2|ud2 to d1|ud1, giving the sum d3|ud3.

                HEADER  2,"D+",NORMAL
D_PLUS:
                clc
                lda     <7                      ; Add low words
                adc     <3
                sta     <7
                lda     <5                      ; Then the high words
                adc     <1
                sta     <5
                tdc                             ; Drop top double
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                CONTINUE                        ; Done

; D- ( d1|ud1 d2|ud2 -- d3|ud3 )
;
; Subtract d2|ud2 from d1|ud1, giving the difference d3|ud3.

                HEADER  2,"D-",NORMAL
D_MINUS:
                sec
                lda     <7                      ; Subtract low words
                sbc     <3
                sta     <7
                lda     <5                      ; Then the high words
                sbc     <1
                sta     <5
                tdc                             ; Drop top double
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                CONTINUE                        ; Done

; D0< ( d -- flag )
;
; flag is true if and only if d is less than zero.

                HEADER  3,"D0<",NORMAL
D_ZERO_LESS:
                ldx     <1                      ; Fetch sign
                tdc                             ; Drop a word
                inc     a
                inc     a
                tcd
                stz     <1                      ; Assume false
                txa
                bpl     D_ZERO_LESS_1
                dec     <1
D_ZERO_LESS_1:  CONTINUE

; D0= ( d -- flag )
;
; flag is true if and only if d is equal to zero.

                HEADER  3,"D0=",NORMAL
D_ZERO_EQUAL:
                ldx     <1                      ; Fetch sign
                tdc                             ; Drop a word
                inc     a
                inc     a
                tcd
                stz     <1                      ; Assume false
                txa
                bne     D_ZERO_EQUAL_1
                dec     <1
D_ZERO_EQUAL_1: CONTINUE

; D2* ( xd1 -- xd2 )
;
; xd2 is the result of shifting xd1 one bit toward the most-significant bit,
; filling the vacated least-significant bit with zero.

                HEADER  3,"D2*",NORMAL
D_TWO_STAR:
                asl     <3
                rol     <1
                CONTINUE

; D2/ ( xd1 -- xd2 )
;
; xd2 is the result of shifting xd1 one bit toward the least-significant bit,
; leaving the most-significant bit unchanged.

                HEADER  3,"D2/",NORMAL
D_TWO_SLASH:
                lda     <1
                rol     a
                ror     <1
                ror     <3
                CONTINUE

; D< ( d1 d2 -- flag )
;
; flag is true if and only if d1 is less than d2.

                HEADER  2,"D<",NORMAL
D_LESS:         jsr     DO_COLON
                dw      D_MINUS
                dw      D_ZERO_LESS
                dw      EXIT

; D= ( d1 d2 -- flag )
;
; flag is true if and only if d1 is bit-for-bit the same as d2.

                HEADER  2,"D=",NORMAL
D_EQUAL:        jsr     DO_COLON
                dw      D_MINUS
                dw      D_ZERO_EQUAL
                dw      EXIT

; DABS ( d -- ud )
;
; ud is the absolute value of d.

                HEADER  4,"DABS",NORMAL
DABS:
                lda     <1
                bpl     DABS_1
                jmp     DNEGATE
DABS_1:         CONTINUE

; DMAX ( d1 d2 -- d3 )
;
; d3 is the greater of d1 and d2.

                HEADER  4,"DMAX",NORMAL
DMAX:           jsr     DO_COLON
                dw      TWO_OVER
                dw      TWO_OVER
                dw      D_LESS
                dw      QUERY_BRANCH,DMAX_1
                dw      TWO_SWAP
DMAX_1:         dw      TWO_DROP
                dw      EXIT

; DMIN ( d1 d2 -- d3 )
;
; d3 is the lesser of d1 and d2.

                HEADER  4,"DMIN",NORMAL
DMIN:           jsr     DO_COLON
                dw      TWO_OVER
                dw      TWO_OVER
                dw      D_LESS
                dw      INVERT
                dw      QUERY_BRANCH,DMIN_1
                dw      TWO_SWAP
DMIN_1:         dw      TWO_DROP
                dw      EXIT

; DNEGATE ( d1 -- d2 )
;
; d2 is the negation of d1.

                HEADER  7,"DNEGATE",NORMAL
DNEGATE:
                sec
                lda     #0                      ; Subtract low word from zero
                sbc     <3
                sta     <3
                lda     #0                      ; Then the high word
                sbc     <1
                sta     <1
                CONTINUE                        ; Done

;===============================================================================
; Mixed Arithmetic
;-------------------------------------------------------------------------------

; D>S ( d -- n )
;
; n is the equivalent of d. An ambiguous condition exists if d lies outside the
; range of a signed single-cell number.

                HEADER  3,"D>S",NORMAL
D_TO_S:
                tdc
                inc     a                       ; Drop the high word
                inc     a
                tcd
                CONTINUE

; M* ( n1 n2 -- d )
;
; d is the signed product of n1 times n2.
;
; In this implementation it is defined as:
;
;   2DUP XOR >R                 carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE

                HEADER  2,"M*",NORMAL
M_STAR:         jsr     DO_COLON
                dw      TWO_DUP
                dw      XOR
                dw      TO_R
                dw      SWAP
                dw      ABS
                dw      SWAP
                dw      ABS
                dw      UM_STAR
                dw      R_FROM
                dw      QUERY_DNEGATE
                dw      EXIT

; M*/ ( d1 n1 +n2 -- d2 )
;
; Multiply d1 by n1 producing the triple-cell intermediate result t. Divide t
; by +n2 giving the double-cell quotient d2. An ambiguous condition exists if
; +n2 is zero or negative, or the quotient lies outside of the range of a
; double-precision signed integer.



; M+ ( d1|ud1 n -- d2|ud2 )
;
; Add n to d1|ud1, giving the sum d2|ud2.

                HEADER  2,"M+",NORMAL
M_PLUS:
                clc
                lda     <1
                adc     <5
                sta     <5
                bcc     $+4
                inc     <3
                tdc
                inc     a
                inc     a
                tcd
                CONTINUE

; S>D ( n -- d )
;
; Convert the number n to the double-cell number d with the same numerical
; value.

                HEADER  3,"S>D",NORMAL
S_TO_D:
                tdc
                dec     a                       ; Assume n is positive
                dec     a
                tcd
                stz     <1                      ; .. push a zero value
                lda     <3                      ; Test the number
                bpl     S_TO_D_1
                dec     <1                      ; Make top -1 if negative
S_TO_D_1        CONTINUE                        ; Done

; SM/REM ( d1 n1 -- n2 n3 )
;
; Divide d1 by n1, giving the symmetric quotient n3 and the remainder n2.
; Input and output stack arguments are signed. An ambiguous condition exists if
; n1 is zero or if the quotient lies outside the range of a single-cell signed
; integer.
;
; In this implementation it is defined as:
;
;   2DUP XOR >R                 sign of quotient
;   OVER >R                     sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;

                HEADER  6,"SM/REM",NORMAL
SM_SLASH_REM:   jsr     DO_COLON
                dw      TWO_DUP
                dw      XOR
                dw      TO_R
                dw      OVER
                dw      TO_R
                dw      ABS
                dw      TO_R
                dw      DABS
                dw      R_FROM
                dw      UM_SLASH_MOD
                dw      SWAP
                dw      R_FROM
                dw      QUERY_NEGATE
                dw      SWAP
                dw      R_FROM
                dw      QUERY_NEGATE
                dw      EXIT

; UD* ( ud1 d2 -- ud3)
;
; 32*16->32 multiply
;
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;

                HEADER  3,"UD*",NORMAL
UD_STAR:        jsr     DO_COLON
                dw      DUP
                dw      TO_R
                dw      UM_STAR
                dw      DROP
                dw      SWAP
                dw      R_FROM
                dw      UM_STAR
                dw      ROT
                dw      PLUS
                dw      EXIT

; UM* ( u1 u2 -- ud )
;
; Multiply u1 by u2, giving the unsigned double-cell product ud. All values and
; arithmetic are unsigned.

                HEADER  3,"UM*",NORMAL
UM_STAR:
                lda     <1                      ; Fetch multiplier
                pha
                stz     <1                      ; Clear the result
                ldx     #16
UM_STAR_1:      lda     <3                      ; Shift multiplier one bit
                lsr     a
                bcc     UM_STAR_2               ; Not set, no add
                lda     1,s                     ; Fetch multiplicand
                clc
                adc     <1
                sta     <1
UM_STAR_2:      ror     <1                      ; Rotate high word down
                ror     <3
                dex
                bne     UM_STAR_1
                pla
                CONTINUE                        ; Done

; UM/MOD ( ud u1 -- u2 u3 )
;
; Divide ud by u1, giving the quotient u3 and the remainder u2. All values and
; arithmetic are unsigned. An ambiguous condition exists if u1 is zero or if the
; quotient lies outside the range of a single-cell unsigned integer.

                HEADER  6,"UM/MOD",NORMAL
UM_SLASH_MOD:
                sec                             ; Check for overflow
                lda     <3
                sbc     <1
                bcs     UM_SLASH_MOD_3

                ldx     #17
UM_SLASH_MOD_1: rol     <5                      ; Rotate dividend lo
                dex
                beq     UM_SLASH_MOD_4
                rol     <3
                bcs     UM_SLASH_MOD_2          ; Carry set dividend > divisor

                lda     <3                      ; Is dividend < divisor?
                cmp     <1
                bcc     UM_SLASH_MOD_1          ; Yes, shift in 0

UM_SLASH_MOD_2: lda     <3                      ; Reduce dividend
                sbc     <1
                sta     <3
                bra     UM_SLASH_MOD_1          ; Shift in 1

UM_SLASH_MOD_3: lda     #$ffff                  ; Overflowed set results
                sta     <3
                sta     <5
UM_SLASH_MOD_4: tdc                             ; Drop top word
                inc     a
                inc     a
                tcd
                jmp     SWAP                    ; Swap quotient and remainder

;===============================================================================
; Comparisons
;-------------------------------------------------------------------------------

; 0< ( n -- flag )
;
; flag is true if and only if n is less than zero.

                HEADER  2,"0<",NORMAL
ZERO_LESS:
                lda     <1                      ; Test top of stack
                stz     <1                      ; Assume false result
                bpl     ZERO_LT_1               ; Was the value negative?
                dec     <1                      ; Yes, make true result
ZERO_LT_1:      CONTINUE                        ; Done

; 0<> ( x -- flag )
;
; flag is true if and only if x is not equal to zero.

                HEADER  3,"0<>",NORMAL
ZERO_NOT_EQUAL:
                lda     <1                      ; Test top of stack
                stz     <1                      ; Assume false result
                beq     ZERO_NE_1               ; Was the value non-zero?
                dec     <1                      ; Yes, make true result
ZERO_NE_1:      CONTINUE                        ; Done

; 0= ( x -- flag )
;
; flag is true if and only if x is equal to zero.

                HEADER  2,"0=",NORMAL
ZERO_EQUAL:
                lda     <1                      ; Test top of stack
                stz     <1                      ; Assume false result
                bne     ZERO_EQ_1               ; Was the value zero?
                dec     <1                      ; Yes, make true result
ZERO_EQ_1:      CONTINUE                        ; Done

; 0> ( n -- flag )
;
; flag is true if and only if n is greater than zero.

                HEADER  2,"0>",NORMAL
ZERO_GREATER:
                lda     <1                      ; Test top of stack
                stz     <1                      ; Assume false result
                bmi     ZERO_GT_EXIT            ; Was the value positive?
                beq     ZERO_GT_EXIT            ; .. but not zero
                dec     <1                      ; Yes, make true result
ZERO_GT_EXIT:   CONTINUE                        ; Done

; < ( n1 n2 -- flag )
;
; flag is true if and only if n1 is less than n2.

                HEADER  1,"<",NORMAL
LESS:           jsr     DO_COLON
                dw      SWAP
                dw      GREATER
                dw      EXIT

; <> ( x1 x2 -- flag )
;
; flag is true if and only if x1 is not bit-for-bit the same as x2.

                HEADER  2,"<>",NORMAL
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
NE_EXIT:        CONTINUE                        ; Done

; = ( x1 x2 -- flag )
;
; flag is true if and only if x1 is bit-for-bit the same as x2.

                HEADER  1,"=",NORMAL
EQUAL:
                ldx     <1                      ; Pull x2 from stack
                tdc
                inc     a
                inc     a
                tcd
                cpx     <1                      ; Compare with x1
                stz     <1                      ; Assume not equal
                bne     EQ_EXIT                 ; Test the flags
                dec     <1                      ; Make result true
EQ_EXIT:        CONTINUE                        ; Done

; > ( n1 n2 -- flag )
;
; flag is true if and only if n1 is greater than n2.

                HEADER  1,">",NORMAL
GREATER:
                ldx     <1                      ; Pull x2 from stack
                tdc
                inc     a
                inc     a
                tcd
                txa
                sec                             ; Compare with x1
                sbc     <1
                stz     <1                      ; Assume false result
                bvc     GREATER_1
                eor     #$8000
GREATER_1:      bpl     GREATER_2               ; V == 1 && N == 1
                dec     <1
GREATER_2:      CONTINUE

; U< ( u1 u2 -- flag )
;
; flag is true if and only if u1 is less than u2.

                HEADER  2,"U<",NORMAL
U_LESS:
                ldx     <1                      ; Pull x2
                tdc                             ; Drop from stack
                inc     a
                inc     a
                tcd
                cpx     <1                      ; Compare with x1
                stz     <1                      ; Assume false
                beq     U_LESS_1                ; Equal
                bcc     U_LESS_1                ; Less
                dec     <1
U_LESS_1:       CONTINUE

; U> ( u1 u2 -- flag )
;
; flag is true if and only if u1 is greater than u2.

                HEADER  2,"U>",NORMAL
U_GREATER:      jsr     DO_COLON
                dw      SWAP
                dw      U_LESS
                dw      EXIT

;===============================================================================
; Logical Operations
;-------------------------------------------------------------------------------

; AND ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit logical “and” of x1 with x2.

                HEADER  3,"AND",NORMAL
AND:
                lda     <1
                and     <3
                sta     <3
                tdc
                inc     a
                inc     a
                tcd
                CONTINUE

; INVERT ( x1 -- x2 )
;
; Invert all bits of x1, giving its logical inverse x2.

                HEADER  6,"INVERT",NORMAL
INVERT:
                lda     <1                      ; Fetch top value
                eor     #$ffff                  ; Invert all the bits
                sta     <1                      ; .. and write back
                CONTINUE                        ; Done

; LSHIFT ( x1 u -- x2 )
;
; Perform a logical left shift of u bit-places on x1, giving x2. Put zeroes
; into the least significant bits vacated by the shift. An ambiguous condition
; exists if u is greater than or equal to the number of bits in a cell.

                HEADER  6,"LSHIFT",NORMAL
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
LSHIFT_0        CONTINUE                        ; Done
LSHIFT_2        stz     <1                      ; Clear top value
                CONTINUE                        ; Done

; OR ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit inclusive-or of x1 with x2.

                HEADER  2,"OR",NORMAL
OR:
                lda     <1
                ora     <3
                sta     <3
                tdc
                inc     a
                inc     a
                tcd
                CONTINUE

; RSHIFT ( x1 u -- x2 )
;
; Perform a logical right shift of u bit-places on x1, giving x2. Put zeroes
; into the most significant bits vacated by the shift. An ambiguous condition
; exists if u is greater than or equal to the number of bits in a cell.

                HEADER  6,"RSHIFT",NORMAL
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
RSHIFT_0        CONTINUE                        ; Done
RSHIFT_2        stz     <1                      ; Clear top value
                CONTINUE                        ; Done

; XOR ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit exclusive-or of x1 with x2.

                HEADER  3,"XOR",NORMAL
XOR:
                lda     <1
                eor     <3
                sta     <3
                tdc
                inc     a
                inc     a
                tcd
                CONTINUE

;===============================================================================
; Control Words
;-------------------------------------------------------------------------------

; ?ABORT
;
;   ROT IF TYPE ABORT THEN 2DROP ;

QUERY_ABORT:    jsr     DO_COLON
                dw      ROT
                dw      QUERY_BRANCH,QUERY_ABORT_1
                dw      TYPE
                dw      ABORT
QUERY_ABORT_1:  dw      TWO_DROP
                dw      EXIT

; ABORT ( i*x -- ) ( R: j*x -- )
;
; Empty the data stack and perform the function of QUIT, which includes
; emptying the return stack, without displaying a message.

                HEADER  5,"ABORT",NORMAL
ABORT:          jsr     DO_COLON
                dw      DO_ABORT
                dw      QUIT

DO_ABORT:
                lda     #DSTACK_END-1
                tcd
                CONTINUE

; (BUILD) ( dtc-addr -- )
;
; Adds a jump the to exection function for the new word.

;               HEADER  7,"(BUILD)",NORMAL
BUILD:          jsr     DO_COLON
                dw      DO_LITERAL,$20
                dw      C_COMMA
                dw      COMMA
                dw      EXIT

; CREATE ( -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below. If the data-
; space pointer is not aligned, reserve enough data space to align it. The new
; data-space pointer defines name’s data field. CREATE does not allocate data
; space in name’s data field.

                HEADER  6,"CREATE",NORMAL
CREATE:         jsr     DO_COLON
                dw      LATEST
                dw      FETCH
                dw      COMMA
                dw      ZERO
                dw      C_COMMA
                dw      HERE
                dw      LATEST
                dw      STORE
                dw      BL
                dw      WORD
                dw      C_FETCH
                dw      ONE_PLUS
                dw      ALLOT
                dw      EXIT

; EXECUTE ( i*x xt -- j*x )
;
; Remove xt from the stack and perform the semantics identified by it. Other
; stack effects are due to the word EXECUTEd.

                HEADER  7,"EXECUTE",NORMAL
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
;
; Return control to the calling definition specified by nest-sys. Before
; executing EXIT within a do-loop, a program shall discard the loop-control
; parameters by executing UNLOOP.

                HEADER  4,"EXIT",NORMAL
EXIT:
                ply
                CONTINUE

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
;     REFILL
;     WHILE SOURCE EVALUATE
;     STATE @ 0= IF S" Ok" CR TYPE THEN
;   AGAIN ;

                HEADER  4,"QUIT",NORMAL
QUIT:           jsr     DO_COLON
                dw      DO_QUIT
                dw      ZERO
                dw      STATE
                dw      STORE
                dw      ZERO
                dw      SOURCEID
                dw      STORE
QUIT_1:         dw      REFILL
                dw      QUERY_BRANCH,QUIT_2
                dw      INTERPRET
QUIT_2:         dw      STATE
                dw      FETCH
                dw      ZERO_EQUAL
                dw      QUERY_BRANCH,QUIT_3
                dw      DO_S_QUOTE
                db      2,"Ok"
                dw      TYPE
                dw      CR
QUIT_3:         dw      BRANCH,QUIT_1

DO_QUIT:
                lda     #RSTACK_END-1           ; Reset the return stack
                tcs
                CONTINUE                        ; Done

;===============================================================================
; Parser & Interpreter
;-------------------------------------------------------------------------------

; ?NUMBER
;
;   DUP  0 0 ROT COUNT      -- ca ud adr n
;   ?SIGN >R  >NUMBER       -- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0   -- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1  -- n -1   (ok)
;   THEN ;

                HEADER  7,"?NUMBER",NORMAL
QUERY_NUMBER:   jsr     DO_COLON
                dw      DUP
                dw      ZERO
                dw      ZERO
                dw      ROT
                dw      COUNT
                dw      QUERY_SIGN
                dw      TO_R
                dw      TO_NUMBER
                dw      QUERY_BRANCH,QNUM_1
                dw      R_FROM
                dw      TWO_DROP
                dw      TWO_DROP
                dw      ZERO
                dw      BRANCH,QNUM_3
QNUM_1:         dw      TWO_DROP
                dw      NIP
                dw      R_FROM
                dw      QUERY_BRANCH,QNUM_2
                dw      NEGATE
QNUM_2:         dw      DO_LITERAL,-1
QNUM_3:         dw      EXIT

; ?SIGN ( c-addr n -- adr' n' f )
;
;   OVER C@                 -- adr n c
;   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
;   DUP IF 1+               -- +=0, -=+2
;       >R 1 /STRING R>     -- adr' n' f
;   THEN ;

                HEADER  5,"?SIGN",NORMAL
QUERY_SIGN:     jsr     DO_COLON
                dw      OVER
                dw      C_FETCH
                dw      DO_LITERAL,','
                dw      MINUS
                dw      DUP
                dw      ABS
                dw      DO_LITERAL,1
                dw      EQUAL
                dw      AND
                dw      DUP
                dw      QUERY_BRANCH,QSIGN_1
                dw      ONE_PLUS
                dw      TO_R
                dw      DO_LITERAL,1
                dw      SLASH_STRING
                dw      R_FROM
QSIGN_1:        dw      EXIT

; >COUNTED ( c-addr n -- )
;
;   2DUP C! CHAR+ SWAP CMOVE

TO_COUNTED:     jsr     DO_COLON
                dw      TWO_DUP
                dw      C_STORE
                dw      CHAR_PLUS
                dw      SWAP
                dw      CMOVE
                dw      EXIT

; >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
;
; ud2 is the unsigned result of converting the characters within the string
; specified by c-addr1 u1 into digits, using the number in BASE, and adding
; each into ud1 after multiplying ud1 by the number in BASE. Conversion
; continues left-to-right until a character that is not convertible, including
; any “+” or “-”, is encountered or the string is entirely converted. c-addr2
; is the location of the first unconverted character or the first character
; past the end of the string if the string was entirely converted. u2 is the
; number of unconverted characters in the string. An ambiguous condition exists
; if ud2 overflows during the conversion.
;
; In this implementation its is defined as:
;
;   BEGIN
;   DUP WHILE
;       OVER C@ DIGIT?
;       0= IF DROP EXIT THEN
;       >R 2SWAP BASE @ UD*
;       R> M+ 2SWAP
;       1 /STRING
;   REPEAT ;

                HEADER  7,">NUMBER",NORMAL
TO_NUMBER:      jsr     DO_COLON
TO_NUM_1:       dw      DUP
                dw      QUERY_BRANCH,TO_NUM_3
                dw      OVER
                dw      C_FETCH
                dw      DIGIT_QUERY
                dw      ZERO_EQUAL
                dw      QUERY_BRANCH,TO_NUM_2
                dw      DROP
                dw      EXIT
TO_NUM_2:       dw      TO_R
                dw      TWO_SWAP
                dw      BASE
                dw      FETCH
                dw      UD_STAR
                dw      R_FROM
                dw      M_PLUS
                dw      TWO_SWAP
                dw      DO_LITERAL,1
                dw      SLASH_STRING
                dw      BRANCH,TO_NUM_1
TO_NUM_3:       dw      EXIT

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
; In this implementation it is defined as:
;
;   OVER + 1- OVER      -- sa ea a
;   BEGIN KEY           -- sa ea a c
;   DUP 0D <> WHILE
;     DUP 8 = OVER 127 = OR IF
;       DROP 1-
;       >R OVER R> UMAX
;       8 EMIT SPACE 8 EMIT
;     ELSE
;       DUP EMIT        -- sa ea a c
;       OVER C! 1+ OVER UMIN
;     THEN              -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - ;

                HEADER  6,"ACCEPT",NORMAL
ACCEPT:         jsr     DO_COLON
                dw      OVER
                dw      PLUS
                dw      ONE_MINUS
                dw      OVER
ACCEPT_1:       dw      KEY
                dw      DUP
                dw      DO_LITERAL,$0D
                dw      NOT_EQUAL
                dw      QUERY_BRANCH,ACCEPT_4
                dw      DUP
                dw      DO_LITERAL,$08
                dw      EQUAL
                dw      OVER
                dw      DO_LITERAL,$7f
                dw      EQUAL
                dw      OR
                dw      QUERY_BRANCH,ACCEPT_2
                dw      DROP
                dw      ONE_MINUS
                dw      TO_R
                dw      OVER
                dw      R_FROM
                dw      UMAX
                dw      DO_LITERAL,8
                dw      EMIT
                dw      SPACE
                dw      DO_LITERAL,8
                dw      EMIT
                dw      BRANCH,ACCEPT_3
ACCEPT_2:       dw      DUP
                dw      EMIT
                dw      OVER
                dw      C_STORE
                dw      ONE_PLUS
                dw      OVER
                dw      UMIN
ACCEPT_3:       dw      BRANCH,ACCEPT_1
ACCEPT_4:       dw      DROP
                dw      NIP
                dw      SWAP
                dw      MINUS
                dw      EXIT

; DIGIT?
;
;   [ HEX ] DUP 39 > 100 AND +     silly looking
;   DUP 140 > 107 AND -   30 -     but it works!
;   DUP BASE @ U< ;

                HEADER  6,"DIGIT?",NORMAL
DIGIT_QUERY:    jsr     DO_COLON
                dw      DUP
                dw      DO_LITERAL,'9'
                dw      GREATER
                dw      DO_LITERAL,$100
                dw      AND
                dw      PLUS
                dw      DUP
                dw      DO_LITERAL,$140
                dw      GREATER
                dw      DO_LITERAL,$107
                dw      AND
                dw      MINUS
                dw      DO_LITERAL,'0'
                dw      MINUS
                dw      DUP
                dw      BASE
                dw      FETCH
                dw      U_LESS
                dw      EXIT

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

                HEADER  8,"EVALUATE",NORMAL
EVALUATE:       jsr     DO_COLON
                dw      TO_R
                dw      TO_R
                dw      SAVE_INPUT
                dw      R_FROM
                dw      R_FROM
                dw      TRUE
                dw      SOURCEID
                dw      STORE
                dw      ZERO
                dw      TO_IN
                dw      STORE
                dw      LENGTH
                dw      STORE
                dw      BUFFER
                dw      STORE
                dw      INTERPRET
                dw      RESTORE_INPUT
                dw      DROP
                dw      EXIT

; INTERPRET ( -- )
;
;
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       FIND                    -- a 0/1/-1
;       ?DUP IF                 -- xt 1/-1
;           1+ STATE @ 0= OR    immed or interp?
;           IF EXECUTE ELSE , THEN
;       ELSE                    -- textadr
;           ?NUMBER
;           IF STATE @
;               IF POSTPONE LITERAL THEN     converted ok
;           ELSE COUNT TYPE 3F EMIT CR ABORT  err
;           THEN
;       THEN
;   REPEAT DROP ;

                HEADER  9,"INTERPRET",NORMAL
INTERPRET:      jsr     DO_COLON
INTERPRET_1:    dw      BL
                dw      WORD
                dw      DUP
                dw      C_FETCH
                dw      QUERY_BRANCH,INTERPRET_7
                dw      FIND
                dw      QUERY_DUP
                dw      QUERY_BRANCH,INTERPRET_4
                dw      ONE_PLUS
                dw      STATE
                dw      FETCH
                dw      ZERO_EQUAL
                dw      OR
                dw      QUERY_BRANCH,INTERPRET_2
                dw      EXECUTE
                dw      BRANCH,INTERPRET_3
INTERPRET_2:    dw      COMMA
INTERPRET_3:    dw      BRANCH,INTERPRET_6
INTERPRET_4:    dw      QUERY_NUMBER
                dw      QUERY_BRANCH,INTERPRET_5
                dw      STATE
                dw      FETCH
                dw      QUERY_BRANCH,INTERPRET_6
                dw      LITERAL
                dw      BRANCH,INTERPRET_6
INTERPRET_5:    dw      COUNT
                dw      TYPE
                dw      DO_LITERAL,$3f
                dw      EMIT
                dw      CR
                dw      ABORT
INTERPRET_6     dw      BRANCH,INTERPRET_1
INTERPRET_7:    dw      DROP
                dw      EXIT

; FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
;
; Find the definition named in the counted string at c-addr. If the definition
; is not found, return c-addr and zero. If the definition is found, return its
; execution token xt. If the definition is immediate, also return one (1),
; otherwise also return minus-one (-1). For a given string, the values returned
; by FIND while compiling may differ from those returned while not compiling.
;
; In this implementation it is defined as:
;
;   LATEST @ BEGIN             -- a nfa
;       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
;       S=                     -- a nfa f
;       DUP IF
;           DROP
;           NFA>LFA @ DUP      -- a link link
;       THEN
;   0= UNTIL                   -- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA        -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;   THEN ;

                HEADER  4,"FIND",NORMAL
FIND:           jsr     DO_COLON
                dw      LATEST
                dw      FETCH
FIND1:          dw      TWO_DUP
                dw      OVER
                dw      C_FETCH
                dw      CHAR_PLUS
                dw      S_EQUAL
                dw      DUP
                dw      QUERY_BRANCH,FIND2
                dw      DROP
                dw      NFA_TO_LFA
                dw      FETCH
                dw      DUP
FIND2:          dw      ZERO_EQUAL
                dw      QUERY_BRANCH,FIND1
                dw      DUP
                dw      QUERY_BRANCH,FIND3
                dw      NIP
                dw      DUP
                dw      NFA_TO_CFA
                dw      SWAP
                dw      IMMED_QUERY
                dw      ZERO_EQUAL
                dw      DO_LITERAL,1
                dw      OR
FIND3:          dw      EXIT

; IMMED? ( nfa -- f )

IMMED_QUERY:    jsr     DO_COLON
                dw      ONE_MINUS
                dw      C_FETCH
                dw      EXIT

; NFA>CFA ( nfa -- cfa )

NFA_TO_CFA:     jsr     DO_COLON
                dw      COUNT
                dw      PLUS
                dw      EXIT

; NFA>LFA ( nfa -- lfa )

NFA_TO_LFA:     jsr     DO_COLON
                dw      DO_LITERAL,3
                dw      MINUS
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
; In this implementation it is defined as:
;
;   SOURCE-ID 0= IF
;     TIB DUP #TIB @ ACCEPT SPACE
;     LENGTH ! BUFFER !
;     0 >IN ! TRUE EXIT
;   THEN
;   FALSE

                HEADER  6,"REFILL",NORMAL
REFILL:         jsr     DO_COLON
                dw      SOURCE_ID
                dw      ZERO_EQUAL
                dw      QUERY_BRANCH,REFILL_1
                dw      TIB
                dw      DUP
                dw      HASH_TIB
                dw      FETCH
                dw      ACCEPT
                dw      SPACE
                dw      LENGTH
                dw      STORE
                dw      BUFFER
                dw      STORE
                dw      ZERO
                dw      TO_IN
                dw      STORE
                dw      TRUE
                dw      EXIT
REFILL_1:       dw      FALSE
                dw      EXIT

; RESTORE-INPUT ( xn ... x1 n -- flag )
;
; Attempt to restore the input source specification to the state described by
; x1 through xn. flag is true if the input source specification cannot be so
; restored.
;
; An ambiguous condition exists if the input source represented by the
; arguments is not the same as the current input source.
;
; In this implementation it is defined as:
;
;   >IN ! (LENGTH) ! BUFFER !
;   SOURCEID !
;   TRUE

                HEADER  13,"RESTORE-INPUT",NORMAL
RESTORE_INPUT   jsr     DO_COLON
                dw      TO_IN
                dw      STORE
                dw      LENGTH
                dw      STORE
                dw      BUFFER
                dw      STORE
                dw      SOURCEID
                dw      STORE
                dw      TRUE
                dw      EXIT

; S= ( c-addr1 caddr2 u -- n)
;
; Misnamed, more like C's strncmp. Note that counted length bytes are compared!

S_EQUAL:
                phy
                ldx     <1                      ; Fetch maximum length
                beq     S_EQUAL_3
                ldy     #0
                short_a
S_EQUAL_1:
                lda     (5),y                   ; Compare bytes
                cmp     (3),y
                bne     S_EQUAL_2
                iny
                dex                             ; End of strings?
                bne     S_EQUAL_1               ; No
                bra     S_EQUAL_3               ; Yes. must be the same
S_EQUAL_2:
                ldx     #$ffff                  ; Difference found
S_EQUAL_3:
                long_a
                tdc                             ; Clean up the stack
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                stx     <1                      ; Save the flag
                ply
                CONTINUE

; SAVE-INPUT ( -- xn ... x1 n )
;
; x1 through xn describe the current state of the input source specification
; for later use by RESTORE-INPUT.

                HEADER  10,"SAVE-INPUT",NORMAL
SAVE_INPUT:     jsr     DO_COLON
                dw      SOURCEID
                dw      FETCH
                dw      BUFFER
                dw      FETCH
                dw      LENGTH
                dw      FETCH
                dw      TO_IN
                dw      FETCH
                dw      EXIT

; SCAN ( c-addr n c == c-addr' n' )

SCAN:
SCAN_1:
                lda     <3                      ; Any data left to scan?
                beq     SCAN_2                  ; No.
                lda     <1                      ; Fetch and compare with scan
                short_a
                cmp     (5)
                long_a
                beq     SCAN_2
                inc     <5
                dec     <3
                bra     SCAN_1
SCAN_2:
                jmp     DROP                    ; Drop the character

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

; SOURCE ( -- c-addr u )
;
; c-addr is the address of, and u is the number of characters in, the input
; buffer.
;
; In this implementation it is defined as
;
;   BUFFER @ LENGTH @

                HEADER  6,"SOURCE",NORMAL
SOURCE:         jsr     DO_COLON
                dw      BUFFER
                dw      FETCH
                dw      LENGTH
                dw      FETCH
                dw      EXIT

; SOURCE-ID ( -- 0 | -1 )
;
; Identifies the input source: -1 if string (via EVALUATE), 0 if user input
; device.

                HEADER  9,"SOURCE-ID",NORMAL
SOURCE_ID:      jsr     DO_COLON
                dw      SOURCEID
                dw      FETCH
                dw      EXIT

; WORD ( char “<chars>ccc<char>” -- c-addr )
;
; Skip leading delimiters. Parse characters ccc delimited by char. An
; ambiguous condition exists if the length of the parsed string is greater
; than the implementation-defined length of a counted string.
;
; c-addr is the address of a transient region containing the parsed word as
; a counted string. If the parse area was empty or contained no characters
; other than the delimiter, the resulting string has a zero length. A space,
; not included in the length, follows the string. A program may replace
; characters within the string.
;
; In this implementation it is defined as:
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

                HEADER  4,"WORD",NORMAL
WORD:           jsr     DO_COLON
                dw      DUP
                dw      SOURCE
                dw      TO_IN
                dw      FETCH
                dw      SLASH_STRING
                dw      DUP
                dw      TO_R
                dw      ROT
                dw      SKIP
                dw      OVER
                dw      TO_R
                dw      ROT
                dw      SCAN
                dw      DUP
                dw      QUERY_BRANCH,WORD_1
                dw      CHAR_MINUS
WORD_1:         dw      R_FROM
                dw      R_FROM
                dw      ROT
                dw      MINUS
                dw      TO_IN
                dw      PLUS_STORE
                dw      TUCK
                dw      MINUS
                dw      HERE
                dw      TO_COUNTED
                dw      HERE
                dw      BL
                dw      OVER
                dw      COUNT
                dw      PLUS
                dw      C_STORE
                dw      EXIT

;===============================================================================
; String Words
;-------------------------------------------------------------------------------

; -TRAILING ( c-addr u1 -- c-addr u2 )
;
; If u1 is greater than zero, u2 is equal to u1 less the number of spaces at
; the end of the character string specified by c-addr u1. If u1 is zero or the
; entire string consists of spaces, u2 is zero.

                HEADER  9,"-TRAILING",NORMAL
DASH_TRAILING:
                phy                             ; Save IP
                ldy     <1                      ; Is u1 > 0?
                beq     DASH_TRAIL_3            ; No
                short_a
                dey                             ; Convert to offset
DASH_TRAIL_1:   lda     (3),y                   ; Space character at end?
                cmp     #' '
                bne     DASH_TRAIL_2            ; No
                dey                             ; More characters to check?
                bpl     DASH_TRAIL_1            ; Yes
DASH_TRAIL_2:   long_a
                iny                             ; Convert to length
DASH_TRAIL_3:   sty     <1                      ; Update
                ply                             ; Restore IP
                CONTINUE                        ; Done

; /STRING ( c-addr1 u1 n -- c-addr2 u2 )
;
; Adjust the character string at c-addr1 by n characters. The resulting
; character string, specified by c-addr2 u2, begins at c-addr1 plus n;
; characters and is u1 minus n characters long.
;
; In this implementation it is defined as:
;
;   ROT OVER + ROT ROT -

                HEADER  7,"/STRING",NORMAL
SLASH_STRING:   jsr     DO_COLON
                dw      ROT
                dw      OVER
                dw      PLUS
                dw      ROT
                dw      ROT
                dw      MINUS
                dw      EXIT

; BLANK ( c-addr u -- )
;
; If u is greater than zero, store the character value for space in u
; consecutive character positions beginning at c-addr.
;
; In this implementation it is defined as
;
;   ?DUP IF OVER + SWAP DO BL I C! LOOP ELSE DROP THEN

                HEADER  5,"BLANK",NORMAL
BLANK:          jsr     DO_COLON
                dw      QUERY_DUP
                dw      QUERY_BRANCH,BLANK_2
                dw      OVER
                dw      PLUS
                dw      SWAP
                dw      DO_DO
BLANK_1:        dw      BL
                dw      I
                dw      C_STORE
                dw      DO_LOOP,BLANK_1
                dw      EXIT
BLANK_2:        dw      DROP
                dw      EXIT

; CMOVE ( c-addr1 c-addr2 u -- )
;
; If u is greater than zero, copy u consecutive characters from the data space
; starting at c-addr1 to that starting at c-addr2, proceeding character-by-
; character from lower addresses to higher addresses.

                HEADER  5,"CMOVE",NORMAL
CMOVE:
                phy
                ldx     <1                      ; Any characters to move?
                beq     CMOVE_2                 ; No
                ldy     #0
                short_a
CMOVE_1:                                        ; Transfer a byte
                lda     (5),y
                sta     (3),y
                iny
                dex                             ; Decrement count
                bne     CMOVE_1                 ; .. and repeat until done
                long_a
CMOVE_2:
                tdc                             ; Clean up the stack
                clc
                adc     #6
                tcd
                ply
                CONTINUE                        ; Done

; CMOVE> ( c-addr1 c-addr2 u -- )
;
; If u is greater than zero, copy u consecutive characters from the data space
; starting at c-addr1 to that starting at c-addr2, proceeding character-by-
; character from higher addresses to lower addresses.

                HEADER  6,"CMOVE>",NORMAL
CMOVE_GREATER:
                phy
                ldx     <1                      ; Any characters to move?
                beq     CMOVE_GT_2              ; No.
                ldy     <1
                short_a
CMOVE_GT_1:
                dey                             ; Transfer a byte
                lda     (5),y
                sta     (3),y
                dex                             ; Decrement length
                bne     CMOVE_GT_1              ; .. and repeat until done
                long_a
CMOVE_GT_2:
                tdc                             ; Clean up the stack
                clc
                adc     #6
                tcd
                ply
                CONTINUE                        ; Done

; COMPARE ( c-addr1 u1 c-addr2 u2 -- n )
;
; Compare the string specified by c-addr1 u1 to the string specified by c-addr2
; u2. The strings are compared, beginning at the given addresses, character by
; character, up to the length of the shorter string or until a difference is
; found. If the two strings are identical, n is zero. If the two strings are
; identical up to the length of the shorter string, n is minus-one (-1) if u1
; is less than u2 and one (1) otherwise. If the two strings are not identical
; up to the length of the shorter string, n is minus-one (-1) if the first
; non-matching character in the string specified by c-addr1 u1 has a lesser
; numeric value than the corresponding character in the string specified by
; c-addr2 u2 and one (1) otherwise.

                HEADER  7,"COMPARE",NORMAL
COMPARE:
                lda     <1                      ; Both string lengths zero?
                ora     <5
                beq     COMPARE_X               ; Yes, must be equal

                lda     <1                      ; Second string length zero?
                beq     COMPARE_P               ; Yes, must be shorter
                lda     <5                      ; First string length zero?
                beq     COMPARE_N               ; Yes, must be shorter
                short_a
                lda     (7)                     ; Compare next characters
                cmp     (3)
                long_a
                bcc     COMPARE_N
                bne     COMPARE_P

                inc     <3                      ; Bump string pointers
                inc     <7
                dec     <1                      ; And reduce lengths
                dec     <5
                bra     COMPARE

COMPARE_P:      lda     #1
                bra     COMPARE_X
COMPARE_N:      lda     #-1

COMPARE_X:      sta     <7                      ; Save the result
                tdc
                clc
                adc     #6
                tcd
                CONTINUE                        ; Done

; COUNT ( c-addr1 -- c-addr2 u )
;
; Return the character string specification for the counted string stored at
; c-addr1. c-addr2 is the address of the first character after c-addr1. u is
; the contents of the character at c-addr1, which is the length in characters
; of the string at c-addr2.
;
; In this implementation it is defined as
;
;   DUP CHAR+ SWAP C@

                HEADER  5,"COUNT",NORMAL
COUNT:          jsr     DO_COLON
                dw      DUP
                dw      CHAR_PLUS
                dw      SWAP
                dw      C_FETCH
                dw      EXIT

; SEARCH ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
;
; Search the string specified by c-addr1 u1 for the string specified by c-addr2
; u2. If flag is true, a match was found at c-addr3 with u3 characters
; remaining. If flag is false there was no match and c-addr3 is c-addr1 and u3
; is u1.

                HEADER  6,"SEARCH",NORMAL
SEARCH:         jsr     DO_COLON
; TODO
                CONTINUE

;===============================================================================
; Compiling Words
;-------------------------------------------------------------------------------

; ( ( -- )
;
; Parse ccc delimited by ) (right parenthesis). ( is an immediate word.
;
; The number of characters in ccc may be zero to the number of characters in the
; parse area.
;
; In this implementation it is defined as:
;
;  [ HEX ] 29 WORD DROP ; IMMEDIATE

                HEADER  1,"(",IMMEDIATE
                jsr     DO_COLON
                dw      DO_LITERAL,')'
                dw      WORD
                dw      DROP
                dw      EXIT

; .(

                HEADER  2,".(",IMMEDIATE
DOT_PAREN:      jsr     DO_COLON
                dw      DO_LITERAL,')'
                dw      WORD
                dw      COUNT
                dw      TYPE
                dw      EXIT

; ." ( -- )

                LINK    IMMEDIATE
                db      2,".",'"'
DOT_QUOTE:      jsr     DO_COLON
                dw      S_QUOTE
                dw      DO_LITERAL,TYPE
                dw      COMMA
                dw      EXIT


; +LOOP ( -- )

                HEADER  5,"+LOOP",IMMEDIATE
PLUS_LOOP:      jsr     DO_COLON
                dw      DO_LITERAL,DO_PLUS_LOOP
                dw      COMMA
                dw      COMMA
                dw      QUERY_DUP
                dw      QUERY_BRANCH,PLUS_LOOP_1
                dw      HERE
                dw      SWAP
                dw      STORE
PLUS_LOOP_1:    dw      EXIT

DO_PLUS_LOOP:
                ldx     <1                      ; Fetch increment
                tdc                             ; And drop
                inc     a
                inc     a
                tcd
                clc                             ; Add to loop counter
                txa
                adc     1,s
                sta     1,s
                cmp     3,s                     ; Reached limit?
                bcs     DO_PLOOP_END            ; Yes
                lda     !0,y                    ; No, branch back to start
                tay
                CONTINUE                        ; Done

DO_PLOOP_END:   iny                             ; Skip over address
                iny
                pla                             ; Drop loop variables
                pla
                CONTINUE                        ; Done

; : ( -- )

                HEADER  1,":",NORMAL
COLON:          jsr     DO_COLON
                dw      CREATE
                dw      DO_LITERAL,DO_COLON
                dw      BUILD
                dw      RIGHT_BRACKET
                dw      EXIT

DO_COLON:
                plx                             ; Pull new word IP-1
                phy                             ; Save the old IP
                inx                             ; Work out new IP
                txy
                CONTINUE                        ; Done

; :NONAME ( -- xt )

                HEADER  7,":NONAME",NORMAL
NONAME:         jsr     DO_COLON
                dw      HERE
                dw      DO_LITERAL,DO_COLON
                dw      BUILD
                dw      RIGHT_BRACKET
                dw      EXIT

; ; ( -- )

                LINK    IMMEDIATE
                db      1,";"
SEMICOLON:      jsr     DO_COLON
                dw      DO_LITERAL,EXIT
                dw      COMMA
                dw      LEFT_BRACKET
                dw      EXIT

; ?DO ( -- )

                HEADER  3,"?DO",IMMEDIATE
QUERY_DO:       jsr     DO_COLON
                dw      DO_LITERAL,QUERY_DO_DO
                dw      COMMA
                dw      HERE
                dw      ZERO
                dw      COMMA
                dw      HERE
                dw      EXIT

QUERY_DO_DO:
                lda     <1                      ; Are the start and limit
                eor     <3                      ; .. the same?
                beq     QUERY_DO_DO_1
                iny                             ; No, Skip over jump address
                iny
                jmp     DO_DO                   ; And start a normal loop

QUERY_DO_DO_1:  tdc                             ; Drop the loop parameters
                inc     a
                inc     a
                inc     a
                inc     a
                tcd
                jmp     BRANCH                  ; And skip over loop

; 2CONSTANT ( x “<spaces>name” -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below.

                HEADER  9,"2CONSTANT",NORMAL
TWO_CONSTANT:   jsr     DO_COLON
                dw      CREATE
                dw      DO_LITERAL,DO_TWO_CONSTANT
                dw      BUILD
                dw      COMMA
                dw      COMMA
                dw      EXIT; AGAIN ( -- )

DO_TWO_CONSTANT:
                plx                             ; Get return address
                tdc                             ; Create space on stack
                dec     a
                dec     a
                dec     a
                dec     a
                tcd
                lda     !1,x                    ; Transfer the value
                sta     <1
                lda     !3,x
                sta     <3
                CONTINUE                        ; Done

; 2LITERAL

                HEADER  8,"2LITERAL",IMMEDIATE
TWO_LITERAL:    jsr     DO_COLON
                dw      DO_LITERAL,DO_TWO_LITERAL
                dw      COMMA
                dw      COMMA
                dw      COMMA
                dw      EXIT

DO_TWO_LITERAL:
                tdc                             ; Make room on stack
                dec     a
                dec     a
                dec     a
                dec     a
                tcd
                lda     !0,y                    ; Fetch constant from IP
                sta     <1
                lda     !2,y
                sta     <3
                iny                             ; Bump IP
                iny
                iny
                iny
                CONTINUE                        ; Done

; 2VARIABLE

                HEADER  9,"2VARIABLE",IMMEDIATE
TWO_VARIABLE:   jsr     DO_COLON
                dw      CREATE
                dw      DO_LITERAL,DO_VARIABLE
                dw      BUILD
                dw      DO_LITERAL,2
                dw      CELLS
                dw      ALLOT
                dw      EXIT

; ABORT" ( -- )

                LINK    IMMEDIATE
                db      6,"ABORT",'"'
ABORT_QUOTE:    jsr     DO_COLON
                dw      S_QUOTE
                dw      DO_LITERAL,QUERY_ABORT
                dw      COMMA
                dw      EXIT

; AGAIN ( -- )
                HEADER  5,"AGAIN",IMMEDIATE
AGAIN:          jsr     DO_COLON
                dw      DO_LITERAL,BRANCH
                dw      COMMA
                dw      COMMA
                dw      EXIT

; BEGIN ( -- )

                HEADER  5,"BEGIN",IMMEDIATE
BEGIN:          jsr     DO_COLON
                dw      HERE
                dw      EXIT

; CONSTANT ( x “<spaces>name” -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below.

                HEADER  8,"CONSTANT",NORMAL
CONSTANT:       jsr     DO_COLON
                dw      CREATE
                dw      DO_LITERAL,DO_CONSTANT
                dw      BUILD
                dw      COMMA
                dw      EXIT

DO_CONSTANT:
                plx                             ; Get return address
                tdc                             ; Create space on stack
                dec     a
                dec     a
                tcd
                lda     !1,x                    ; Transfer the value
                sta     <1
                CONTINUE                        ; Done

; DO ( -- )

                HEADER  2,"DO",IMMEDIATE
DO:             jsr     DO_COLON
                dw      DO_LITERAL,DO_DO
                dw      COMMA
                dw      ZERO
                dw      HERE
                dw      EXIT

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
                CONTINUE

; ELSE ( -- )

                HEADER  4,"ELSE",IMMEDIATE
ELSE:           jsr     DO_COLON
                dw      DO_LITERAL,BRANCH
                dw      COMMA
                dw      HERE
                dw      ZERO
                dw      COMMA
                dw      HERE
                dw      SWAP
                dw      STORE
                dw      EXIT

BRANCH:
                lda     !0,y                    ; Load branch address into IP
                tay
                CONTINUE                        ; Done

; IF ( -- )

                HEADER  2,"IF",IMMEDIATE
IF:             jsr     DO_COLON
                dw      DO_LITERAL,QUERY_BRANCH
                dw      COMMA
                dw      HERE
                dw      ZERO
                dw      COMMA
                dw      EXIT

QUERY_BRANCH:
                ldx     <1                      ; Pull the top of stack value
                tdc
                inc     a                       ; Drop top item
                inc     a
                tcd
                txa
                beq     BRANCH                  ; Branch if top was zero
                iny                             ; Otherwise skip address
                iny
                CONTINUE                        ; Done

; IMMEDIATE ( -- )

                HEADER  9,"IMMEDIATE",IMMEDIATE
                jsr     DO_COLON
                dw      DO_LITERAL,IMMEDIATE
                dw      LATEST
                dw      FETCH
                dw      ONE_MINUS
                dw      C_STORE
                dw      EXIT

; LITERAL ( x -- )
;
; Append the run-time semantics given below to the current definition.

                HEADER  7,"LITERAL",IMMEDIATE
LITERAL:        jsr     DO_COLON
                dw      DO_LITERAL,DO_LITERAL
                dw      COMMA
                dw      COMMA
                dw      EXIT

DO_LITERAL:
                tdc                             ; Make room on stack
                dec     a
                dec     a
                tcd
                lda     !0,y                    ; Fetch constant from IP
                sta     <1
                iny
                iny
                CONTINUE                        ; Done

; LOOP

                HEADER  4,"LOOP",IMMEDIATE
LOOP:           jsr     DO_COLON
                dw      DO_LITERAL,DO_LOOP
                dw      COMMA
                dw      COMMA
                dw      QUERY_DUP
                dw      QUERY_BRANCH,LOOP_1
                dw      HERE
                dw      SWAP
                dw      STORE
LOOP_1:         dw      EXIT

; (LOOP)

;               HEADER  6,"(LOOP)",NORMAL
DO_LOOP
                lda     1,s                     ; Add one to loop counter
                inc     a
                sta     1,s
                cmp     3,s                     ; Reached limit?
                bcs     DO_LOOP_END             ; Yes
                lda     !0,y                    ; No, branch back to start
                tay
                CONTINUE                        ; Done

DO_LOOP_END:    iny                             ; Skip over address
                iny
                pla                             ; Drop loop variables
                pla
                CONTINUE                        ; Done

; POSTPONE

;   BL WORD FIND
;   DUP 0= ABORT" ?"
;   0< IF   -- xt       non immed: add code to current
;                       def'n to compile xt later.
;       ['] LIT ,XT  ,  add "LIT,xt,COMMAXT"
;       ['] ,XT ,XT     to current definition
;   ELSE  ,XT      immed: compile into cur. def'n
;   THEN ; IMMEDIATE

                HEADER  8,"POSTPONE",IMMEDIATE
POSTPONE:       jsr     DO_COLON
                dw      BL
                dw      WORD
                dw      FIND
                dw      DUP
                dw      ZERO_EQUAL
                dw      DO_S_QUOTE
                db      1,"?"
                dw      QUERY_ABORT
                dw      ZERO_LESS
                dw      QUERY_BRANCH,POSTPONE_1
                dw      DO_LITERAL,DO_LITERAL
                dw      COMMA
                dw      COMMA
                dw      BRANCH,POSTPONE_2
POSTPONE_1:     dw      COMMA
POSTPONE_2:     dw      EXIT

; RECURSE ( -- )

                HEADER  7,"RECURSE",IMMEDIATE
RECURSE:        jsr     DO_COLON
                dw      LATEST
                dw      FETCH
                dw      NFA_TO_CFA
                dw      COMMA
                dw      EXIT

; S"

                LINK    IMMEDIATE
                db      2,"S",'"'
S_QUOTE:        jsr     DO_COLON
                dw      DO_LITERAL,DO_S_QUOTE
                dw      COMMA
                dw      DO_LITERAL,'"'
                dw      WORD
                dw      C_FETCH
                dw      ONE_PLUS
                dw      ALIGNED
                dw      ALLOT
                dw      EXIT

; (S") ( -- c-addr u )

DO_S_QUOTE:
                jsr     DO_COLON
                dw      R_FROM
                dw      COUNT
                dw      TWO_DUP
                dw      PLUS
                dw      ALIGNED
                dw      TO_R
                dw      EXIT

; THEN ( -- )

                HEADER  4,"THEN",IMMEDIATE
THEN:           jsr     DO_COLON
                dw      HERE
                dw      SWAP
                dw      STORE
                dw      EXIT

; UNTIL ( -- )

                HEADER  5,"UNTIL",IMMEDIATE
UNTIL:          jsr     DO_COLON
                dw      DO_LITERAL,QUERY_BRANCH
                dw      COMMA
                dw      COMMA
                dw      EXIT

; USER

                HEADER  4,"USER",NORMAL
USER:           jsr     DO_COLON
                dw      CREATE
                dw      DO_LITERAL,DO_USER
                dw      BUILD
                dw      COMMA
                dw      EXIT

                HEADER  6,"(USER)",NORMAL
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
                CONTINUE                        ; Done

; VARIABLE ( “<spaces>name” -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below. Reserve one
; cell of data space at an aligned address.

                LINK    NORMAL
                db      8,"VARIABLE"
VARIABLE:       jsr     DO_COLON
                dw      CREATE
                dw      DO_LITERAL,DO_VARIABLE
                dw      BUILD
                dw      DO_LITERAL,1
                dw      CELLS
                dw      ALLOT
                dw      EXIT

DO_VARIABLE:
                tdc
                dec     a
                dec     a
                tcd
                pla
                inc     a
                sta     <1
                CONTINUE

; WORDS ( -- )
;
;   LATEST @ BEGIN
;       DUP COUNT TYPE SPACE
;       NFA>LFA @
;   DUP 0= UNTIL
;   DROP ;

                HEADER  5,"WORDS",NORMAL
                jsr     DO_COLON
                dw      LATEST
                dw      FETCH
WORDS_1:        dw      DUP
                dw      COUNT
                dw      TYPE
                dw      SPACE
                dw      NFA_TO_LFA
                dw      FETCH
                dw      DUP
                dw      ZERO_EQUAL
                dw      QUERY_BRANCH,WORDS_1
                dw      DROP
                dw      EXIT

; [
;
; In this implementation it is defined as
;
;   0 STATE !

                HEADER  1,"[",IMMEDIATE
LEFT_BRACKET:   jsr     DO_COLON
                dw      ZERO
                dw      STATE
                dw      STORE
                dw      EXIT

; \ ( -- )
;
; Parse and discard the remainder of the parse area. \ is an immediate word.
;
; In this implementation it is defined as
;
;   1 WORD DROP

                HEADER  1,"\",IMMEDIATE
BACKSLASH:      jsr     DO_COLON
                dw      DO_LITERAL,1
                dw      WORD
                dw      DROP
                dw      EXIT

; ]
;
; In this implementation it is defined as
;
;   -1 STATE !

                HEADER  1,"]",NORMAL
RIGHT_BRACKET:  jsr     DO_COLON
                dw      DO_LITERAL,-1
                dw      STATE
                dw      STORE
                dw      EXIT

;===============================================================================
; I/O Operations
;-------------------------------------------------------------------------------

; CR ( -- )
;
; Cause subsequent output to appear at the beginning of the next line.
;
; In this implementation it is defined as
;
;   13 EMIT 10 EMIT

                HEADER  2,"CR",NORMAL
CR:             jsr     DO_COLON
                dw      DO_LITERAL,13
                dw      EMIT
                dw      DO_LITERAL,10
                dw      EMIT
                dw      EXIT

; EMIT ( x -- )
;
; If x is a graphic character in the implementation-defined character set,
; display x. The effect of EMIT for all other values of x is implementation
; -defined.

                HEADER  4,"EMIT",NORMAL
                extern  UartTx
EMIT:
                lda     <1                      ; Fetch character from stack
                jsr     UartTx                  ; .. and transmit
                tdc
                inc     a                       ; Drop the character
                inc     a
                tcd
                CONTINUE                        ; Done

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
                tax
                tdc
                dec     a                       ; And push to stack
                dec     a
                tcd
                stx     <1
                CONTINUE                        ; Done

; SPACE ( -- )
;
; Display one space.
;
; In this implementation it is defined as
;
;   BL EMIT

                HEADER  5,"SPACE",NORMAL
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

                HEADER  6,"SPACES",NORMAL
SPACES:         jsr     DO_COLON
SPACES_1:       dw      DUP
                dw      ZERO_GREATER
                dw      QUERY_BRANCH,SPACES_2
                dw      SPACE
                dw      ONE_MINUS
                dw      BRANCH,SPACES_1
SPACES_2:       dw      DROP
                dw      EXIT

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
TYPE:           jsr     DO_COLON
                dw      QUERY_DUP
                dw      QUERY_BRANCH,TYPE_2
                dw      OVER
                dw      PLUS
                dw      SWAP
                dw      DO_DO
TYPE_1:         dw      I
                dw      C_FETCH
                dw      EMIT
                dw      DO_LOOP,TYPE_1
                dw      BRANCH,TYPE_3
TYPE_2          dw      DROP
TYPE_3          dw      EXIT

;===============================================================================
; Formatted Output
;-------------------------------------------------------------------------------

; # ( ud1 -- ud2 )
;
; Divide ud1 by the number in BASE giving the quotient ud2 and the remainder n.
; (n is the least-significant digit of ud1.) Convert n to external form and add
; the resulting character to the beginning of the pictured numeric output string.
; An ambiguous condition exists if # executes outside of a <# #> delimited
; number conversion.
;
;       BASE @ >R 0 R@ UM/MOD ROT ROT R> UM/MOD ROT ROT DUP 9 > 7 AND + 30 + HOLD

                HEADER  1,"#",NORMAL
HASH:           jsr     DO_COLON
                dw      BASE
                dw      FETCH
                dw      TO_R
                dw      ZERO
                dw      R_FETCH
                dw      UM_SLASH_MOD
                dw      ROT
                dw      ROT
                dw      R_FROM
                dw      UM_SLASH_MOD
                dw      ROT
                dw      ROT
                dw      DUP
                dw      DO_LITERAL,9
                dw      GREATER
                dw      DO_LITERAL,7
                dw      AND
                dw      PLUS
                dw      DO_LITERAL,'0'
                dw      PLUS
                dw      HOLD
                dw      EXIT

; #> ( xd -- c-addr u )
;
; Drop xd. Make the pictured numeric output string available as a character
; string. c-addr and u specify the resulting character string. A program may
; replace characters within the string.
;
;       2DROP HP @ PAD OVER -

                HEADER  2,"#>",NORMAL
HASH_GREATER:   jsr     DO_COLON
                dw      TWO_DROP
                dw      HP
                dw      FETCH
                dw      PAD
                dw      OVER
                dw      MINUS
                dw      EXIT

; #S ( ud1 -- ud2 )
;
; Convert one digit of ud1 according to the rule for #. Continue conversion
; until the quotient is zero. ud2 is zero. An ambiguous condition exists if #S
; executes outside of a <# #> delimited number conversion.
;
;       BEGIN # 2DUP OR 0= UNTIL

                HEADER  2,"#S",NORMAL
HASH_S:         jsr     DO_COLON
HASH_S_1:       dw      HASH
                dw      TWO_DUP
                dw      OR
                dw      ZERO_EQUAL
                dw      QUERY_BRANCH,HASH_S_1
                dw      EXIT

; . ( n -- )
;
; Display n in free field format.
;
;       <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE

                HEADER  1,".",NORMAL
DOT:            jsr     DO_COLON
                dw      LESS_HASH
                dw      DUP
                dw      ABS
                dw      ZERO
                dw      HASH_S
                dw      ROT
                dw      SIGN
                dw      HASH_GREATER
                dw      TYPE
                dw      SPACE
                dw      EXIT

; <# ( -- )
;
; Initialize the pictured numeric output conversion process.
;
;       PAD HP !

                HEADER  2,"<#",NORMAL
LESS_HASH:      jsr     DO_COLON
                dw      PAD
                dw      HP
                dw      STORE
                dw      EXIT

; HOLD ( char -- )

; Add char to the beginning of the pictured numeric output string. An
; ambiguous condition exists if HOLD executes outside of a <# #> delimited
; number conversion.
;
;       -1 HP +!  HP @ C!

                HEADER  4,"HOLD",NORMAL
HOLD:           jsr     DO_COLON
                dw      DO_LITERAL,-1
                dw      HP
                dw      PLUS_STORE
                dw      HP
                dw      FETCH
                dw      C_STORE
                dw      EXIT

; PAD ( -- c-addr )
;
; c-addr is the address of a transient region that can be used to hold data
; for intermediate processing.

                HEADER  3,"PAD",NORMAL
PAD:            jsr     DO_CONSTANT
                dw      PAD_AREA

; SIGN ( n -- )
;
; If n is negative, add a minus sign to the beginning of the pictured numeric
; output string. An ambiguous condition exists if SIGN executes outside of a
; <# #> delimited number conversion.
;
;       [ HEX ] 0< IF 2D HOLD THEN

                HEADER  4,"SIGN",NORMAL
SIGN:           jsr     DO_COLON
                dw      ZERO_LESS
                dw      QUERY_BRANCH,SIGN_1
                dw      DO_LITERAL,'-'
                dw      HOLD
SIGN_1:         dw      EXIT

; U. ( u -- )
;
; Display u in free field format.
;
;  <# 0 #S #> TYPE SPACE

                HEADER  2,"U.",NORMAL
U_DOT:          jsr     DO_COLON
                dw      LESS_HASH
                dw      ZERO
                dw      HASH_S
                dw      HASH_GREATER
                dw      TYPE
                dw      SPACE
                dw      EXIT

;===============================================================================
; Programming Tools
;-------------------------------------------------------------------------------

; .NYBBLE ( n -- )
;
; Print the least significant nybble of the top value on the stack in hex.

;               HEADER  7,".NYBBLE",NORMAL
DOT_NYBBLE:
                lda     <1
                and     #$000f
                ora     #$0030
                cmp     #$003a
                bcc     $+5
                adc     #$0006
                jsr     UartTx
                jmp     DROP

; .BYTE ( n -- )
;
; Print least significant byte of top value on the stack in hex followed by
; a space.

                HEADER  5,".BYTE",NORMAL
DOT_BYTE:       jsr     DO_COLON
                dw      DUP
                dw      DO_LITERAL,4
                dw      RSHIFT
                dw      DOT_NYBBLE
                dw      DOT_NYBBLE
                dw      SPACE
                dw      EXIT

; .WORD ( n -- )
;
; Print the top value on the stack in hex followed by a space.

                HEADER  5,".WORD",NORMAL
DOT_WORD:       jsr     DO_COLON
                dw      DUP
                dw      DO_LITERAL,12
                dw      RSHIFT
                dw      DOT_NYBBLE
                dw      DUP
                dw      DO_LITERAL,8
                dw      RSHIFT
                dw      DOT_NYBBLE
                dw      DUP
                dw      DO_LITERAL,4
                dw      RSHIFT
                dw      DOT_NYBBLE
                dw      DOT_NYBBLE
                dw      SPACE
                dw      EXIT

; .DP

                HEADER  3,".DP",NORMAL
                jsr     DO_COLON
                dw      AT_DP
                dw      DOT_WORD
                dw      EXIT

                HEADER  3,".RP",NORMAL
                jsr     DO_COLON
                dw      AT_RP
                dw      DOT_WORD
                dw      EXIT

; .S ( -- )
;
; Copy and display the values currently on the data stack. The format of the
; display is implementation-dependent.

                HEADER  2,".S",NORMAL
                jsr     DO_COLON
		dw	DO_LITERAL,'{'
		dw	EMIT
		dw	SPACE
                dw      AT_DP
                dw      ONE_PLUS
                dw      DO_LITERAL,DSTACK_END
                dw      SWAP
                dw      QUERY_DO_DO,DOT_S_2
DOT_S_1:        dw      I
                dw      FETCH
                dw      DOT_WORD
                dw      DO_LITERAL,2
                dw      DO_PLUS_LOOP
                dw      DOT_S_1
DOT_S_2:	dw	DO_LITERAL,'}'
		dw	EMIT
		dw	SPACE
                dw      EXIT

; ? ( a-addr -- )
;
; Display the value stored at a-addr.

                HEADER  1,"?",NORMAL
                jsr     DO_COLON
                dw      FETCH
                dw      DOT_WORD
                dw      EXIT

                HEADER  3,"@DP",NORMAL
AT_DP:
                phd
                tdc
                dec     a
                dec     a
                tcd
                pla
                sta     <1
                CONTINUE

                HEADER  3,"@RP",NORMAL
AT_RP:
                tdc
                dec     a
                dec     a
                tcd
                tsx
                stx     <1
                CONTINUE


;-------------------------------------------------------------------------------

                include "device.asm"

                TRAILER
NEXT_WORD:

                end