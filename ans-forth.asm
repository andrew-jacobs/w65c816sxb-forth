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

;==============================================================================
; Data Areas
;------------------------------------------------------------------------------

                page0
                org     $00

WA              ds      2

DSTACK          ds      DSTACK_SIZE

                code
                public  START
START:

                native
                long_ai
                ldx     #$01ff
                txs
                ldx     #DSTACK+DSTACK_SIZE-1

                ldy     #TEST
                jmp     NEXT

TEST:
                dw      BL
                dw      EMIT
                dw      BRANCH
                dw      TEST

;==============================================================================
; System/User Variables
;------------------------------------------------------------------------------


;==============================================================================
;------------------------------------------------------------------------------


BRANCH:
                lda     0,y
                tay
                jmp     NEXT

;==============================================================================
; Constants
;------------------------------------------------------------------------------

; BL

                HEADER  2,"BL",NORMAL,0
BL:
                dex
                dex
                lda     #'.'
                sta     DSTACK+1,x
                jmp     NEXT

; FALSE ( -- false )

                HEADER  5,"FALSE",NORMAL,BL
FALSE:
                dex                             ; Make space on the stack
                dex
                stz     DSTACK+1,X              ; And create a false value
                jmp     NEXT                    ; Done

; TRUE ( -- true )

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
                lda     DSTACK+1,x
                pha
                lda     DSTACK+3,x
                sta     DSTACK+1,x
                pla
                sta     DSTACK+3,x
                jmp     NEXT

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

                HEADER 2,"R>",NORMAL,TO_R
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

; NEGATE

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

;==============================================================================
; Logical Operations
;------------------------------------------------------------------------------

; AND ( x1 x2 -- x3 )

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

;================================================================================
; Control Words
;--------------------------------------------------------------------------------

                HEADER  1,":",NORMAL,XOR
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


; EXIT ( -- ) ( R: nest-sys -- )

                HEADER 4,"EXIT",NORMAL,COLON
EXIT:
                ply
                ply
                jmp     NEXT

;================================================================================
;--------------------------------------------------------------------------------

; EMIT

                HEADER  4,"EMIT",NORMAL,EXIT
                extern  UART_TX
EMIT:
                lda     DSTACK+1,X
                jsr     UART_TX
                inx
                inx
                jmp     NEXT

; KEY 

                HEADER  3,"KEY",NORMAL,EMIT
                extern  UART_RX
KEY:
                jsr     UART_RX
                and     #$00ff
                dex
                dex
                sta     DSTACK+1,x
                jmp     NEXT

;================================================================================
;--------------------------------------------------------------------------------

                end