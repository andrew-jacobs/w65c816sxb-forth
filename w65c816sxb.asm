;==============================================================================
; __        ____  ____   ____ ___  _  __  ______  ______
; \ \      / / /_| ___| / ___( _ )/ |/ /_/ ___\ \/ / __ )
;  \ \ /\ / / '_ \___ \| |   / _ \| | '_ \___ \\  /|  _ \
;   \ V  V /| (_) |__) | |__| (_) | | (_) |__) /  \| |_) |
;    \_/\_/  \___/____/ \____\___/|_|\___/____/_/\_\____/
;
; Basic Vector Handling for the W65C816SXB Development Board
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
; ACIA interrupts are used to handle recieved serial data but the silicon bug
; prevents them from being used for transmission, instead a VIA timer is used
; trigger periodic buffer checks and to start the transmission of any data.
;
;------------------------------------------------------------------------------

                pw      132
                inclist on

                chip    65816
                longi   off
                longa   off

                include "w65c816.inc"
                include "w65c816sxb.inc"

;==============================================================================
; Configuration
;------------------------------------------------------------------------------

BAUD_RATE       equ     19200
TX_DELAY        equ     OSC_FREQ / BAUD_RATE

RX_SIZE         equ     32
TX_SIZE         equ     32

;==============================================================================
; Data Areas
;------------------------------------------------------------------------------

                page0
                org     $f8


                data
                org     $200

COPV            ds      2
BRKV            ds      2

TX_HEAD         ds      1
TX_TAIL         ds      1
RX_HEAD         ds      1
RX_TAIL         ds      1

TX_BUFF         ds      TX_SIZE
RX_BUFF         ds      RX_SIZE

;==============================================================================
;
;------------------------------------------------------------------------------

                code
                extern  START
RESET:
                stz     TX_HEAD                 ; Reset the TX and RX buffer
                stz     TX_TAIL                 ; .. indexes
                stz     RX_HEAD
                stz     RX_TAIL

                lda     #%00011111              ; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTL
                lda     #%00001001              ; No parity, no interrupt
                sta     ACIA_CMD
                lda     ACIA_RXD                ; Clear receive buffer
                lda     ACIA_RXD

                cli

loop:
                jsr     UART_RX
                jsr     UART_TX
                bra     loop

                jmp     START                   ; Jump to the application start

;==============================================================================
; Interrupt Handlers
;------------------------------------------------------------------------------

; Handle IRQ and BRK interrupts in emulation mode.
; -- Currently this code assumes only IRQs will occur

IRQBRK:
                pha                             ; Save callers registers
                phx
                jsr     SERVICE                 ; Service the hardware
                plx                             ; Restore registers
                pla
                rti                             ; Done

; Handle NMI interrupts in emulation mode.

NMIRQ:
                rti

;------------------------------------------------------------------------------

; Handle IRQ interrupts in native mode.

IRQ:
                pha                             ; Save callers registers
                phx
                php
                short_ai                        ; Make registers 8-bit
                jsr     SERVICE                 ; Service the hardware
                plp                             ; Restore register widths
                plx                             ; .. and values
                pla
                rti                             ; Done

; Handle IRQ interrupts in native mode.

BRK:
                rti

; Handle IRQ interrupts in native mode.

NMI:
                rti

;------------------------------------------------------------------------------

; COP and ABORT interrupts are not handled.

COP:
                bra     $                       ; Loop forever

ABORT:
                bra     $                       ; Loop forever

;==============================================================================
; Interrup Servicing
;------------------------------------------------------------------------------

SERVICE:
                lda     ACIA_SR                 ; Is the ACIA the source?
                bpl     ACIA_HANDLED            ; No, try next device
                and     #$08                    ; Check if RX interrupt
                bne     ACIA_RX                 ; Yes, it is
ACIA_TX:
                ldx     TX_HEAD                 ; Get the head index
                cpx     TX_TAIL                 ; Is the transmit buffer empty?
                beq     TX_EMPTY
                lda     TX_BUFF,x               ; Fetch next character
                sta     ACIA_TXD                ; .. and transmit
                jsr     BUMP_TX                 ; Update the head index
                stx     TX_HEAD
                bra     ACIA_HANDLED            ; Done

TX_EMPTY:
                lda     #%00001001              ; Turn TX interrupts off
                sta     ACIA_CMD
                bra     ACIA_HANDLED            ; Done

ACIA_RX:
                lda     ACIA_RXD
                ldx     RX_TAIL                 ; Save the new character
                sta     RX_BUFF,x
                jsr     BUMP_RX                 ; Bump tail index
                cpx     RX_HEAD                 ; Is the receive buffer full?
                beq     ACIA_HANDLED            ; Yes, drop the character
                stx     RX_TAIL                 ; No, update the tail
ACIA_HANDLED:

;------------------------------------------------------------------------------

PIA_HANDLED:
VIA1_HANDLED:
VIA2_HANDLED:
                rts                             ; Done

;==============================================================================
; Buffered UART Interface
;------------------------------------------------------------------------------

; Adds the characater in A to the transmit buffer and change the ACIA settings
; to enable a transmit interrupt when its is ready to send.

                public  UART_TX
UART_TX:
                pha                             ; Save callers registers
                phx
                php                             ; Save current MX settings
                short_ai                        ; .. and ensure 8-bits
                ldx     TX_TAIL                 ; Save the character
                sta     TX_BUFF,x
                jsr     BUMP_TX                 ; Bump the tail index
TX_WAIT:        cpx     TX_HEAD                 ; Wait until save to update
                beq     TX_WAIT                 ; .. the tail index
                stx     TX_TAIL
                lda     #%00000101              ; Enable transmit interrupt
                sta     ACIA_CMD
                plp                             ; Restore flags
                plx                             ; .. and registers
                pla
                rts                             ; Done

; Fetch the next character from the RX buffer waiting for some to arrive if the
; buffer is empty.

                public  UART_RX
UART_RX:
                phx                             ; Save callers X
                php                             ; Save current MX settings
                short_ai                        ; .. and ensure 8-bits
                ldx     RX_HEAD                 ; Load head index
RX_WAIT:        cpx     RX_TAIL                 ; Any data in the buffer?
                beq     RX_WAIT                 ; No, wait for some
                lda     RX_BUFF,x               ; Yes, fetch it
                jsr     BUMP_RX                 ; Update head index
                stx     RX_HEAD
                plp                             ; Restore flags
                plx                             ; .. and X
                rts                             ; Done

; Increment the transmit buffer index in X and wrap it back to zero if it
; goes past the end.

BUMP_TX:
                inx                             ; Bump the TX index
                cpx     #TX_SIZE                ; Reached the limit?
                bne     $+4                     ; No
                ldx     #0                      ; Yes, wrap around
                rts                             ; Done

; Increment the recieve buffer index in X and wrap it back to zero if it
; goes past the end.

BUMP_RX:
                inx                             ; Bump the RX index
                cpx     #RX_SIZE                ; Reached the limit?
                bne     $+4                     ; No
                ldx     #0                      ; Yes, wrap around
                rts                             ; Done

;==============================================================================
; Reset Vectors
;------------------------------------------------------------------------------

ShadowVectors   section offset $7ee0

                ds      4               ; Reserved
                dw      COP             ; $FFE4 - COP(816)
                dw      BRK             ; $FFE6 - BRK(816)
                dw      ABORT           ; $FFE8 - ABORT(816)
                dw      NMI             ; $FFEA - NMI(816)
                ds      2               ; Reserved
                dw      IRQ             ; $FFEE - IRQ(816)

                ds      4
                dw      COP             ; $FFF4 - COP(C02)
                ds      2               ; $Reserved
                dw      ABORT           ; $FFF8 - ABORT(C02)
                dw      NMIRQ           ; $FFFA - NMI(C02)
                dw      RESET           ; $FFFC - RESET(C02)
                dw      IRQBRK          ; $FFFE - IRQBRK(C02)

                ends

;------------------------------------------------------------------------------

Vectors         section offset $ffe0

                ds      4               ; Reserved
                dw      COP             ; $FFE4 - COP(816)
                dw      BRK             ; $FFE6 - BRK(816)
                dw      ABORT           ; $FFE8 - ABORT(816)
                dw      NMI             ; $FFEA - NMI(816)
                ds      2               ; Reserved
                dw      IRQ             ; $FFEE - IRQ(816)

                ds      4
                dw      COP             ; $FFF4 - COP(C02)
                ds      2               ; $Reserved
                dw      ABORT           ; $FFF8 - ABORT(C02)
                dw      NMIRQ           ; $FFFA - NMI(C02)
                dw      RESET           ; $FFFC - RESET(C02)
                dw      IRQBRK          ; $FFFE - IRQBRK(C02)

                ends

                end