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
; This code does not use ACIA interrupts directly by the status register is
; still checked in the interrupt handler. In testing I found that ACIA would
; not work it it was not regularly polled.
;
; A software semaphore cleared by a one short timer (VIA1 T2) is used to work
; around the ACIA transmission silicon bug.
;
; TODO:
;
; No support for BRK in emulation mode.
; Improve data memory bank handling.
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

TIMER_HZ        equ     200                     ; Jiffy timer rate Hz

BAUD_RATE       equ     19200                   ; ACIA baud rate

;------------------------------------------------------------------------------

TMR_COUNT       equ     OSC_FREQ/TIMER_HZ-2

                if      TMR_COUNT&$ffff0000
                messg   "TMR_COUNT does not fit in 16-bits"
                endif

TXD_COUNT       equ     OSC_FREQ/(BAUD_RATE/11)

                if      TXD_DELAY&$ffff0000
                messg   "TXD_DELAY does not fit in 16-bits"
                endif

;==============================================================================
; Data Areas
;------------------------------------------------------------------------------

                data
                org     $200

JIFFY           ds      1                       ; Jiffy counter
TIME            ds      4                       ; Seconds counter

;==============================================================================
; Power On Reset
;------------------------------------------------------------------------------

                code
                extern  Start
RESET:
                lda     VIA1_IER                ; Disable VIA interrupts
                sta     VIA1_IER
                lda     VIA2_IER
                sta     VIA2_IER

                stz     JIFFY                   ; Clear timer counters
                stz     TIME+0
                stz     TIME+1
                stz     TIME+2
                stz     TIME+3

                lda     #%00011111              ; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTL
                lda     #%11001001              ; No parity, no interrupt
                sta     ACIA_CMD
                lda     ACIA_RXD                ; Clear receive buffer

                lda     #%01000000              ; Configure VIA Timers
                sta     VIA1_ACR
                lda     #<TMR_COUNT             ; Set timer counts
                sta     VIA1_T1CL
                lda     #>TMR_COUNT
                sta     VIA1_T1CH
                lda     #%11000000              ; Enable Timer1 interrupt
                sta     VIA1_IER

                lda     #1<<5                   ; Enable Timer2 for one shot
                trb     VIA2_ACR
                jsr     TxDelay                 ; And prime the timer

                cli

                jmp     Start                   ; Jump to the application start

;==============================================================================
; Interrupt Handlers
;------------------------------------------------------------------------------

; Handle IRQ and BRK interrupts in emulation mode.
; -- Currently this code assumes only IRQs will occur

IRQBRK:
                pha                             ; Save callers registers
                phx
                jsr     Service                 ; Service the hardware
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
                phy
                php                             ; Save current MX bits
                short_ai                        ; Make registers 8-bit
                jsr     Service                 ; Service the hardware
                plp                             ; Restore register widths
                ply                             ; .. and values
                plx
                pla
                rti                             ; Done

; Handle IRQ interrupts in native mode.

BRK:
                rti                             ; Loop forever

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
; Interrupt Servicing
;------------------------------------------------------------------------------

Service:
                lda     ACIA_SR                 ; Read ACIA status
                bpl     ACIAHandled

ACIAHandled:

;------------------------------------------------------------------------------

                lda     VIA1_IFR                ; Is VIA1 the source?
                bpl     VIA1Handled             ; No.

                and     #%01000000              ; Is Timer1 the source?
                beq     VIA1T1Handled           ; No

                lda     VIA1_T1CL               ; Clear the interrupt

                inc     JIFFY                   ; Bump jiffy counter
                lda     JIFFY
                cmp     #TIMER_HZ               ; Reached a second?
                bne     VIA1T1Handled           ; No.

                stz     JIFFY                   ; Reset jiffy counter
                inc     TIME+0                  ; And bump main timer
                bne     VIA1T1Handled
                inc     TIME+1
                bne     VIA1T1Handled
                inc     TIME+2
                bne     VIA1T1Handled
                inc     TIME+3
VIA1T1Handled:

VIA1Handled:

;------------------------------------------------------------------------------

                lda     VIA2_IFR                ; Is VIA2 the source?
                bpl     VIA2Handled             ; No.

VIA2Handled:

;------------------------------------------------------------------------------

                rts                             ; Done

;==============================================================================
; Buffered UART Interface
;------------------------------------------------------------------------------

; Adds the characater in A to the transmit buffer and configure T2 for a one
; shot interrupt when its transmission should be complete.

                public  UartTx
UartTx:
                pha                             ; Save callers A
                php                             ; .. and MX bits
                short_a                         ; Ensure 8-bits
                pha
                lda     #1<<5                   ; Has the last character
TxWait:         bit     VIA2_IFR                ; .. been transmitted?
                beq     TxWait
                pla
                sta     ACIA_TXD                ; Transmit the character
                jsr     TxDelay
                plp                             ; Restore flags and A
                pla
                rts                             ; Done

TxDelay:
                lda     #<TXD_COUNT             ; Load transmission oounter
                sta     VIA2_T2CL
                lda     #>TXD_COUNT
                sta     VIA2_T2CH
                rts

; Fetch the next character from the RX buffer waiting for some to arrive if the
; buffer is empty.

                public  UartRx
UartRx:
                php                             ; Save current MX settings
                short_a                         ; .. and ensure 8-bits
RxWait:
                lda     ACIA_SR                 ; Any data in the RX buffer
                and     #$08
                beq     RxWait                  ; No, wait for some
                lda     ACIA_RXD                ; Recover the received data
                plp
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