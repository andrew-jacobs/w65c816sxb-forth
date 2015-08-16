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

TIMER_HZ        equ     1000

BAUD_RATE       equ     19200

RX_SIZE         equ     32
TX_SIZE         equ     32

;------------------------------------------------------------------------------

TMR_COUNT       equ     OSC_FREQ/TIMER_HZ

                if      TMR_COUNT&$ffff0000
                messg   "TMR_COUNT does not fit in 16-bits"
                endif

TXD_COUNT       equ     OSC_FREQ/(BAUD_RATE/12)

                if      TXD_DELAY&$ffff0000
                messg   "TXD_DELAY does not fit in 16-bits"
                endif

;==============================================================================
; Data Areas
;------------------------------------------------------------------------------

                data
                org     $200

LOCKS           ds      1

TX_LOCK         equ     $80

;------------------------------------------------------------------------------

ML              ds      1
MH              ds      1
SC              ds      1
MN              ds      1
HR              ds      1

;==============================================================================
; Power On Reset
;------------------------------------------------------------------------------

                code
                extern  Start
RESET:
                stz     LOCKS                   ; Clear lock bits

                lda     #%00011111              ; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTL
                lda     #%00001001              ; No parity, no interrupt
                sta     ACIA_CMD
                lda     ACIA_RXD                ; Clear receive buffer

                lda     VIA1_IER                ; Disable active interrupts
                sta     VIA1_IER
                lda     #%01000000              ; Configure VIA Timers
                sta     VIA1_ACR
                lda     #<TMR_COUNT             ; Set timer counts
                sta     VIA1_T1CL
                lda     #>TMR_COUNT
                sta     VIA1_T1CH
                lda     #%11000000              ; Enable Timer1 interrupt
                sta     VIA1_IER

                lda     VIA2_IER                ; Disable active interrupts
                sta     VIA2_IER
                cli
                native

loop:
                jsr     UartRx
                jsr     UartTx
                bra     loop

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
                php
                short_ai                        ; Make registers 8-bit
                jsr     Service                 ; Service the hardware
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
; Interrupt Servicing
;------------------------------------------------------------------------------

Service:
                lda     VIA1_IFR                ; Is VIA1 the source?
                bmi     $+5
                jmp     VIA1Handled             ; No.

                and     #%01000000              ; Is Timer1 the source?
                beq     VIA1T1Handled           ; No
                lda	VIA1_T1CL               ; Clear the interrupt

                sed
                sec
                lda     ML                      ; Bump lo milliseconds
                adc     #0
                sta     ML
                bcc     TimeUpdated
                lda     MH                      ; Bump hi milliseconds
                adc     #0
                sta     MH
                bcc     TimeUpdated
                lda     SC                      ; Bump seconds
                adc     #0
                cmp     #$60
                bne     $+4
                lda     #0
                sta     SC
                bcc     TimeUpdated
                lda     MN                      ; Bump minutes
                adc     #0
                cmp     #$60
                bne     $+4
                lda     #0
                sta     MN
                bcc     TimeUpdated
                lda     HR                      ; Bump hours
                adc     #0
                cmp     #$60
                bne     $+4
                lda     #0
                sta     HR
                bcc     TimeUpdated
                ; TODO: Bump the rest of the time
TimeUpdated:
                cld
VIA1T1Handled:

                lda     VIA1_IFR                ; Is Timer2 the source
                and     #%00100000
                beq     VIA1T2Handled           ; No.
                lda	VIA1_T2CL               ; Clear the interrupt

                lda     #TX_LOCK                ; Lock TX hardware
                trb     LOCKS
VIA1T2Handled:
VIA1Handled:

;------------------------------------------------------------------------------

VIA2Handled:
                rts                             ; Done

;------------------------------------------------------------------------------

NORM_DAYS:      db      $00,$31,$28,$31,$30,$31,$30,$31
                db      $31,$30,$00,$00,$00,$00,$00,$00
                db      $31,$30,$31

LEAP_DAYS:      db      $00,$31,$29,$31,$30,$31,$30,$31
                db      $31,$30,$00,$00,$00,$00,$00,$00
                db      $31,$30,$31

;==============================================================================
; Buffered UART Interface
;------------------------------------------------------------------------------

; Adds the characater in A to the transmit buffer and change the ACIA settings
; to enable a transmit interrupt when its is ready to send.

                public  UartTx
UartTx:
                pha                             ; Save callers A
                php                             ; .. and MX bits
                short_a                         ; Ensure 8-bits
                pha
                lda     #TX_LOCK                ; Wait if TX operation
TxWait:         bit     LOCKS                   ; .. is still on going
                bne     TxWait
                tsb     LOCKS                   ; Mark TX as in use
                pla
                sta     ACIA_TXD                ; Transmit the character
                lda     #<TXD_COUNT             ; Load transmission oounter
                sta     VIA1_T2CL
                lda     #>TXD_COUNT
                sta     VIA1_T2CH
                lda     #%10100000              ; And allow the interrupt
                sta     VIA1_IER
                plp                             ; Restore flags and A
                pla
                rts                             ; Done

; Fetch the next character from the RX buffer waiting for some to arrive if the
; buffer is empty.

                public  UartRx
UartRx:
                php                             ; Save current MX settings
                short_a                         ; .. and ensure 8-bits
RxWait:
                lda     ACIA_SR
                and     #$08
                beq     RxWait
                lda     ACIA_RXD
                plp
                rts

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