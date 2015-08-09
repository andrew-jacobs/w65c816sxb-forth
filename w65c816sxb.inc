;==============================================================================
; __        ____  ____   ____ ___  _  __  ______  ______  
; \ \      / / /_| ___| / ___( _ )/ |/ /_/ ___\ \/ / __ ) 
;  \ \ /\ / / '_ \___ \| |   / _ \| | '_ \___ \\  /|  _ \ 
;   \ V  V /| (_) |__) | |__| (_) | | (_) |__) /  \| |_) |
;    \_/\_/  \___/____/ \____\___/|_|\___/____/_/\_\____/ 
;                                                         
; Western Design Center W65C816SXB Development Board Hardware Definitions
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
; All of the current stock of W65C51 ACIAs have a silicon bug that causes the
; 'Transmit Data Register Empty' (TDRE) bit in the status register to be stuck
; high making it impossible to tell when the transmitter is ready for the next
; data byte.
;
;------------------------------------------------------------------------------

OSC_FREQ        equ     8000000                 ; SXB runs at 8MHz

;==============================================================================
; W65C21 PIA
;------------------------------------------------------------------------------

;==============================================================================
; W65C22 VIA
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------

;==============================================================================
; W65C51 ACIA
;------------------------------------------------------------------------------

ACIA_RXD        equ     $7f80
ACIA_TXD        equ     $7f80
ACIA_SR         equ     $7f81
ACIA_CMD        equ     $7f82
ACIA_CTL        equ     $7f83