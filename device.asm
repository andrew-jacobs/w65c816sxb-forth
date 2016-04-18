;==============================================================================
;     _    _   _ ____    _____          _   _       _  ___  _  __
;    / \  | \ | / ___|  |  ___|__  _ __| |_| |__   ( )( _ )/ |/ /_
;   / _ \ |  \| \___ \  | |_ / _ \| '__| __| '_ \  |/ / _ \| | '_ \
;  / ___ \| |\  |___) | |  _| (_) | |  | |_| | | |   | (_) | | (_) |
; /_/   \_\_| \_|____/  |_|  \___/|_|   \__|_| |_|    \___/|_|\___/
;
; Device Specific Words for the W65C816SXB
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
;------------------------------------------------------------------------------

; (TITLE) - ( -- )
;

; 		HEADER	7,"(TITLE)",NORMAL
DO_TITLE:	jsr	DO_COLON	
                dw      DO_S_QUOTE
                db      28,"W65C816SXB ANS-Forth [16.04]"
		dw	EXIT

; BYE ( -- )
;
; Return control to the host operating system, if any.

		HEADER	3,"BYE",NORMAL
BYE:
		sei
		cld
		emulate
		jmp	($fffc)		; Reset the processor
		
; UNUSED ( -- u )
;
; u is the amount of space remaining in the region addressed by HERE , in
; address units.

		HEADER	6,"UNUSED",NORMAL
UNUSED:		jsr	DO_COLON
		dw	DO_LITERAL,$7e00
		dw	HERE
		dw	MINUS
		dw	EXIT
		
		
;-------------------------------------------------------------------------------
		
		HEADER	8,"ACIA$RXD",NORMAL
		jsr	DO_CONSTANT
		dw	$7f80
		
		HEADER	8,"ACIA$TXD",NORMAL
		jsr	DO_CONSTANT
		dw	$7f80
		
		HEADER	7,"ACIA$SR",NORMAL
		jsr	DO_CONSTANT
		dw     	$7f81
		
		HEADER	8,"ACIA$CMD",NORMAL
		jsr	DO_CONSTANT
		dw     	$7f82
		
		HEADER	8,"ACIA$CTL",NORMAL
		jsr	DO_CONSTANT
		dw	$7f83

;-------------------------------------------------------------------------------

		HEADER	7,"PIA$PIA",NORMAL
		jsr	DO_CONSTANT
		dw      $7fa0
		
		HEADER	8,"PIA$DDRA",NORMAL
		jsr	DO_CONSTANT
		dw	$7fa0
		
		HEADER	7,"PIA$CRA",NORMAL
		jsr	DO_CONSTANT
		dw      $7fa1
		
		HEADER	7,"PIA$PIB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fa2
		
		HEADER	8,"PIA$DDRB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fa2
		
		HEADER	7,"PIA$CRB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fa3

;-------------------------------------------------------------------------------

		HEADER	8,"VIA1$ORB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc0
		
		HEADER	8,"VIA1$IRB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc0
		
		HEADER	8,"VIA1$ORA",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc1

		HEADER	8,"VIA1$IRA",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc1

		HEADER	9,"VIA1$DDRB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc2
		
		HEADER	9,"VIA1$DDRA",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc3

		HEADER	9,"VIA1$T1CL",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc4
		
		HEADER	9,"VIA1$T1CH",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc5
		
		HEADER	9,"VIA1$T1LL",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc6
		
		HEADER	9,"VIA1$T1LH",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc7
		
		HEADER	9,"VIA1$T2CL",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc8
		
		HEADER	9,"VIA1$T2CH",NORMAL
		jsr	DO_CONSTANT
		dw      $7fc9
		
		HEADER	7,"VIA1$SR",NORMAL
		jsr	DO_CONSTANT
		dw      $7fca
		
		HEADER	8,"VIA1$ACR",NORMAL
		jsr	DO_CONSTANT
		dw      $7fcb
		
		HEADER	8,"VIA1$PCR",NORMAL
		jsr	DO_CONSTANT
		dw      $7fcc
		
		HEADER	8,"VIA1$IFR",NORMAL
		jsr	DO_CONSTANT
		dw      $7fcd
		
		HEADER	8,"VIA1$IER",NORMAL
		jsr	DO_CONSTANT
		dw      $7fce
		
		HEADER	9,"VIA1$ORAN",NORMAL
		jsr	DO_CONSTANT
		dw      $7fcf
		
		HEADER	9,"VIA1$IRAN",NORMAL
		jsr	DO_CONSTANT
		dw      $7fcf

;-------------------------------------------------------------------------------

		
		HEADER	8,"VIA2$ORB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe0
		
		HEADER	8,"VIA2$IRB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe0
		
		HEADER	8,"VIA2$ORA",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe1
		
		HEADER	8,"VIA2$IRA",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe1
		
		HEADER	9,"VIA2$DDRB",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe2
		
		HEADER	9,"VIA2$DDRA",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe3
		
		HEADER	9,"VIA2$T1CL",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe4
		
		HEADER	9,"VIA2$T1CH",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe5
		
		HEADER	9,"VIA2$T1LL",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe6
		
		HEADER	9,"VIA2$T1LH",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe7
		
		HEADER	9,"VIA2$T2CL",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe8
		
		HEADER	9,"VIA2$T2CH",NORMAL
		jsr	DO_CONSTANT
		dw      $7fe9
		
		HEADER	7,"VIA2$SR",NORMAL
		jsr	DO_CONSTANT
		dw      $7fea
		
		HEADER	8,"VIA2$ACR",NORMAL
		jsr	DO_CONSTANT
		dw      $7feb
		
		HEADER	8,"VIA2$PCR",NORMAL
		jsr	DO_CONSTANT
		dw      $7fec
		
		HEADER	8,"VIA2$IFR",NORMAL
		jsr	DO_CONSTANT
		dw      $7fed
		
		HEADER	8,"VIA2$IER",NORMAL
		jsr	DO_CONSTANT
		dw      $7fee
		
		HEADER	9,"VIA2$ORAN",NORMAL
		jsr	DO_CONSTANT
		dw      $7fef
		
		HEADER	9,"VIA2$IRAN",NORMAL
		jsr	DO_CONSTANT
		dw      $7fef
