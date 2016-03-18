;===============================================================================
; Device Specific Words
;-------------------------------------------------------------------------------

TITLE:		jsr	DO_COLON	
                dw      DO_S_QUOTE
                db      28,"W65C816SXB ANS-Forth [16.03]"
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
		
VIA1_IRB        dw      $7fc0
VIA1_ORA        dw      $7fc1
VIA1_IRA        dw      $7fc1
VIA1_DDRB       dw      $7fc2
VIA1_DDRA       dw      $7fc3
VIA1_T1CL       dw      $7fc4
VIA1_T1CH       dw      $7fc5
VIA1_T1LL       dw      $7fc6
VIA1_T1LH       dw      $7fc7
VIA1_T2CL       dw      $7fc8
VIA1_T2CH       dw      $7fc9
VIA1_SR         dw      $7fca
VIA1_ACR        dw      $7fcb
VIA1_PCR        dw      $7fcc
VIA1_IFR        dw      $7fcd
VIA1_IER        dw      $7fce
VIA1_ORAN       dw      $7fcf
VIA1_IRAN       dw      $7fcf

;-------------------------------------------------------------------------------

VIA2_ORB        dw      $7fe0
VIA2_IRB        dw      $7fe0
VIA2_ORA        dw      $7fe1
VIA2_IRA        dw      $7fe1
VIA2_DDRB       dw      $7fe2
VIA2_DDRA       dw      $7fe3
VIA2_T1CL       dw      $7fe4
VIA2_T1CH       dw      $7fe5
VIA2_T1LL       dw      $7fe6
VIA2_T1LH       dw      $7fe7
VIA2_T2CL       dw      $7fe8
VIA2_T2CH       dw      $7fe9
VIA2_SR         dw      $7fea
VIA2_ACR        dw      $7feb
VIA2_PCR        dw      $7fec
VIA2_IFR        dw      $7fed
VIA2_IER        dw      $7fee
VIA2_ORAN       dw      $7fef
VIA2_IRAN       dw      $7fef
