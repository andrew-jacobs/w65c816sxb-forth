# w65c816-forth
A Direct Threaded Forth for the WDC 65C816

This project builds a Forth environment for a WDC W65C816SXB which can be
accessed via a USB serial connection to the ACIA port. (I use a cheap PL2303
module with jumper wires acquired from ebay).

The code is split into two modules namely:

- The sxb65c02sxb.asm file is a general purpose vector handler and UART
  interrupt handler (to get around hardware problems in the W65C51 ACIA chip).

- The ans-forth.asm file is implements the forth environment.

A number of include files are used to support the code, namely:

- The w65c816.inc file contains useful definitions and macros for the W65C816
  processor.
  
- The w65c816sxb.inc file contains hardware definitions for the WDC W65C816SXB
  development board.

  