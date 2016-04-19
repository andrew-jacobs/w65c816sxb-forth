# w65c816-forth

This project builds a Forth environment for a WDC W65C816SXB which can be
accessed via a USB serial connection to the ACIA port. (I use a cheap PL2303
module with jumper wires acquired from ebay).

The code is split into two modules namely:

- The sxb65c02sxb.asm file is a general purpose vector handler and UART
  interrupt handler (to get around hardware problems in the W65C51 ACIA chip).

- The ans-forth.asm file is implements the forth environment. This file includes
  device.asm to implement hardware specific words.

A number of include files are used to support the code, namely:

- The w65c816.inc file contains useful definitions and macros for the W65C816
  processor.
  
- The w65c816sxb.inc file contains hardware definitions for the WDC W65C816SXB
  development board.

## Current Status

The code in this repository builds a functional Forth environment that can
execute standard commands and compile new words.

```

W65C816SXB ANS-Forth [16.04]

: STAR ( -- ) 42 EMIT ; Ok
: STARS ( n -- ) 0 DO STAR LOOP ; Ok
CR 7 STARS
*******Ok
: SQUARE ( n -- )  DUP 0 DO DUP STARS CR LOOP DROP ; Ok
CR 6 SQUARE CR
******
******
******
******
******
******

Ok
```

I will continue to add documentation, more words and debug.

## Bugs

- The interpreter does not correctly convert signed numbers. Until I have tracked
  this down you have to enter expressions like '7 NEGATE' instead of -7.

- The generation of new lines during command entry needs to be improved.
