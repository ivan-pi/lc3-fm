#include "LC-3.asm"

  LEA R0, HELLO_STR
  PUTS
  HALT

HELLO_STR:
  #d STRINGZ("Hello World")
