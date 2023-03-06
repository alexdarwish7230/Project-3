#include <P18F4620.inc>
    config OSC = INTIO67
    config WDT = OFF
    config LVP = OFF
    config BOREN = OFF
    InA equ 0x20
    InB equ 0x21
    Result equ 0x22

    ORG 0x0000
START: 
    MOVLW 0x0F;			    Move literal value 0x0F to the W register 
    MOVWF ADCON1;		    ADCON1 = 0x0F
    MOVLW 0xFF ;		    Move literal value 0xFF to the W register 
    MOVWF TRISA ;		    TRISA = 0xFF
    MOVWF TRISB ;		    TRISB = 0xFF
    MOVLW 0x07;			    Move literal value 0x07 to the W register 
    MOVWF TRISD ;		    TRISD = 0x07
    MOVLW 0x00 ;		    Move literal value 0x00 to the W register 
    MOVWF TRISC ;		    TRISC = 0x00
    MOVWF TRISE ;		    TRISE = 0x00     
    InA equ 0x20
    InB equ 0x21
    Result equ 0x22


 
 MAIN_LOOP:
    BTFSC PORTD, 2;		    If bit 'b' in register 'f' is 0 then, the next instruction is skipped.
		   ;		    If bit 'b' is 0, then the next instruction fetched during the current instruction
		   ;		    execution is discarded, and a NOP is executed instead, making this a two-cycle instruction.
    GOTO PORTD2EQ1;		    20-bit value 'PORTD2EQ1' is loaded into PC<20:1>.
    GOTO PORTD2EQ0;		    20-bit value 'PORTD2EQ0' is loaded into PC<20:1>.
    PORTD2EQ1:
	GOTO TASK_BCD;		    20-bit value 'TASK_BCD' is loaded into PC<20:1>.
    PORTD2EQ0:
	BTFSC PORTD, 1;		    If bit 'b' in register 'f' is 0 then, the next instruction is skipped.
		   ;		    If bit 'b' is 0, then the next instruction fetched during the current instruction
		   ;		    execution is discarded, and a NOP is executed instead, making this a two-cycle instruction.
	GOTO PORTD21EQ01;	    20-bit value 'PORTD21EQ01' is loaded into PC<20:1>.
	GOTO PORTD21EQ00;	    20-bit value 'PORTD21EQ00' is loaded into PC<20:1>.
    PORTD21EQ00:
	BTFSC PORTD, 0;		    If bit 'b' in register 'f' is 0 then, the next instruction is skipped.
		      ;		    If bit 'b' is 0, then the next instruction fetched during the current instruction
		      ;		    execution is discarded, and a NOP is executed instead, making this a two-cycle instruction.
	GOTO TASK_ADD;		    20-bit value 'PORTD21EQ01' is loaded into PC<20:1>.
	GOTO TASK_COMP;		    20-bit value 'PORTD21EQ01' is loaded into PC<20:1>.
    PORTD21EQ01:
	BTFSC PORTD, 0;		    If bit 'b' in register 'f' is 0 then, the next instruction is skipped.
		   ;		    If bit 'b' is 0, then the next instruction fetched during the current instruction
		   ;		    execution is discarded, and a NOP is executed instead, making this a two-cycle instruction.
	GOTO TASK_XOR;		    20-bit value 'TASK_XOR' is loaded into PC<20:1>.
	GOTO TASK_AND;		    20-bit value 'TASK_AND' is loaded into PC<20:1>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    TASK_COMP:
	BCF PORTD, 7 ;		    This is to clear the Blue LED of the RGB
	BCF PORTD, 6 ;		    This is to clear the Green LED of the RGB
	BCF PORTD, 5 ;		    This is to clear the LED LED of the RGB
	CALL SUB_COMP;		    Subroutine call of entire 2 Mbyte memory range. First return address (PC+4)
		     ;		    is pushed onto the return stack.
	GOTO MAIN_LOOP;		    20-bit value 'PORTD21EQ01' is loaded into PC<20:1>.
    TASK_ADD:
	BCF PORTD, 7;		    Bit 'b' in register 'PORTD' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	BCF PORTD, 6;		    Bit 'b' in register 'PORTD' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	BSF PORTD, 5;		    Bit 'b' in register 'PORTD' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	CALL SUB_ADD;		    Subroutine call of entire 2 Mbyte memory range. First return address (PC+4)
		     ;		    is pushed onto the return stack.
	GOTO MAIN_LOOP;		    20-bit value 'PORTD21EQ01' is loaded into PC<20:1>.
    TASK_AND:
	BCF PORTD, 7;		    Bit 'b' in register 'PORTD' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
       BSF PORTD, 6;		    Bit 'b' in register 'PORTD' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
       BCF PORTD, 5;		    Bit 'b' in register 'PORTD' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
       CALL SUB_AND;		    Subroutine call of entire 2 Mbyte memory range. First return address (PC+4)
		     ;		    is pushed onto the return stack.
       GOTO MAIN_LOOP;		    20-bit value 'PORTD21EQ01' is loaded into PC<20:1>.
     TASK_XOR:
	BCF PORTD, 7;		    Bit 'b' in register 'PORTD' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	BSF PORTD, 6;		    Bit 'b' in register 'PORTD' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	BSF PORTD, 5;		    Bit 'b' in register 'PORTD' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	CALL SUB_XOR;		    Subroutine call of entire 2 Mbyte memory range. First return address (PC+4)
		     ;		    is pushed onto the return stack.
	GOTO MAIN_LOOP;		    20-bit value 'PORTD21EQ01' is loaded into PC<20:1>.
     TASK_BCD:
	BSF PORTD, 7;		    Bit 'b' in register 'PORTD' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	BCF PORTD, 6;		    Bit 'b' in register 'PORTD' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	BCF PORTD, 5;		    Bit 'b' in register 'PORTD' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	CALL SUB_BCD;		    Subroutine call of entire 2 Mbyte memory range. First return address (PC+4)
		     ;		    is pushed onto the return stack.
	GOTO MAIN_LOOP;		    20-bit value 'PORTD21EQ01' is loaded into PC<20:1>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SUB_COMP:
 
  MOVF PORTA, W;		    Move contents of register 'PORTA' are moved to a destination dependent upon the status of 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF InA;			    Move data from W to register 'InA' 
    COMF InA, W;		    Contents of register 'InA' are complemented and stored in 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF Result;		    Move data from W to register 'InA' 
    MOVFF Result, PORTC;	    Contents of source register 'Result' are moved to destination register 'PORTC'.
    BZ Set_ZFlag1;		    If Zero bit is '1', then program will branch.

    GOTO Clear_ZFlag1;		    20-bit value 'Clear_ZFlag1' is loaded into PC<20:1>.
    Clear_ZFlag1:
	BCF PORTE, 0;		    Bit 'b' in register 'PORTE' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).
	RETURN 
    Set_ZFlag1:
	BSF PORTE,0 ;		    Bit 'b' in register 'PORTE' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).
	
    RETURN
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
SUB_ADD:

    MOVF PORTA, W;		    Move contents of register 'PORTA' are moved to a destination dependent upon the status of 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF InA;			    Move data from W to register 'InA' 
    MOVF PORTB, W;		    Move contents of register 'PORTB' are moved to a destination dependent upon the status of 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF InB;			    Move data from W to register 'InB' 
    
    ADDWF InA, W;		    ADD W to register 'InA'. Stored in WREG.

    MOVWF Result;		    Move data from W to register 'Result' 
    MOVFF Result, PORTC;	    Contents of source register 'Result' are moved to destination register 'PORTC'.
    BZ Set_ZFlag2;		    If Zero bit is '1', then program will branch.
    
    GOTO Clear_ZFlag2;		    20-bit value 'Clear_ZFlag2' is loaded into PC<20:1>.
    Clear_ZFlag2:
	BCF PORTE, 0 ;		    Bit 'b' in register 'PORTE' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).
	RETURN 
    Set_ZFlag2:
	BSF PORTE,0;		    Bit 'b' in register 'PORTE' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).
    RETURN
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SUB_AND:

    MOVF PORTA, W;		    Move contents of register 'PORTA' are moved to a destination dependent upon the status of 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF InA;			    Move data from W to register 'InA' 
    MOVF PORTB, W;		    Move contents of register 'PORTB' are moved to a destination dependent upon the status of 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF InB;			    Move data from W to register 'InB' 
    

    ANDWF InA, W;		    Contents of W are AND'ed with 'InA'. Result stored in WREG.

     MOVWF Result;		    Move data from W to register 'Result' 
    MOVFF Result, PORTC;	    Contents of source register 'Result' are moved to destination register 'PORTC'.
    BZ Set_ZFlag3;		    If Zero bit is '1', then program will branch.

    GOTO Clear_ZFlag3;		    20-bit value 'Clear_ZFlag3' is loaded into PC<20:1>.
    Clear_ZFlag3:
	BCF PORTE, 0 ;		    Bit 'b' in register 'PORTE' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).
	RETURN 
    Set_ZFlag3:
	BSF PORTE,0;		    Bit 'b' in register 'PORTE' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).
    RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SUB_XOR:

   MOVF PORTA, W;		    Move contents of register 'PORTA' are moved to a destination dependent upon the status of 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF InA;			    Move data from W to register 'InA' 
    MOVF PORTB, W;		    Move contents of register 'PORTB' are moved to a destination dependent upon the status of 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF InB;			    Move data from W to register 'InB' 
    
    XORWF InA, W;		    Exclusive OR the conents of W with register 'InA'. Result stored in WREG.

     MOVWF Result;		    Move data from W to register 'Result' 
    MOVFF Result, PORTC;	    Contents of source register 'Result' are moved to destination register 'PORTC'.
    BZ Set_ZFlag4;		    If Zero bit is '1', then program will branch.


    GOTO Clear_ZFlag4;		    20-bit value 'Clear_ZFlag4' is loaded into PC<20:1>.
    Clear_ZFlag4:
	BCF PORTE, 0 ;		    Bit 'b' in register 'PORTE' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).
	RETURN 
    Set_ZFlag4:
	BSF PORTE,0;		    Bit 'b' in register 'PORTE' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).

    RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SUB_BCD: 

  MOVF PORTA, W;		    Move contents of register 'PORTA' are moved to a destination dependent upon the status of 'W'.
    ANDLW 0x0F;			    Contents of W are ANDed with the 8-bit literal '0x0F'. Result is placed in 'W'.
    MOVWF InA;			    Move data from W to register 'InA' 
    
    MOVLW 0x09;			    Move literal value 0x09 to the W register 
    CPFSGT InA,1;		    Compares the contents of data memory location 'InA' to the content of the W by perfoming an 
		;		    unsigned subtraction. If contents of 'InA' are greater than the contents of WREG, then
		;		    the fetched instruction is discarded and a NOP is executed instead, making this a two-cycle instruction.
    GOTO notconversion ;	    20-bit value 'notconversion' is loaded into PC<20:1>.
    GOTO Conversion	;	    20-bit value 'Conversion' is loaded into PC<20:1>.

    
    
    
    Conversion:
	MOVF InA, W;		    Move contents of register 'PORTA' are moved to a destination dependent upon the status of 'W'.
	DAW;			    Adjust the eight-bit value in W, resulting from the earlier addition of two variables (each in packed BCD format)
	    ;			    and produces a correct packed BCD result.
	MOVWF Result;		    Move data from W to register 'InA' 
	MOVFF Result, PORTC;	    Contents of source register 'Result' are moved to destination register 'PORTC'.
	BZ Set_ZFlag5;		    If Zero bit is '1', then program will branch.
	GOTO Clear_ZFlag5 ;	    20-bit value 'Clear_ZFlag5' is loaded into PC<20:1>.
	RETURN
    notconversion:
	MOVFF InA,Result;	    Contents of source register 'InA' are moved to destination register 'Result'.
	MOVFF Result, PORTC;	    Contents of source register 'Result' are moved to destination register 'PORTC'.
	BZ Set_ZFlag5;		    If Zero bit is '1', then program will branch.
	GOTO Clear_ZFlag5  ;	    20-bit value 'Clear_ZFlag5' is loaded into PC<20:1>.
	RETURN
    
    Clear_ZFlag5:
	BCF PORTE, 0;		    Bit 'b' in register 'PORTE' is cleared. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default). 
	RETURN 
    Set_ZFlag5:
	BSF PORTE,0;		    Bit 'b' in register 'PORTE' is set. If 'a' is 0, Access Bank is selected, overriding
		    ;		    BSR value. If 'a' = 1, bank will be selected as per BSR value (default).
	RETURN
	
RETURN

 
 END    
    


