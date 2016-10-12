'5200BAS -- 5200 Basic compiler
'Copyright 2001-2002 by Jeffry Johnston
CONST VERSION$ = "1.97"

'Version History
'1.97   Modified keypad debounce algorithm
'       SCREEN command can now create display lists for ANTIC modes 2-15
'       Work in progress: vector graphics
'1.96   Fixed CLS so it can be used when screen memory is moved
'       Changed VTRIGR vector to $0000 as in the documentation
'       Moved default VDSLST from $BC20 to $BC16 to fill a gap
'       Moved default CHARSET from $A000 to $B800
'       PUT statement can now use the FROMH variable for height
'1.95   Added ability to draw sprites upside down when negative
'       height is specified
'1.94   Changed DLIST to disable ANTIC rather than interrupts
'       Puts double backslashes for TASM style #INCLUDE's
'       Changed PALETTE to accept a number or variable for the color
'1.93   Fixed midline IF THEN GOTO/EXIT bug
'       Fixed KEYPAD bugs
'       Fixed KEYPAD FIX bug
'1.92   Added JOYTRIG2 function for reading the top controller buttons
'       Fixed JOYTRIG bug when using controllers >0
'1.91   Added optional height parameter to PUT statement
'       Added AND and OR operators to IF statement
'1.90   Added missile support to the PUT statement
'       Added EXIT SUB, EXIT SELECT, IF...EXIT SELECT, MISSILES {ON|OFF}
'1.81   Split SOUND command into SOUND and VOLUME.
'       Fixed internal sound channel bug.
'1.80   Modified title screen code with Dennis Debro's updates
'       Added SOUND command
'       Added /M switch
'1.70   Added support for DASM output
'       Now checks numbers to make sure they are in range (0-32767 or 0-255)
'       Added ++ and -- operators (add with carry, subtract with borrow)
'       Added y2k fix and AUTHOR command
'       Moved default display list location
'1.60   Fixed END SUB bug
'       Keypad repeat fixed! KEYPAD FIX updated.
'       Added DIV8, DIV16, MUL8.  Removed DIV.
'       Added SELECT CASE, CASE/CASE_, END SELECT
'       DO/DO_ WHILE/UNTIL, LOOP/LOOP_ WHILE/UNTIL
'       More efficient string storage
'       Added NFLAG, >>=, <<, TRUE/FALSE
'       IF THEN, ELSE, END IF
'1.50   Improved program compile error reporting
'       Added /16 command line option for $8000 offset
'       Fixed line clear in MOVEUP routine
'       Fixed bug in SCREEN(var,var)
'       Fixed OR / EOR bugs
'       Added keypad repeat handling VBI and KEYPAD FIX command
'       (for inclusion with custom VBIs)
'       Added ATTRACT command to reset ATRACT timer
'       INKEY and INPUT now modify X
'1.40   SPRITES {ON|OFF}, SET SPRITES=, INTERNAL, PUT(X,Y),NAME,0-3
'       AXY=JOYTRIG(n), PUSHA=>PUSH ALL, POPA=>POP ALL, PUSH A, POP A
'       removed DLI as a token, DLIST nnnn
'       added {} to PRINT
'       fixed bug in flag EXIT DO/FOR, fixed docs on SCREEN function
'1.30   Custom character set now optional
'       Title screen now optional (TITLE OFF)
'       Fixed keypad code after implementing title screen changes (Cafeman)
'       Fixed TITLE
'       Added PUSHA, POPA
'       Redesign of SUB/END
'       SET option=location [label]
'       SUB xxxx, END SUB
'1.21   Decimal/binary support extended to full 16 bits.
'1.20   Added SCREEN, PALETTE, MEMCOPY, MEMAREA, IF_, FOR_
'       IF CFLAG=x, IF ZFLAG=x, MULADD
'       Fixed * / bug
'1.10a  Added JOYX(), JOYY(), PEEK(), POKE, INKEY, CHBASE, POS (X=row,A=col)
'       Added MOVEUP, PRINT, }
'       Fixed FCN.SCREEN ) bug
'       Fixed carry bug in -
'       Fixed major problems in MULADD, so LOCATE var,var now works
'       Changed LOCATE num,num to be 0 based instead of 1 based
'       Allows for plain 6502 ASM instructions and ASM-style comments
'       PRINT strings are no longer translated to ATASCII
'       Allows for 16-bit DEFINE
'       Split RIGHT/DOWN, UP/LEFT function granularity
'       Compiler messages now go to the Standard Output
'1.01a  Fixed IN/DE bug
'1.00a  Initial release

DEFLNG A-Z
CONST FALSE = 0, TRUE = NOT FALSE
DECLARE SUB ATASCII (A$)
DECLARE FUNCTION BACKSLASH$ (TEXT$)
DECLARE SUB BRANCH (MINUS, VAR1$, SIGN$, VAR2$, DEST$, VAR2TYPE$, COMM$)
DECLARE FUNCTION BYTE$ (A$)
DECLARE SUB CMD.ATTRACT ()
DECLARE SUB CMD.AUTHOR ()
DECLARE SUB CMD.CASE (MINUS)
DECLARE SUB CMD.CHARSET ()
DECLARE SUB CMD.CLS ()
DECLARE SUB CMD.DATA ()
DECLARE SUB CMD.DEFINE ()
DECLARE SUB CMD.DIVIDE8 ()
DECLARE SUB CMD.DIVIDE16 ()
DECLARE SUB CMD.DLIST ()
DECLARE SUB CMD.DO (MINUS)
DECLARE SUB CMD.DOWN ()
DECLARE SUB CMD.ELSE ()
DECLARE SUB CMD.END ()
DECLARE SUB CMD.EXIT ()
DECLARE SUB CMD.FOR (MINUS)
DECLARE SUB CMD.GOSUB ()
DECLARE SUB CMD.GOTO ()
DECLARE SUB CMD.IF (MINUS)
DECLARE SUB CMD.INPUT ()
DECLARE SUB CMD.INTERNAL ()
DECLARE SUB CMD.KEYPAD ()
DECLARE SUB CMD.LEFT ()
DECLARE SUB CMD.LOCATE ()
DECLARE SUB CMD.LOOP (MINUS)
DECLARE SUB CMD.MEMAREA ()
DECLARE SUB CMD.MEMCOPY ()
DECLARE SUB CMD.MISSILES ()
DECLARE SUB CMD.MUL8 ()
DECLARE SUB CMD.MULADD ()
DECLARE SUB CMD.MOVEUP ()
DECLARE SUB CMD.NEXT ()
DECLARE SUB CMD.OPTIONS ()
DECLARE SUB CMD.PALETTE ()
DECLARE SUB CMD.POKE ()
DECLARE SUB CMD.POP ()
DECLARE SUB CMD.POS ()
DECLARE SUB CMD.PRINT ()
DECLARE SUB CMD.PUSH ()
DECLARE SUB CMD.PUT ()
DECLARE SUB CMD.RETURN ()
DECLARE SUB CMD.RIGHT ()
DECLARE SUB CMD.RIGHTBRACE ()
DECLARE SUB CMD.SCREEN ()
DECLARE SUB CMD.SELECT ()
DECLARE SUB CMD.SET ()
DECLARE SUB CMD.SOUND ()
DECLARE SUB CMD.SPRITES ()
DECLARE SUB CMD.SUB ()
DECLARE SUB CMD.TITLE ()
DECLARE SUB CMD.UP ()
DECLARE SUB CMD.VOLUME ()
DECLARE FUNCTION COMMENT$ (N)
DECLARE SUB CONSCOLOR (c)
DECLARE FUNCTION DEC (H$)
DECLARE SUB CMD.DIV8 ()
DECLARE SUB CMD.DIV16 ()
DECLARE SUB ERROROUT (E$)
DECLARE SUB FCN.INKEY ()
DECLARE SUB FCN.JOYTRIG ()
DECLARE SUB FCN.JOYTRIG2 ()
DECLARE SUB FCN.JOYX ()
DECLARE SUB FCN.JOYY ()
DECLARE SUB FCN.PEEK ()
DECLARE SUB FCN.SCREEN ()
DECLARE FUNCTION FINDTOKEN (TEXT$, START)
DECLARE SUB FIXSTR (A$)
DECLARE FUNCTION HEX2$ (DECIMAL)
DECLARE FUNCTION HEX4$ (DECIMAL)
DECLARE SUB INCLUDES (INCLUDE$, OPTIONS$(), DASM, JUSTINC)
DECLARE FUNCTION INVERTSIGN$ (SIGN$)
DECLARE SUB J5 (W$)
DECLARE SUB LDX (W$)
DECLARE FUNCTION PARAM$ (MODE, LINE$)
DECLARE FUNCTION PARSE$ (S$)
DECLARE SUB PRINTASC (S$)
DECLARE SUB PRINTOUT (OPCODE$, OPERANDS$)
DECLARE SUB STX (W$)
DECLARE SUB TOHEX (A$)

'0000-0018      reserved
'0019-00FF      free
'0100-01FF      reserved (stack)
'0200-021B      reserved (shadows)
'021C-3FFF      free
'4000-BFFF      ROM
'4000-xxFF      sprites
'xx00-xxFF      character sets
'xx00-xxxx      code & DLI's
'xxxx-xxxx      display lists
'xxxx-BFE6      keypad IRQ

DIM SHARED TOKEN$(127), OPTIONS$(19, 1)
DIM SHARED DOSTACK(31), FORSTACK(31), SELSTACK(31, 2), IFSTACK(31)
COMMON SHARED LINENUM, J, Q$, DEBUG, OPTIONS, TITLE, SCRMODE, SCRHGT
', VECTGR
COMMON SHARED INCLUDE, INCLUDE$
COMMON SHARED CHAR$, SPRITE$, TITLE$, COPYRIGHT$, ST$
COMMON SHARED DOCOUNT, DODEPTH, FORCOUNT, FORDEPTH, SELCOUNT, SELDEPTH, IFCOUNT, IFDEPTH
'0-6, 7-13, 14-19
DATA "FC03","FCB8","BC20","FEA1","FD02","BC00","0000"
DATA "0000","0000","0000","0000","0000","0000","0000"
DATA "1000","3000","B800","B000","BFB4","BD00"
OPTIONS = 19: FOR T = 0 TO OPTIONS: READ OPTIONS$(T, 0): NEXT T
J = -1: LINENUM = 0: FIRSTORG = TRUE
DOCOUNT = 0: FORCOUNT = 0: SELCOUNT = 0
DODEPTH = 0: FORDEPTH = 0: SELDEPTH = 0
SCRMODE = 2: SCRHGT = 24: TITLE = TRUE: SPRRES = 0
ST$ = "": CHAR$ = "": SPRITE$ = ""
TITLE$ = "UNTITLED": COPYRIGHT$ = "COPYRIGHT 2002 ATARI"
INCLUDE = 23: INCLUDE$ = SPACE$(INCLUDE)
'VECTGR = FALSE

OPEN "CONS:" FOR OUTPUT AS #3
PRINT #3, "5200BAS Basic Compiler, version " + VERSION$ + ".  Copyright 2001-2002 by Jeffry Johnston."
PRINT #3, ""
F$ = RTRIM$(LTRIM$(COMMAND$))
DOT = INSTR(F$, "/16 ")
IF DOT <> 0 THEN
  STARTADDR$ = "$8000": T$ = "16"
  F$ = MID$(F$, 1, DOT - 1) + MID$(F$, DOT + 4)
ELSE
  STARTADDR$ = "$4000": T$ = "32"
END IF
DOT = INSTR(F$, "/D ")
IF DOT <> 0 THEN
  DASM = TRUE
  F$ = MID$(F$, 1, DOT - 1) + MID$(F$, DOT + 3)
ELSE
  DASM = FALSE
END IF
DOT = INSTR(F$, "/M ")
IF DOT <> 0 THEN
  DCOLOR = FALSE
  F$ = MID$(F$, 1, DOT - 1) + MID$(F$, DOT + 3)
ELSE
  DCOLOR = TRUE
END IF
DOT = INSTR(F$, "*"): DEBUG = 0
IF DOT <> 0 THEN DEBUG = 1: F$ = MID$(F$, 1, DOT - 1) + MID$(F$, DOT + 1)
IF F$ = "" THEN
  PRINT #3, "Usage: 5200BAS [/16] [/D] [/M] file[.bas][*]"
  PRINT #3, "  /16  Compile for 16k ROM (default: 32k)"
  PRINT #3, "  /D   Compile for DASM assembler (default: TASM)"
  PRINT #3, "  /M   Don't use ANSI colors in debug output"
  PRINT #3, "  *    Debug output"
  PRINT #3, ""
  PRINT #3, "Error: No input file given"
  END
END IF
DOT = INSTR(F$, ".")
IF DOT = 0 THEN
  F2$ = F$ + ".ASM": F3$ = F$ + ".1": F4$ = F$ + ".2": F$ = F$ + ".BAS"
ELSE
  F2$ = MID$(F$, 1, DOT - 1): F3$ = F2$ + ".1": F4$ = F2$ + ".2"
  F2$ = F2$ + ".ASM"
END IF
IF DASM = TRUE THEN PRINT #3, "Compiling " + T$ + "k ROM for DASM 2.12" ELSE PRINT #3, "Compiling " + T$ + "k ROM for TASM 2.2"
PRINT #3, "Input file  : "; F$
PRINT #3, "Output file : "; F2$
PRINT #3, "Pre-include : "; F4$
PRINT #3, "Post-include: "; F3$
OPEN F$ FOR INPUT AS #1
OPEN F2$ FOR OUTPUT AS #2
CALL PRINTOUT(";========================================================", "~")
T$ = "; ASM Code Produced by 5200BAS" + VERSION$
T = VAL(MID$(TIME$, 1, 2)): T2$ = "a"
IF T > 11 THEN T = T - 12: T2$ = "p"
IF T = 0 THEN T = 12
T3$ = LTRIM$(STR$(T))
IF T > 9 THEN T = 10 ELSE T = 11
T$ = T$ + SPACE$(T - LEN(VERSION$)) + MID$(DATE$, 4, 2)
T$ = T$ + MID$("  JanFebMarAprMayJunJulAugSepOctNovDec", 3 * VAL(MID$(DATE$, 1, 2)), 3)
T$ = T$ + MID$(DATE$, 7, 4) + "  "
CALL PRINTOUT(T$ + T3$ + MID$(TIME$, 3, 3) + T2$, "~")
CALL PRINTOUT(";========================================================", "~")
IF DASM = TRUE THEN CALL PRINTOUT("processor", "6502")
IF STARTADDR$ <> "$4000" THEN
  CALL PRINTOUT(".ORG    $4000", "~")
  CALL PRINTOUT("LDA", "#$00;this is included for the assembler")
END IF
CALL PRINTOUT(".ORG    " + STARTADDR$, "~")
CALL PRINTOUT("ATARI   .EQU    1", "~")
CALL PRINTOUT("#INCLUDE        EQUATES.INC", "~")
CALL PRINTOUT("#INCLUDE        HEADER.INC", "~")
IF DASM = TRUE THEN
  CALL PRINTOUT("#INCLUDE        " + CHR$(34) + F4$ + CHR$(34), "~")
ELSE
  CALL PRINTOUT("#INCLUDE        " + CHR$(34) + BACKSLASH$(F4$) + CHR$(34), "~")
END IF
D0: DO
  IF J >= LEN(Q$) OR J = -1 THEN
    IF EOF(1) THEN EXIT DO
    LINE INPUT #1, A$: A$ = LTRIM$(RTRIM$(A$)) + " ": LINENUM = LINENUM + 1
    IF MID$(A$, 1, 1) = "'" OR MID$(A$, 1, 1) = ";" THEN
      PRINT #2, ";" + MID$(A$, 2)
      J = -1: GOTO D0
    END IF
    IF UCASE$(MID$(A$, 1, 3)) = "RAW" THEN
      PRINT #2, SPACE$(8); LTRIM$(MID$(A$, 4)): J = -1: GOTO D0
    END IF
    ERASE TOKEN$
    Q$ = PARSE$(A$): J = 0
  END IF
  IF DEBUG = 1 THEN PRINT #3, MID$(Q$, J + 1); " ";
  SELECT CASE MID$(Q$, J + 1, 1)
  CASE "E" 'END OF LINE
    J = -1
  CASE ":" ':
    J = J + 1
  CASE "L" 'LABEL
    CALL PRINTOUT(TOKEN$(J), "~")
    J = J + 1
  CASE "R"
    T = 8
    IF MID$(A$, 1, 1) = "#" THEN T = 0
    IF MID$(A$, 2, 3) = "ORG" THEN
      IF FIRSTORG = TRUE THEN
        CALL PRINTOUT("ENDMAIN JMP     ENDMAIN         ;Endless loop", "~")
        IF DASM = TRUE THEN
          CALL PRINTOUT("#INCLUDE        " + CHR$(34) + F3$ + CHR$(34), "~")
        ELSE
          CALL PRINTOUT("#INCLUDE        " + CHR$(34) + BACKSLASH$(F3$) + CHR$(34), "~")
        END IF
        FIRSTORG = FALSE
      END IF
      T = 0
    END IF
    PRINT #2, SPACE$(T); A$: J = -1: GOTO D0
  CASE "V" 'VARIABLE
    IF TOKEN$(J + 1) <> "=" THEN CALL ERROROUT("Missing =")
    IF TOKEN$(J) <> TOKEN$(J + 2) THEN
      SELECT CASE TOKEN$(J + 2)
      CASE "JOYTRIG2": CALL FCN.JOYTRIG2
      CASE "JOYTRIG(": CALL FCN.JOYTRIG

      CASE "SCREEN(": CALL FCN.SCREEN
      CASE "JOYX(": CALL FCN.JOYX
      CASE "JOYY(": CALL FCN.JOYY
      CASE "PEEK(": CALL FCN.PEEK
      CASE "INKEY": CALL FCN.INKEY
      CASE ELSE
        SELECT CASE TOKEN$(J)
        CASE "A", "X", "Y"
          J = J + 2: CALL LDX(TOKEN$(J - 2)): J = J + 1
        CASE ELSE
          SELECT CASE TOKEN$(J + 2)
          CASE "A", "X", "Y"
            CALL PRINTOUT("ST" + TOKEN$(J + 2), TOKEN$(J) + COMMENT$(3))
            J = J + 3
          CASE ELSE
            J = J + 2: CALL LDX("A"): J = J - 2
            CALL PRINTOUT("STA", TOKEN$(J) + COMMENT$(3)): J = J + 3
          END SELECT
        END SELECT
      END SELECT
    ELSE
      SELECT CASE TOKEN$(J + 3)
      CASE "+"
        IF TOKEN$(J + 4) = "0001" AND TOKEN$(J) <> "A" THEN
          SELECT CASE TOKEN$(J)
          CASE "X", "Y"
            CALL PRINTOUT("IN" + TOKEN$(J), COMMENT$(5)): J = J + 5
          CASE ELSE
            CALL PRINTOUT("INC", TOKEN$(J) + COMMENT$(5)): J = J + 5
          END SELECT
        ELSE
          CALL PRINTOUT("CLC", "")
          CALL J5("ADC")
        END IF
      CASE "-"
        IF TOKEN$(J + 4) = "0001" AND TOKEN$(J) <> "A" THEN
          SELECT CASE TOKEN$(J)
          CASE "X", "Y"
            CALL PRINTOUT("DE" + TOKEN$(J), COMMENT$(5)): J = J + 5
          CASE ELSE
            CALL PRINTOUT("DEC", TOKEN$(J) + COMMENT$(5)): J = J + 5
          END SELECT
        ELSE
          CALL PRINTOUT("SEC", "")
          CALL J5("SBC")
        END IF
      CASE "++"
        IF TOKEN$(J) <> "A" THEN CALL ERROROUT("Type mismatch")
        CALL J5("ADC")
      CASE "--"
        IF TOKEN$(J) <> "A" THEN CALL ERROROUT("Type mismatch")
        CALL J5("SBC")
      CASE "*"
        IF TOKEN$(J) <> "A" THEN CALL ERROROUT("Type mismatch")
        IF MID$(Q$, J + 5, 1) <> "N" THEN CALL ERROROUT("Type mismatch")
        T = LOG(DEC(TOKEN$(J + 4))) / LOG(2)
        FOR T2 = 1 TO T
          IF T2 = 1 THEN
            IF DASM = FALSE THEN
              CALL PRINTOUT("ASL", "A" + COMMENT$(5))
            ELSE
              CALL PRINTOUT("ASL", COMMENT$(5))
            END IF
          ELSE
            IF DASM = FALSE THEN
              CALL PRINTOUT("ASL", "A")
            ELSE
              CALL PRINTOUT("ASL", "")
            END IF
          END IF
        NEXT T2
        J = J + 5
      CASE "/"
        IF TOKEN$(J) <> "A" THEN CALL ERROROUT("Type mismatch")
        IF MID$(Q$, J + 5, 1) <> "N" THEN CALL ERROROUT("Type mismatch")
        T = LOG(DEC(TOKEN$(J + 4))) / LOG(2)
        FOR T2 = 1 TO T
          IF T2 = 1 THEN
            IF DASM = FALSE THEN
              CALL PRINTOUT("LSR", "A" + COMMENT$(5))
            ELSE
              CALL PRINTOUT("LSR", COMMENT$(5))
            END IF
          ELSE
            IF DASM = FALSE THEN
              CALL PRINTOUT("LSR", "A")
            ELSE
              CALL PRINTOUT("LSR", "")
            END IF
          END IF
        NEXT T2
        J = J + 5
      CASE "AND"
        CALL J5("AND")
      CASE "OR"
        CALL J5("ORA")
      CASE "XOR"
        CALL J5("EOR")
      CASE ELSE
        CALL ERROROUT("Invalid operator")
      END SELECT
    END IF
  CASE "F" 'FUNCTION
    CALL ERROROUT("Illegal function call")
  CASE "C" 'COMMAND
    SELECT CASE TOKEN$(J)
    CASE "SOUND": CALL CMD.SOUND
    CASE "VOLUME": CALL CMD.VOLUME
    CASE "MUL8": CALL CMD.MUL8
    CASE "DIV8": CALL CMD.DIV8
    CASE "DIV16": CALL CMD.DIV16
    CASE "ATTRACT": CALL CMD.ATTRACT
    CASE "DLIST": CALL CMD.DLIST
    CASE "PUT": CALL CMD.PUT
    CASE "INTERNAL": CALL CMD.INTERNAL
    CASE "SPRITES": CALL CMD.SPRITES
    CASE "MISSILES": CALL CMD.MISSILES
    CASE "SET": CALL CMD.SET
    CASE "PUSH": CALL CMD.PUSH
    CASE "POP": CALL CMD.POP
    CASE "MULADD": CALL CMD.MULADD
    CASE "MEMAREA": CALL CMD.MEMAREA
    CASE "MEMCOPY": CALL CMD.MEMCOPY
    CASE "PALETTE": CALL CMD.PALETTE
    CASE "SCREEN": CALL CMD.SCREEN
    CASE "}": CALL CMD.RIGHTBRACE
    CASE "MOVEUP": CALL CMD.MOVEUP
    CASE "POS": CALL CMD.POS
    CASE "CHARSET": CALL CMD.CHARSET
    CASE "POKE": CALL CMD.POKE
    CASE "INPUT": CALL CMD.INPUT
    CASE "KEYPAD": CALL CMD.KEYPAD
    CASE "DEFINE": CALL CMD.DEFINE
    CASE "DATA": CALL CMD.DATA
    CASE "CLS": CALL CMD.CLS
    CASE "LOCATE": CALL CMD.LOCATE
    CASE "RIGHT": CALL CMD.RIGHT
    CASE "LEFT": CALL CMD.LEFT
    CASE "DOWN": CALL CMD.DOWN
    CASE "UP": CALL CMD.UP
    CASE "PRINT": CALL CMD.PRINT
    CASE "TITLE": CALL CMD.TITLE
    CASE "AUTHOR": CALL CMD.AUTHOR
    CASE "IF": CALL CMD.IF(FALSE)
    CASE "IF_": CALL CMD.IF(TRUE)
    CASE "ELSE": CALL CMD.ELSE
    CASE "GOTO": CALL CMD.GOTO
    CASE "GOSUB": CALL CMD.GOSUB
    CASE "RETURN": CALL CMD.RETURN
    CASE "SUB": CALL CMD.SUB
    CASE "END": CALL CMD.END
    CASE "SELECT": CALL CMD.SELECT
    CASE "CASE_": CALL CMD.CASE(TRUE)
    CASE "CASE": CALL CMD.CASE(FALSE)
    CASE "DO": CALL CMD.DO(FALSE)
    CASE "DO_": CALL CMD.DO(TRUE)
    CASE "LOOP": CALL CMD.LOOP(FALSE)
    CASE "LOOP_": CALL CMD.LOOP(TRUE)
    CASE "EXIT": CALL CMD.EXIT
    CASE "FOR": CALL CMD.FOR(FALSE)
    CASE "FOR_": CALL CMD.FOR(TRUE)
    CASE "NEXT": CALL CMD.NEXT
    CASE ELSE
      CALL ERROROUT("Unknown command")
    END SELECT
  CASE ELSE
    CALL ERROROUT("Syntax error")
  END SELECT
LOOP
IF FIRSTORG = TRUE THEN
  CALL PRINTOUT("ENDMAIN JMP     ENDMAIN         ;Endless loop", "~")
  IF DASM = TRUE THEN
    CALL PRINTOUT("#INCLUDE        " + CHR$(34) + F3$ + CHR$(34), "~")
  ELSE
    CALL PRINTOUT("#INCLUDE        " + CHR$(34) + BACKSLASH$(F3$) + CHR$(34), "~")
  END IF
END IF
CALL PRINTOUT("", "~")
CALL PRINTOUT(";-----------------------------------------------------------", "~")
CALL PRINTOUT("; Strings", "~")
CALL PRINTOUT(";-----------------------------------------------------------", "~")
CALL PRINTOUT(".ORG    $" + OPTIONS$(17, 0), "~")
IF DEBUG = 1 THEN PRINT #3, "": CALL CONSCOLOR(15): PRINT #3, CHR$(34) + ST$ + CHR$(34): CALL CONSCOLOR(7)
CALL PRINTASC(ST$)
IF OPTIONS$(5, 1) = "" THEN
  CALL INCLUDES(INCLUDE$, OPTIONS$(), DASM, 4) 'KEYPAD
END IF
IF OPTIONS$(2, 1) = "" THEN
  CALL INCLUDES(INCLUDE$, OPTIONS$(), DASM, 3) 'DEFERVBI
END IF
'IF OPTIONS$(6, 1) = "" THEN
'  CALL INCLUDES(INCLUDE$, OPTIONS$(), DASM, 5) 'JOYSTICK
'END IF
'IF VECTGR = TRUE THEN
'  CALL INCLUDES(INCLUDE$, OPTIONS$(), DASM, 6) 'VECTTBL
'END IF
IF OPTIONS$(18, 1) = "" THEN
  CALL PRINTOUT("", "~")
  CALL PRINTOUT(";-----------------------------------------------------------", "~")
  CALL PRINTOUT("; Display List", "~")
  CALL PRINTOUT(";-----------------------------------------------------------", "~")
  CALL PRINTOUT(".ORG    $" + OPTIONS$(18, 0), "~")
  CALL PRINTOUT(".WORD", "$7070;skip 24 scan lines")
  CALL PRINTOUT(".BYTE", "$70")
  CALL PRINTOUT(".BYTE", "$" + HEX2$(&H40 + SCRMODE) + ";set up ANTIC " + HEX$(SCRMODE) + " screen")
  CALL PRINTOUT(".WORD", "$" + OPTIONS$(14, 0) + ";address of screen memory")
  FOR A = 1 TO SCRHGT - 1
    CALL PRINTOUT(".BYTE", "$" + HEX2$(SCRMODE))
    IF A = 101 AND SCRMODE >= 14 THEN
      CALL PRINTOUT(".BYTE", "$" + HEX2$(&H40 + SCRMODE) + ";2nd part of ANTIC " + HEX$(SCRMODE) + " screen")
      CALL PRINTOUT(".WORD", "$" + HEX4$(DEC(OPTIONS$(14, 0)) + &H1000) + ";address of screen memory")
    END IF
  NEXT A
  CALL PRINTOUT(".BYTE", "$41")
  CALL PRINTOUT(".WORD", "$" + OPTIONS$(18, 0) + ";jump back to top of list")
END IF
CALL PRINTOUT("", "~")
CALL PRINTOUT(";-----------------------------------------------------------", "~")
CALL PRINTOUT("; Monitor Information", "~")
CALL PRINTOUT(";-----------------------------------------------------------", "~")
IF TITLE = TRUE THEN
  CALL PRINTOUT(".ORG    $BFD4", "~")
  T$ = SPACE$(20)
  MID$(T$, 10 - LEN(COPYRIGHT$) / 2 + 1, LEN(COPYRIGHT$)) = COPYRIGHT$
  CALL ATASCII(T$): CALL PRINTASC(T$)
  T$ = SPACE$(20)
  MID$(T$, 10 - LEN(TITLE$) / 2 + 1, LEN(TITLE$)) = TITLE$
  CALL ATASCII(T$): CALL PRINTASC(T$)
  CALL PRINTOUT(".BYTE", "$B5")
END IF
CALL PRINTOUT(".ORG    $BFFD", "~")
CALL PRINTOUT(".BYTE", "$FF" + "; Skip title")
CALL PRINTOUT(".WORD", STARTADDR$ + ";MAIN")
CALL PRINTOUT(".END", "~")
CLOSE #2
OPEN F3$ FOR OUTPUT AS #2
CALL INCLUDES(INCLUDE$, OPTIONS$(), DASM, 0)
IF TITLE = TRUE THEN
  CALL INCLUDES(INCLUDE$, OPTIONS$(), DASM, 1) 'Y2K
END IF
CALL INCLUDES(INCLUDE$, OPTIONS$(), DASM, 2) 'CLEARRAM
CALL PRINTOUT(";-----------------------------------------------------------", "~")
CALL PRINTOUT("; IRQ VECTORS", "~")
CALL PRINTOUT(";-----------------------------------------------------------", "~")
CALL PRINTOUT("OPTIONS:", "~")
FOR T = 0 TO 13
  CALL PRINTOUT(".WORD   $" + OPTIONS$(T, 0), "~")
NEXT T
IF OPTIONS$(16, 1) = "" THEN
  CALL PRINTOUT("", "~")
  CALL PRINTOUT(".ORG    $" + OPTIONS$(16, 0), "~")
  CALL PRINTOUT("#INCLUDE        ASCIISET.INC", "~")
END IF
CLOSE #2
OPEN F4$ FOR OUTPUT AS #2
CALL PRINTOUT(";-----------------------------------------------------------", "~")
CALL PRINTOUT("; INIT", "~")
CALL PRINTOUT(";-----------------------------------------------------------", "~")
IF TITLE = TRUE THEN
  CALL PRINTOUT("TITLE   .EQU    1", "~")
ELSE
  CALL PRINTOUT("TITLE   .EQU    0", "~")
END IF
CALL PRINTOUT("LDA", "#$" + MID$(OPTIONS$(18, 0), 1, 2) + "; Display List high")
CALL PRINTOUT("STA", "SDLSTH")
CALL PRINTOUT("LDA", "#$" + MID$(OPTIONS$(18, 0), 3, 2) + "; Display List low")
CALL PRINTOUT("STA", "SDLSTL")
IF OPTIONS$(5, 1) = "" THEN
  'IF OPTIONS$(6, 1) = "" THEN
  '  CALL PRINTOUT("LDA", "#$C0; Enable deferred keyboard & trigger")
  'ELSE
    CALL PRINTOUT("LDA", "#$40; Enable deferred keyboard")
  'END IF
  CALL PRINTOUT("STA", "POKMSK")
  CALL PRINTOUT("STA", "IRQEN")
'ELSE
  'IF OPTIONS$(6, 1) = "" THEN
  '  CALL PRINTOUT("LDA", "#$80; Enable joystick trigger")
  '  CALL PRINTOUT("STA", "POKMSK")
  '  CALL PRINTOUT("STA", "IRQEN")
  'END IF
END IF
CALL PRINTOUT("LDA", "#$" + MID$(OPTIONS$(16, 0), 1, 2))
CALL PRINTOUT("STA", "CHBASE")
CALL PRINTOUT("LDA", "#$22")
CALL PRINTOUT("STA", "SDMCTL")
IF OPTIONS$(3, 1) <> "" THEN
  CALL PRINTOUT("LDA", "#$C0 ; Enable DLI & VBI")
  CALL PRINTOUT("STA", "NMIEN")
ELSE
  CALL PRINTOUT("LDA", "#$40 ; Enable VBI")
  CALL PRINTOUT("STA", "NMIEN")
END IF
CALL PRINTOUT("LDA", "#$" + MID$(OPTIONS$(14, 0), 1, 2))
CALL PRINTOUT("STA", "LOCATEH")
CALL PRINTOUT("LDA", "#$0E")
CALL PRINTOUT("STA", "COLOR0")
CALL PRINTOUT("STA", "COLOR1")
CALL PRINTOUT("LDA", "#$" + MID$(OPTIONS$(15, 0), 1, 2) + "; Sprite page")
CALL PRINTOUT("STA", "PMBASE")
CALL PRINTOUT("LDA", "#$03")
CALL PRINTOUT("STA", "GRACTL")
CALL PRINTOUT("CLI", "; Enable Interrupts")
CLOSE #2
PRINT #3, ""
PRINT #3, "Done."
END

'convert ASCII string to ATASCII
SUB ATASCII (A$)
  FOR A = 1 TO LEN(A$)
    B = ASC(MID$(A$, A, 1))
    SELECT CASE B
    CASE 32 TO 95
      B = B + 32
    CASE 96
      B = 71
    CASE 97 TO 122
      B = B - 64
    CASE 126
      B = 141
    END SELECT
    MID$(A$, A, 1) = CHR$(B)
  NEXT A
END SUB

'          TOKEN$: J+1    J+2    J+3    J+6    Q$@J+4     COMMENT$(7)
SUB BRANCH (MINUS, VAR1$, SIGN$, VAR2$, DEST$, VAR2TYPE$, COMM$)
  SELECT CASE VAR1$ + SIGN$
  CASE "CFLAG="
    SELECT CASE VAR2$
    CASE "0000", "FALSE"
      GOTO CARRYCLEAR
    CASE "0001", "TRUE"
      GOTO CARRYSET
    CASE ELSE
      CALL ERROROUT("Test must be 0 or 1")
    END SELECT
  CASE "CFLAG<>"
    SELECT CASE VAR2$
    CASE "0000", "FALSE"
      GOTO CARRYSET
    CASE "0001", "TRUE"
      GOTO CARRYCLEAR
    CASE ELSE
      CALL ERROROUT("Test must be 0 or 1")
    END SELECT
  CASE "ZFLAG="
    SELECT CASE VAR2$
    CASE "0000", "FALSE"
      GOTO ZEROCLEAR
    CASE "0001", "TRUE"
      GOTO ZEROSET
    CASE ELSE
      CALL ERROROUT("Test must be 0 or 1")
    END SELECT
  CASE "ZFLAG<>"
    SELECT CASE VAR2$
    CASE "0000", "FALSE"
      GOTO ZEROSET
    CASE "0001", "TRUE"
      GOTO ZEROCLEAR
    CASE ELSE
      CALL ERROROUT("Test must be 0 or 1")
    END SELECT
  CASE "NFLAG="
    SELECT CASE VAR2$
    CASE "0000", "FALSE"
      GOTO NEGCLEAR
    CASE "0001", "TRUE"
      GOTO NEGSET
    CASE ELSE
      CALL ERROROUT("Test must be 0 or 1")
    END SELECT
  CASE "NFLAG<>"
    SELECT CASE VAR2$
    CASE "0000", "FALSE"
      GOTO NEGSET
    CASE "0001", "TRUE"
      GOTO NEGCLEAR
    CASE ELSE
      CALL ERROROUT("Test must be 0 or 1")
    END SELECT
  END SELECT
  T = INSTR("AXY", VAR1$) - 1
  IF T = -1 THEN CALL ERROROUT("A/X/Y expected")
  T$ = MID$("CMPCPXCPY", T * 3 + 1, 3)
  IF VAR2TYPE$ = "N" THEN
    CALL PRINTOUT(T$, "#$" + BYTE$(VAR2$) + COMM$): COMM$ = ""
  ELSE
    IF VAR2TYPE$ <> "V" THEN CALL ERROROUT("Type mismatch")
    SELECT CASE VAR2$
    CASE "A", "X", "Y"
      CALL ERROROUT("Illegal variable A/X/Y")
    CASE ELSE
      CALL PRINTOUT(T$, VAR2$ + COMM$): COMM$ = ""
    END SELECT
  END IF
  SELECT CASE SIGN$
  CASE "="  'BEQ LABEL
ZEROSET:
    IF MINUS = FALSE THEN
      CALL PRINTOUT("BEQ", DEST$ + COMM$)
    ELSE
      CALL PRINTOUT(".WORD", "$03D0;BNE past JMP")
      CALL PRINTOUT("JMP", DEST$ + COMM$)
    END IF
  CASE "<>" 'BNE LABEL
ZEROCLEAR:
    IF MINUS = FALSE THEN
      CALL PRINTOUT("BNE", DEST$ + COMM$)
    ELSE
      CALL PRINTOUT(".WORD", "$03F0;BEQ past JMP")
      CALL PRINTOUT("JMP", DEST$ + COMM$)
    END IF
  CASE "<"  'BCC LABEL
CARRYCLEAR:
    IF MINUS = FALSE THEN
      CALL PRINTOUT("BCC", DEST$ + COMM$)
    ELSE
      CALL PRINTOUT(".WORD", "$03B0;BCS past JMP")
      CALL PRINTOUT("JMP", DEST$ + COMM$)
    END IF
  CASE ">"  '.WORD $0290,BNE LABEL
    IF MINUS = FALSE THEN
      CALL PRINTOUT(".WORD", "$0290;BCC past BNE")
      CALL PRINTOUT("BNE", DEST$)
    ELSE
      CALL PRINTOUT(".WORD", "$0590;BCC past JMP")
      CALL PRINTOUT(".WORD", "$03F0;BEQ past JMP")
      CALL PRINTOUT("JMP", DEST$)
    END IF
  CASE "<=" 'BCC LABEL,BEQ LABEL
    IF MINUS = FALSE THEN
      CALL PRINTOUT("BCC", DEST$)
      CALL PRINTOUT("BEQ", DEST$)
    ELSE
      CALL PRINTOUT(".WORD", "$03B0;BCS past JMP")
      CALL PRINTOUT("JMP", DEST$)
      CALL PRINTOUT(".WORD", "$03D0;BNE past JMP")
      CALL PRINTOUT("JMP", DEST$)
    END IF
  CASE ">=" 'BCS LABEL
CARRYSET:
    IF MINUS = FALSE THEN
      CALL PRINTOUT("BCS", DEST$ + COMM$)
    ELSE
      CALL PRINTOUT(".WORD", "$0390;BCC past JMP")
      CALL PRINTOUT("JMP", DEST$ + COMM$)
    END IF
  CASE "<<" 'BMI LABEL
NEGSET:
    IF MINUS = FALSE THEN
      CALL PRINTOUT("BMI", DEST$ + COMM$)
    ELSE
      CALL PRINTOUT(".WORD", "$0310;BPL past JMP")
      CALL PRINTOUT("JMP", DEST$ + COMM$)
    END IF
  CASE ">>=" 'BPL LABEL
NEGCLEAR:
    IF MINUS = FALSE THEN
      CALL PRINTOUT("BPL", DEST$ + COMM$)
    ELSE
      CALL PRINTOUT(".WORD", "$0330;BMI past JMP")
      CALL PRINTOUT("JMP", DEST$ + COMM$)
    END IF
  CASE ELSE
    CALL ERROROUT("Invalid compare")
  END SELECT
END SUB

FUNCTION BYTE$ (A$)
  A = DEC(A$)
  IF A > 255 THEN CALL ERROROUT("Number too large")
  BYTE$ = HEX2$(A)
END FUNCTION

SUB CMD.ATTRACT
  CALL PRINTOUT("LDA", "#$00" + COMMENT$(1))
  CALL PRINTOUT("STA", "ATRACT")
  J = J + 1
END SUB

SUB CMD.AUTHOR
  SELECT CASE MID$(Q$, J + 2, 1)
  CASE "S"
    COPYRIGHT$ = TOKEN$(J + 1)
    IF LEN(TITLE$) > 20 THEN CALL ERROROUT("AUTHOR too long (20 chars max)")
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
  J = J + 2
END SUB

SUB CMD.CASE (MINUS)
  T$ = TOKEN$(J + 1)
  SELECT CASE T$
  CASE "A", "X", "Y"
    CALL ERROROUT("Illegal variable A/X/Y")
  END SELECT
  IF SELDEPTH < 1 THEN CALL ERROROUT("CASE outside SELECT CASE")
  IF SELSTACK(SELDEPTH - 1, 2) > SELSTACK(SELDEPTH - 1, 0) THEN
    CALL PRINTOUT("JMP", "SC" + LTRIM$(STR$(SELSTACK(SELDEPTH - 1, 0))))
    T2$ = LTRIM$(STR$(SELSTACK(SELDEPTH - 1, 2)))
    CALL PRINTOUT("SC" + T2$ + ":", "~")
  END IF
  IF MID$(Q$, J + 2, 1) = "N" THEN T$ = "#$" + BYTE$(T$)
  SELCOUNT = SELCOUNT + 1: SELSTACK(SELDEPTH - 1, 2) = SELCOUNT
  IF T$ <> "ELSE" THEN
    T2$ = MID$("CMPCPXCPY", SELSTACK(SELDEPTH - 1, 1) * 3 - 2, 3)
    CALL PRINTOUT(T2$, T$ + COMMENT$(2))
    T$ = LTRIM$(STR$(SELCOUNT))

    IF MINUS = FALSE THEN
      CALL PRINTOUT("BNE", "SC" + T$)
    ELSE
      CALL PRINTOUT(".WORD", "$03F0;BEQ past JMP")
      CALL PRINTOUT("JMP", "SC" + T$)
    END IF
  END IF
  J = J + 2
END SUB

SUB CMD.CHARSET
  IF MID$(Q$, J + 2, 1) <> "N" THEN CALL ERROROUT("Type mismatch")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 1), 1, 2) + COMMENT$(2))
  CALL PRINTOUT("STA", "CHBASE")
  J = J + 2
END SUB

SUB CMD.CLS
  MID$(INCLUDE$, 1, 1) = "*"
  CALL PRINTOUT("JSR", "CLS" + COMMENT$(1))
  J = J + 1
END SUB

SUB CMD.DATA
  IF MID$(Q$, J + 3, 15) <> "N,N,N,N,N,N,N,N" THEN CALL ERROROUT("Invalid data")
  T$ = ""
  FOR T = 2 TO 16 STEP 2
    T$ = T$ + CHR$(DEC(BYTE$(TOKEN$(T))))
  NEXT T
  SELECT CASE TOKEN$(J + 1)
  CASE "CHAR"
    CHAR$ = CHAR$ + T$
  CASE "SPRITE"
    SPRITE$ = SPRITE$ + T$
  CASE ELSE
    CALL ERROROUT("Unknown statement")
  END SELECT
END SUB

SUB CMD.DEFINE
  IF MID$(Q$, J + 2, 3) <> "V,N" THEN CALL ERROROUT("Type mismatch")
  SELECT CASE TOKEN$(J + 1)
  CASE "A", "X", "Y"
    CALL ERROROUT("Invalid variable")
  CASE ELSE
    IF MID$(TOKEN$(J + 3), 1, 2) = "00" THEN
      T$ = MID$(TOKEN$(J + 3), 3, 2)
    ELSE
      T$ = TOKEN$(J + 3)
    END IF
    CALL PRINTOUT(TOKEN$(J + 1) + SPACE$(8 - LEN(TOKEN$(J + 1))) + ".EQU    $" + T$, "~")
  END SELECT
  J = J + 4
END SUB

SUB CMD.DIV16
  MID$(INCLUDE$, 18, 1) = "*"
  CALL PRINTOUT("JSR", "DIV16" + COMMENT$(1))
  J = J + 1
END SUB

SUB CMD.DIV8
  MID$(INCLUDE$, 7, 1) = "*"
  CALL PRINTOUT("JSR", "DIV8" + COMMENT$(1))
  J = J + 1
END SUB

SUB CMD.DLIST
  IF MID$(Q$, J + 2, 1) <> "N" THEN CALL ERROROUT("Type mismatch")
  'CALL PRINTOUT("SEI", COMMENT$(2))
  'CALL PRINTOUT("LDX", "SDMCTL" + COMMENT$(2))
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 1), 1, 2) + COMMENT$(2))
  CALL PRINTOUT("STA", "SDLSTH")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 1), 3, 2))
  CALL PRINTOUT("STA", "SDLSTL")
  'CALL PRINTOUT("STX", "SDMCTL")
  'CALL PRINTOUT("CLI", "")
  J = J + 2
END SUB

SUB CMD.DO (MINUS)
  DOCOUNT = DOCOUNT + 1
  DOSTACK(DODEPTH) = DOCOUNT: DODEPTH = DODEPTH + 1
  T$ = LTRIM$(STR$(DOCOUNT))
  SELECT CASE TOKEN$(J + 1)
  CASE "WHILE"
    CALL PRINTOUT("DO" + T$ + ":", "~")
    CALL BRANCH(MINUS, TOKEN$(J + 2), INVERTSIGN$(TOKEN$(J + 3)), TOKEN$(J + 4), "ED" + T$, MID$(Q$, J + 5, 1), COMMENT$(5))
    J = J + 4
  CASE "UNTIL"
    CALL PRINTOUT("DO" + T$ + ":", "~")
    CALL BRANCH(MINUS, TOKEN$(J + 2), TOKEN$(J + 3), TOKEN$(J + 4), "ED" + T$, MID$(Q$, J + 5, 1), COMMENT$(5))
    J = J + 4
  CASE ELSE
    CALL PRINTOUT("DO" + T$ + ":", "~" + COMMENT$(1))
  END SELECT
  J = J + 1
END SUB

SUB CMD.DOWN
  MID$(INCLUDE$, 8, 1) = "*"
  CALL PRINTOUT("JSR", "DOWN" + COMMENT$(1)): J = J + 1
END SUB

SUB CMD.ELSE
  IF IFDEPTH < 1 THEN CALL ERROROUT("ELSE without IF")
  T$ = LTRIM$(STR$(IFSTACK(IFDEPTH - 1)))
  IFCOUNT = IFCOUNT + 1: IFSTACK(IFDEPTH - 1) = IFCOUNT
  CALL PRINTOUT("JMP", "IF" + LTRIM$(STR$(IFCOUNT)) + COMMENT$(1))
  CALL PRINTOUT("IF" + T$ + ":", "~")
  J = J + 1
END SUB

SUB CMD.END
  SELECT CASE TOKEN$(J + 1)
  CASE "IF"
    IF IFDEPTH < 1 THEN CALL ERROROUT("END IF without IF")
    IFDEPTH = IFDEPTH - 1
    CALL PRINTOUT("IF" + LTRIM$(STR$(IFSTACK(IFDEPTH))) + ":", "~" + COMMENT$(2))
    J = J + 2
  CASE "SELECT"
    IF SELDEPTH < 1 THEN ERROROUT ("END SELECT without SELECT CASE")
    SELDEPTH = SELDEPTH - 1
    IF SELSTACK(SELDEPTH, 2) > SELSTACK(SELDEPTH, 0) THEN
      T2$ = LTRIM$(STR$(SELSTACK(SELDEPTH, 2)))
      CALL PRINTOUT("SC" + T2$ + ":", "~")
    END IF
    T2$ = LTRIM$(STR$(SELSTACK(SELDEPTH, 0)))
    CALL PRINTOUT("SC" + T2$ + ":", "~" + COMMENT$(2))
    J = J + 2
  CASE "SUB"
    CALL PRINTOUT("RTI", COMMENT$(2))
    J = J + 2
  CASE ELSE
    CALL ERROROUT("Invalid END type")
  END SELECT
END SUB

SUB CMD.EXIT
  SELECT CASE TOKEN$(J + 1)
  CASE "DO"
    IF DODEPTH < 1 THEN CALL ERROROUT("EXIT DO outside DO..LOOP")
    CALL PRINTOUT("JMP", "ED" + LTRIM$(STR$(DOSTACK(DODEPTH - 1))) + COMMENT$(2))
  CASE "FOR"
    IF FORDEPTH < 1 THEN CALL ERROROUT("EXIT FOR outside FOR..NEXT")
    CALL PRINTOUT("JMP", "EF" + LTRIM$(STR$(FORSTACK(FORDEPTH - 1))) + COMMENT$(2))
  CASE "SUB"
    CALL PRINTOUT("RTI", COMMENT$(2))
  CASE "SELECT"
    IF SELDEPTH < 1 THEN ERROROUT ("EXIT SELECT outside SELECT CASE")
    CALL PRINTOUT("JMP", "SC" + LTRIM$(STR$(SELSTACK(SELDEPTH - 1, 0))) + COMMENT$(2))
  CASE ELSE
    CALL ERROROUT("Syntax error")
  END SELECT
  J = J + 2
END SUB

SUB CMD.FOR (MINUS)
  IF MID$(Q$, J + 2, 1) <> "V" THEN CALL ERROROUT("Type mismatch")
  IF TOKEN$(J + 2) <> "TO" THEN CALL ERROROUT("Syntax Error")
  SELECT CASE TOKEN$(J + 1)
  CASE "A", "X", "Y"
    CALL ERROROUT("Illegal variable A/X/Y")
  END SELECT
  FORCOUNT = FORCOUNT + 1
  FORSTACK(FORDEPTH) = FORCOUNT: FORDEPTH = FORDEPTH + 1
  T$ = LTRIM$(STR$(FORCOUNT))
  CALL PRINTOUT("FR" + T$ + ":", "~" + COMMENT$(4))
  J = J + 3: CALL LDX("A")
  CALL PRINTOUT("CMP", TOKEN$(J - 2))
  IF MINUS = FALSE THEN
    CALL PRINTOUT("BCC", "EF" + T$)
  ELSE
    CALL PRINTOUT(".WORD", "$03B0;BCS past JMP")
    CALL PRINTOUT("JMP", "EF" + T$)
  END IF
  J = J + 1
END SUB

SUB CMD.GOSUB
  IF MID$(Q$, J + 2, 1) <> "V" THEN CALL ERROROUT("Missing label")
  CALL PRINTOUT("JSR", TOKEN$(J + 1) + COMMENT$(2)): J = J + 2
END SUB

SUB CMD.GOTO
  IF MID$(Q$, J + 2, 1) <> "V" THEN CALL ERROROUT("Missing label")
  CALL PRINTOUT("JMP", TOKEN$(J + 1) + COMMENT$(2)): J = J + 2
END SUB

SUB CMD.IF (MINUS)
'----------------------------------------------------------------------------
'IF A=1 THEN                                    IF A<>1 THEN GOTO J1
'  ...                                          ...
'END IF                                         J1:
'----------------------------------------------------------------------------
'IF A=1 AND B=1 THEN                            IF A<>1 THEN GOTO J1
'  ...                                          IF B<>1 THEN GOTO J1
'END IF                                         ...
'                                               J1:
'----------------------------------------------------------------------------
'IF A=1 OR B=1 THEN                             IF A=1 THEN GOTO J1
'  ...                                          IF B<>1 THEN GOTO J2
'END IF                                         J1:
'                                               ...
'                                               J2:
'----------------------------------------------------------------------------
'IF A=1 AND B=1 AND C=1 THEN                    IF A<>1 THEN GOTO J1
'  ...                                          IF B<>1 THEN GOTO J1
'END IF                                         IF C<>1 THEN GOTO J1
'                                               ...
'                                               J1:
'----------------------------------------------------------------------------
'IF A=1 AND B=1 OR C=1 THEN                     IF A<>1 THEN GOTO J2
'                                               J1:
'  ...                                          IF B=1 THEN GOTO J3
'END IF                                         J2:
'                                               IF C<>1 THEN GOTO J4
'                                               J3:
'                                               ...
'                                               J4:
'----------------------------------------------------------------------------
'IF A=1 OR B=1 AND C=1 THEN                     IF A=1 THEN GOTO J1
'  ...                                          IF B<>1 THEN GOTO J2
'END IF                                         J1:
'                                               IF C<>1 THEN GOTO J2
'                                               ...
'                                               J2:
'----------------------------------------------------------------------------
'IF A=1 OR B=1 OR C=1 THEN                      IF A=1 THEN GOTO J1
'  ...                                          IF B=1 THEN GOTO J1
'END IF                                         IF C<>1 THEN GOTO J2
'                                               J1:
'                                               ...
'                                               J2:
'----------------------------------------------------------------------------
'IF A=1 OR B=1 AND C=1 OR D=1 AND E=1 THEN      IF A=1 THEN GOTO J1
'                                               IF B<>1 THEN GOTO J2
'                                               J1:
'                                               IF C=1 THEN GOTO J3
'                                               J2:
'                                               IF D<>1 THEN GOTO J4
'                                               J3:
'                                               IF E<>1 THEN GOTO J4
'                                               ...
'                                               J4:
'----------------------------------------------------------------------------
'block: { test } { AND | OR | THEN }
'rules:
'if AND then invert sign, jump past next OR, to ELSE, or to END IF
'if OR then same sign, jump past next AND or to THEN
'if THEN then invert sign, jump to ELSE or to END IF
'----------------------------------------------------------------------------
'0  1 2 3 4    5 6 7 8
'IF A = 0 THEN :
'IF A = 0 AND  Y = 1 THEN
'----------------------------------------------------------------------------
  T2 = 4 + J
  DO
    IF MID$(Q$, T2 + 1, 1) = "E" THEN CALL ERROROUT("IF without THEN")
    IF TOKEN$(T2) = "THEN" THEN EXIT DO
    T2 = T2 + 1
  LOOP
  IF INSTR("E:", MID$(Q$, T2 + 2, 1)) > 0 THEN 'multiline IF...END IF
    IFCOUNT = IFCOUNT + 1: IFSTACK(IFDEPTH) = IFCOUNT: IFDEPTH = IFDEPTH + 1
    T = J + 1: P = -1
    DO
      IF MID$(Q$, T + 1, 2) <> "VO" THEN CALL ERROROUT("Syntax error")
      P = P + 1: IF P > 0 THEN CALL PRINTOUT("IF" + LTRIM$(STR$(IFCOUNT)) + "P" + LTRIM$(STR$(P)) + ":", "~")
      SELECT CASE TOKEN$(T + 3)
      CASE "AND"
        T2 = T + 7: P2 = P + 1
        DO
          SELECT CASE TOKEN$(T2)
          CASE "AND"
          CASE "OR"
            P2 = P2 + 1: EXIT DO
          CASE "THEN"
            P2 = 0: EXIT DO
          CASE ELSE
            CALL ERROROUT("Syntax error")
          END SELECT
          T2 = T2 + 4: P2 = P2 + 1
        LOOP
        IF P2 = 0 THEN
          CALL BRANCH(MINUS, TOKEN$(T), INVERTSIGN$(TOKEN$(T + 1)), TOKEN$(T + 2), "IF" + LTRIM$(STR$(IFCOUNT)), MID$(Q$, T + 3, 1), COMMENT$(4))
        ELSE
          CALL BRANCH(FALSE, TOKEN$(T), INVERTSIGN$(TOKEN$(T + 1)), TOKEN$(T + 2), "IF" + LTRIM$(STR$(IFCOUNT)) + "P" + LTRIM$(STR$(P2)), MID$(Q$, T + 3, 1), COMMENT$(4))
        END IF
      CASE "OR"
        T2 = T + 7: P2 = P + 1
        DO
          SELECT CASE TOKEN$(T2)
          CASE "OR"
          CASE "AND", "THEN"
            P2 = P2 + 1: EXIT DO
          CASE ELSE
            CALL ERROROUT("Syntax error")
          END SELECT
          T2 = T2 + 4: P2 = P2 + 1
        LOOP
        CALL BRANCH(FALSE, TOKEN$(T), TOKEN$(T + 1), TOKEN$(T + 2), "IF" + LTRIM$(STR$(IFCOUNT)) + "P" + LTRIM$(STR$(P2)), MID$(Q$, T + 3, 1), COMMENT$(4))
      CASE "THEN"
        CALL BRANCH(MINUS, TOKEN$(T), INVERTSIGN$(TOKEN$(T + 1)), TOKEN$(T + 2), "IF" + LTRIM$(STR$(IFCOUNT)), MID$(Q$, T + 3, 1), COMMENT$(5))
        P = P + 1: CALL PRINTOUT("IF" + LTRIM$(STR$(IFCOUNT)) + "P" + LTRIM$(STR$(P)) + ":", "~")
        J = J + 4
        EXIT DO
      CASE ELSE
        CALL ERROROUT("Syntax error")
      END SELECT
      T = T + 4: J = J + 4
    LOOP
    J = J + 1
  ELSE 'single line IF...GOTO or IF..EXIT
    IF MID$(Q$, J + 2, 2) <> "VO" THEN CALL ERROROUT("Syntax error")
    SELECT CASE TOKEN$(J + 5)
    CASE "GOTO"
      IF MID$(Q$, J + 7, 1) <> "V" THEN CALL ERROROUT("Missing label")
    CASE "EXIT"
      SELECT CASE TOKEN$(J + 6)
      CASE "DO"
        IF DODEPTH < 1 THEN CALL ERROROUT("EXIT DO outside DO..LOOP")
        TOKEN$(J + 6) = "ED" + LTRIM$(STR$(DOSTACK(DODEPTH - 1)))
      CASE "FOR"
        IF FORDEPTH < 1 THEN CALL ERROROUT("EXIT FOR outside FOR..NEXT")
        TOKEN$(J + 6) = "EF" + LTRIM$(STR$(FORSTACK(FORDEPTH - 1)))
      CASE "SELECT"
        IF SELDEPTH < 1 THEN ERROROUT ("EXIT SELECT outside SELECT CASE")
        TOKEN$(J + 6) = "SC" + LTRIM$(STR$(SELSTACK(SELDEPTH - 1, 0)))
      CASE ELSE
        CALL ERROROUT("Syntax error")
      END SELECT
    CASE ELSE
      CALL ERROROUT("Syntax error")
    END SELECT
    CALL BRANCH(MINUS, TOKEN$(J + 1), TOKEN$(J + 2), TOKEN$(J + 3), TOKEN$(J + 6), MID$(Q$, J + 4, 1), COMMENT$(7))
    J = J + 7
  END IF
END SUB

SUB CMD.INPUT
  IF MID$(Q$, J + 2, 1) <> "V" THEN CALL ERROROUT("Type mismatch")
  MID$(INCLUDE$, 5, 1) = "*"
  CALL PRINTOUT("JSR", "INPUT" + COMMENT$(2))
  J = J + 1: CALL STX("A"): J = J + 1
END SUB

SUB CMD.INTERNAL
  IF MID$(Q$, J + 2, 1) <> "N" THEN CALL ERROROUT("Type mismatch")
  T = DEC(TOKEN$(J + 1))
  IF T < 1 OR T > INCLUDE THEN CALL ERROROUT("Out of range")
  MID$(INCLUDE$, T, 1) = "*"
  J = J + 2
END SUB

SUB CMD.KEYPAD
  IF TOKEN$(J + 1) = "FIX" THEN
    CALL PRINTOUT("LDA", "KEYDB;check key debounce counter")
    CALL PRINTOUT(".WORD", "$0230;if minus, skip")
    CALL PRINTOUT("INC", "KEYDB;otherwise increase by one")
  ELSE
    IF MID$(Q$, J + 2, 1) <> "N" THEN CALL ERROROUT("Type mismatch")
    T = DEC(TOKEN$(J + 1))
    IF T < 0 OR T > 3 THEN CALL ERROROUT("Invalid keypad")
    CALL PRINTOUT("LDA", "#$" + HEX2$(4 + T) + COMMENT$(2))
    CALL PRINTOUT("STA", "CONSOL")
  END IF
  J = J + 2
END SUB

SUB CMD.LEFT
  MID$(INCLUDE$, 4, 1) = "*"
  CALL PRINTOUT("JSR", "LEFT" + COMMENT$(1)): J = J + 1
END SUB

SUB CMD.LOCATE
  SELECT CASE MID$(Q$, J + 2, 3)
  CASE "N,N"
    T$ = HEX4$(40 * DEC(TOKEN$(J + 1)) + DEC(TOKEN$(J + 3)) + DEC(OPTIONS$(14, 0)))
    CALL PRINTOUT("LDA", "#$" + MID$(T$, 1, 2) + COMMENT$(4))
    CALL PRINTOUT("STA", "LOCATEH")
    CALL PRINTOUT("LDA", "#$" + MID$(T$, 3, 2))
    J = J + 4
  CASE "V,N", "N,V", "V,V"
    J = J + 1: CALL LDX("Y"): J = J - 1
    J = J + 3: CALL LDX("A"): J = J - 3
    CALL PRINTOUT("STA", "TEMPL" + COMMENT$(4))
    J = J + 4
    CALL PRINTOUT("LDA", "#$28")
    CALL PRINTOUT("LDX", "#$" + MID$(OPTIONS$(14, 0), 1, 2))
    MID$(INCLUDE$, 6, 1) = "*"
    CALL PRINTOUT("JSR", "MULADD") 'MULADD: (X:A)=A*Y+(X:TEMPL) if Y>0
    CALL PRINTOUT("STX", "LOCATEH")
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
  CALL PRINTOUT("STA", "LOCATEL")
END SUB

SUB CMD.LOOP (MINUS)
  IF DODEPTH < 1 THEN CALL ERROROUT("LOOP without DO")
  DODEPTH = DODEPTH - 1
  T$ = LTRIM$(STR$(DOSTACK(DODEPTH)))
  SELECT CASE TOKEN$(J + 1)
  CASE "WHILE"
    CALL BRANCH(MINUS, TOKEN$(J + 2), TOKEN$(J + 3), TOKEN$(J + 4), "DO" + T$, MID$(Q$, J + 5, 1), COMMENT$(5))
    J = J + 4
  CASE "UNTIL"
    CALL BRANCH(MINUS, TOKEN$(J + 2), INVERTSIGN$(TOKEN$(J + 3)), TOKEN$(J + 4), "DO" + T$, MID$(Q$, J + 5, 1), COMMENT$(5))
    J = J + 4
  CASE ELSE
    CALL PRINTOUT("JMP", "DO" + T$ + COMMENT$(1))
  END SELECT
  CALL PRINTOUT("ED" + T$ + ":", "~")
  J = J + 1
END SUB

SUB CMD.MEMAREA
  IF MID$(Q$, J + 2, 9) <> "N,N,N,N,N" THEN CALL ERROROUT("Illegal function call")
  MID$(INCLUDE$, 16, 1) = "*"
  TOKEN$(J + 5) = HEX4$(DEC(TOKEN$(J + 5)) - 1)
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 1), 1, 2) + COMMENT$(10))
  CALL PRINTOUT("STA", "FROMH")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 1), 3, 2))
  CALL PRINTOUT("STA", "FROML")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 3), 1, 2))
  CALL PRINTOUT("STA", "TOH")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 3), 3, 2))
  CALL PRINTOUT("STA", "TOL")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 5), 1, 2))
  CALL PRINTOUT("STA", "COUNTH")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 5), 3, 2))
  CALL PRINTOUT("STA", "COUNTL")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 7), 3, 2))
  CALL PRINTOUT("STA", "COPYLEN")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 9), 3, 2))
  CALL PRINTOUT("STA", "SKIP")
  CALL PRINTOUT("JSR", "MEMAREA")
  J = J + 10
END SUB

SUB CMD.MEMCOPY
  IF MID$(Q$, J + 2, 5) <> "N,N,N" THEN CALL ERROROUT("Illegal function call")
  MID$(INCLUDE$, 15, 1) = "*"
  TOKEN$(J + 5) = HEX4$(DEC(TOKEN$(J + 5)) - 1)
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 1), 1, 2) + COMMENT$(6))
  CALL PRINTOUT("STA", "FROMH")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 1), 3, 2))
  CALL PRINTOUT("STA", "FROML")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 3), 1, 2))
  CALL PRINTOUT("STA", "TOH")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 3), 3, 2))
  CALL PRINTOUT("STA", "TOL")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 5), 1, 2))
  CALL PRINTOUT("STA", "COUNTH")
  CALL PRINTOUT("LDA", "#$" + MID$(TOKEN$(J + 5), 3, 2))
  CALL PRINTOUT("STA", "COUNTL")
  CALL PRINTOUT("JSR", "MEMCOPY")
  J = J + 6
END SUB

SUB CMD.MISSILES
  SELECT CASE TOKEN$(J + 1)
  CASE "ON"
    CALL PRINTOUT("LDA", "#$03" + COMMENT$(2))
  CASE "OFF"
    CALL PRINTOUT("LDA", "#$02" + COMMENT$(2))
  CASE ELSE
    CALL ERROROUT("Syntax error")
  END SELECT
  CALL PRINTOUT("STA", "GRACTL")
  J = J + 2

END SUB

SUB CMD.MOVEUP
  MID$(INCLUDE$, 11, 1) = "*"
  CALL PRINTOUT("JSR", "MOVEUP")
  J = J + 1
END SUB

SUB CMD.MUL8
  MID$(INCLUDE$, 10, 1) = "*"
  CALL PRINTOUT("JSR", "MUL8" + COMMENT$(1))
  J = J + 1
END SUB

SUB CMD.MULADD
  MID$(INCLUDE$, 6, 1) = "*"
  CALL PRINTOUT("JSR", "MULADD" + COMMENT$(1))
  J = J + 1
END SUB

SUB CMD.NEXT
  IF FORDEPTH < 1 THEN CALL ERROROUT("NEXT without FOR")
  IF MID$(Q$, J + 2, 1) <> "V" THEN CALL ERROROUT("Missing variable")
  SELECT CASE TOKEN$(J + 1)
  CASE "A", "X", "Y"
    CALL ERROROUT("Illegal variable A/X/Y")
  END SELECT
  CALL PRINTOUT("INC", TOKEN$(J + 1) + COMMENT$(2))
  FORDEPTH = FORDEPTH - 1
  CALL PRINTOUT("JMP", "FR" + LTRIM$(STR$(FORSTACK(FORDEPTH))))
  CALL PRINTOUT("EF" + LTRIM$(STR$(FORSTACK(FORDEPTH))) + ":", "~")
  J = J + 2
END SUB

SUB CMD.PALETTE
  IF MID$(Q$, J + 2, 2) <> "N," THEN CALL ERROROUT("Illegal function call")
  T = DEC(TOKEN$(J + 1))
  IF T < 0 OR T > 8 THEN CALL ERROROUT("Illegal function call")
  T$ = MID$("COLOR4COLOR0COLOR1COLOR2COLOR3PCOLR0PCOLR1PCOLR2PCOLR3", T * 6 + 1, 6)
  J = J + 3: CALL LDX("A"): J = J - 3
  CALL PRINTOUT("STA", T$ + COMMENT$(4))
  J = J + 4
END SUB

SUB CMD.POKE
'0    1  2345
'POKE NV,AXY
'POKE NV+X,A
'POKE NV+Y,A
'POKE NV*Y,A
  SELECT CASE MID$(Q$, J + 2, 1)
  CASE "N"
    IF TOKEN$(J + 2) = "*" THEN
      T$ = "$" + BYTE$(TOKEN$(J + 1))
    ELSE
      T$ = "$" + TOKEN$(J + 1)
    END IF
  CASE "V"
    SELECT CASE TOKEN$(J + 1)
    CASE "A", "X", "Y"
      CALL ERROROUT("Illegal variable A/X/Y")
    CASE ELSE
      T$ = TOKEN$(J + 1)
    END SELECT
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
  SELECT CASE TOKEN$(J + 2) + TOKEN$(J + 3)
  CASE "+X"
    IF TOKEN$(J + 4) + TOKEN$(J + 5) <> ",A" THEN CALL ERROROUT("Type mismatch")
    CALL PRINTOUT("STA", T$ + ",X" + COMMENT$(6))
    J = J + 6
  CASE "+Y"
    IF TOKEN$(J + 4) + TOKEN$(J + 5) <> ",A" THEN CALL ERROROUT("Type mismatch")
    CALL PRINTOUT("STA", T$ + ",Y" + COMMENT$(6))
    J = J + 6
  CASE "*Y"
    IF TOKEN$(J + 4) + TOKEN$(J + 5) <> ",A" THEN CALL ERROROUT("Type mismatch")
    CALL PRINTOUT("STA", "(" + T$ + "),Y" + COMMENT$(6))
    J = J + 6
  CASE ",A", ",X", ",Y"
    CALL PRINTOUT("ST" + TOKEN$(J + 3), T$ + COMMENT$(4))
    J = J + 4
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
END SUB

SUB CMD.POP
  SELECT CASE TOKEN$(J + 1)
  CASE "ALL"
    CALL PRINTOUT("PLA", COMMENT$(2))
    CALL PRINTOUT("TAY", "")
    CALL PRINTOUT("PLA", "")
    CALL PRINTOUT("TAX", "")
    CALL PRINTOUT("PLA", "")
  CASE "A"
    CALL PRINTOUT("PLA", COMMENT$(2))
  CASE ELSE
    CALL ERROROUT("Syntax error")
  END SELECT
  J = J + 2
END SUB

SUB CMD.POS
  MID$(INCLUDE$, 14, 1) = "*"
  CALL PRINTOUT("JSR", "POS" + COMMENT$(1))
  J = J + 1
END SUB

SUB CMD.PRINT
  SELECT CASE MID$(Q$, J + 2, 1)
  CASE "S" 'PRINT ""
    MID$(INCLUDE$, 2, 1) = "*"
    CALL FIXSTR(TOKEN$(J + 1))
    TOKEN$(J + 1) = TOKEN$(J + 1) + CHR$(255)
    T = INSTR(ST$, TOKEN$(J + 1)) - 1
    IF T = -1 THEN
      T = LEN(ST$): ST$ = ST$ + TOKEN$(J + 1)
    END IF
    T$ = HEX4$(T + DEC(OPTIONS$(17, 0)))
    CALL PRINTOUT("LDA", "#$" + MID$(T$, 3, 2) + COMMENT$(2))
    CALL PRINTOUT("STA", "TEMPL")
    CALL PRINTOUT("LDA", "#$" + MID$(T$, 1, 2))
    CALL PRINTOUT("STA", "TEMPH")
    CALL PRINTOUT("JSR", "PRINT")
    J = J + 2
  CASE "F" 'PRINT CHR$()
    J = J + 2: CALL LDX("A")
    IF TOKEN$(J + 1) <> ")" THEN CALL ERROROUT("Mismatched parenthesis")
    J = J - 2
    CALL PRINTOUT("LDY", "#$00" + COMMENT$(4))
    CALL PRINTOUT("STA", "(LOCATEL),Y")
    J = J + 4
  CASE ELSE 'PRINT
    MID$(INCLUDE$, 13, 1) = "*"
    CALL PRINTOUT("JSR", "CRLF" + COMMENT$(1))
    J = J + 1
  END SELECT
END SUB

SUB CMD.PUSH
  SELECT CASE TOKEN$(J + 1)
  CASE "ALL"
    CALL PRINTOUT("PHA", COMMENT$(2))
    CALL PRINTOUT("TXA", "")
    CALL PRINTOUT("PHA", "")
    CALL PRINTOUT("TYA", "")
    CALL PRINTOUT("PHA", "")
  CASE "A"
    CALL PRINTOUT("PHA", COMMENT$(2))
  CASE ELSE
    CALL ERROROUT("Syntax error")
  END SELECT
  J = J + 2
END SUB

SUB CMD.PUT
  'PUT ( 2 , 4 ) , 7 , 9 , 11
  IF TOKEN$(J + 1) + TOKEN$(J + 3) + TOKEN$(J + 5) + TOKEN$(J + 6) + TOKEN$(J + 8) + MID$(Q$, J + 10, 1) <> "(,),,N" THEN CALL ERROROUT("Syntax error")
  FROMH = 0
  IF MID$(Q$, J + 11, 2) = ",V" THEN
    IF TOKEN$(J + 11) = "FROMH" THEN
      MID$(Q$, J + 12, 1) = "N": FROMH = 1
    ELSE
      CALL ERROROUT("Variable must be FROMH")
    END IF
  END IF
  IF MID$(Q$, J + 11, 3) = ",OV" THEN
    IF TOKEN$(J + 12) = "FROMH" THEN
      MID$(Q$, J + 13, 1) = "N": FROMH = 1
    ELSE
      CALL ERROROUT("Variable must be FROMH")
    END IF
  END IF
  IF MID$(Q$, J + 11, 2) = ",N" THEN 'height specified?
    T3 = 2
  ELSEIF MID$(Q$, J + 11, 3) = ",ON" THEN 'negative height (invert)
    T3 = 3
  ELSE
    T3 = 0
  END IF
  T = DEC(TOKEN$(J + 9)): IF T > 7 THEN CALL ERROROUT("Out of range")
  T$ = CHR$(T MOD 4 + 48)
  IF T > 3 THEN T2 = 3: T3$ = "M" ELSE T2 = T + 4: T3$ = "P"
  T2$ = HEX2$(DEC(MID$(OPTIONS$(15, 0), 1, 2)) + T2)
  SELECT CASE MID$(Q$, J + 3, 1)
  CASE "N"
    T1 = DEC(MID$(TOKEN$(J + 2), 3, 2)) + &H30
    CALL PRINTOUT("LDA", "#$" + HEX2$(T1) + COMMENT$(10 + T3))
  CASE "V"
    CALL PRINTOUT("LDA", TOKEN$(J + 2) + COMMENT$(10 + T3))
    CALL PRINTOUT("ADC", "#$2F")
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
  CALL PRINTOUT("STA", "HPOS" + T3$ + T$)
  J = J + 4: CALL LDX("X")
  IF T > 3 THEN
    CALL PRINTOUT("STX", "FROML")
    CALL PRINTOUT("LDX", "#$0" + CHR$(T + 44))
  END IF
  CALL PRINTOUT("LDA", "#$" + T2$)
  CALL PRINTOUT("STA", "SPRITEH")
  J = J + 3: CALL LDX("A")
  CALL PRINTOUT("STA", "TEMPH")
  IF T > 3 THEN
    IF T3 = 0 THEN
      CALL PRINTOUT("JSR", "PUTMSL")
      MID$(INCLUDE$, 19, 1) = "*"
    ELSE
      IF T3 = 2 THEN
        IF FROMH = 0 THEN
          CALL PRINTOUT("LDA", "#$" + HEX2$(DEC(TOKEN$(J + 4))))
          CALL PRINTOUT("STA", "FROMH")
        END IF
        CALL PRINTOUT("JSR", "PUTMSH")
        MID$(INCLUDE$, 20, 1) = "*"
      ELSE
        IF FROMH = 0 THEN
          CALL PRINTOUT("LDA", "#$" + HEX2$(DEC(TOKEN$(J + 5)) - 1))
          CALL PRINTOUT("STA", "FROMH")
        END IF
        CALL PRINTOUT("JSR", "PUTMSI")
        MID$(INCLUDE$, 22, 1) = "*"
      END IF
    END IF
  ELSE
    IF T3 = 0 THEN
      CALL PRINTOUT("JSR", "PUTSPR")
      MID$(INCLUDE$, 17, 1) = "*"
    ELSE
      IF T3 = 2 THEN
        IF FROMH = 0 THEN
          CALL PRINTOUT("LDA", "#$" + HEX2$(DEC(TOKEN$(J + 4))))
          CALL PRINTOUT("STA", "FROMH")
        END IF
        CALL PRINTOUT("JSR", "PUTSPH")
        MID$(INCLUDE$, 21, 1) = "*"
      ELSE
        IF FROMH = 0 THEN
          CALL PRINTOUT("LDA", "#$" + HEX2$(DEC(TOKEN$(J + 5)) - 1))
          CALL PRINTOUT("STA", "FROMH")
        END IF
        CALL PRINTOUT("JSR", "PUTSPI")
        MID$(INCLUDE$, 23, 1) = "*"
      END IF
    END IF
  END IF
  J = J + 3 + T3
END SUB

SUB CMD.RETURN
  CALL PRINTOUT("RTS", COMMENT$(1)): J = J + 1
END SUB

SUB CMD.RIGHT
  MID$(INCLUDE$, 3, 1) = "*"
  CALL PRINTOUT("JSR", "RIGHT" + COMMENT$(1)): J = J + 1
END SUB

SUB CMD.RIGHTBRACE
  MID$(INCLUDE$, 3, 1) = "*"
  CALL PRINTOUT("JSR", "RIGHT" + COMMENT$(1))
  MID$(INCLUDE$, 12, 1) = "*"
  CALL PRINTOUT("JSR", "CHKROW")
  J = J + 1
END SUB

SUB CMD.SCREEN
  IF MID$(Q$, J + 2, 1) <> "N" THEN CALL ERROROUT("Illegal function call")
  SCRMODE = DEC(TOKEN$(J + 1)) 'AND 15
  SELECT CASE SCRMODE
  CASE 5, 7
    SCRHGT = 12
  CASE 2, 3, 4, 6, 8
    SCRHGT = 24
  CASE 9, 10
    SCRHGT = 48
  CASE 11, 13
    SCRHGT = 96
  CASE 12, 14, 15
    SCRHGT = 192
  'CASE 16 'ANTIC E, vector graphics mode
  '  SCRMODE = 14: SCRHGT = 192: VECTGR = TRUE
  CASE ELSE
    CALL ERROROUT("Invalid ANTIC mode")
  END SELECT
  T = &HBFD4 - SCRHGT - 8
  IF SCRMODE >= 14 THEN T = T - 2
  OPTIONS$(18, 0) = HEX4$(T)
  J = J + 2
END SUB

SUB CMD.SELECT
  IF TOKEN$(J + 1) <> "CASE" THEN CALL ERROROUT("Syntax error")
  T = INSTR("AXY", TOKEN$(J + 2))
  IF T = 0 THEN CALL ERROROUT("A/X/Y expected")
  SELCOUNT = SELCOUNT + 1
  SELSTACK(SELDEPTH, 0) = SELCOUNT
  SELSTACK(SELDEPTH, 2) = SELCOUNT
  SELSTACK(SELDEPTH, 1) = T
  SELDEPTH = SELDEPTH + 1
  CALL PRINTOUT("", COMMENT$(3))
  J = J + 3
END SUB

SUB CMD.SET
  'SET option=location [label]
       O$ = "VIMIRQ  VVBLKI  VVBLKD  VDSLST  VKYBDI  VKYBDF  VTRIGR  "
  O$ = O$ + "VBRKOP  VSERIN  VSEROR  VSEROC  VTIMR1  VTIMR2  VTIMR4  "
  O$ = O$ + "SCREEN  SPRITES CHARSET STRINGS DLIST   VECTTBL "
  B = INSTR(1, O$, TOKEN$(J + 1) + SPACE$(8 - LEN(TOKEN$(J + 1))))
  IF B = 0 THEN CALL ERROROUT("Syntax error")
  T = (B - 1) / 8
  IF MID$(Q$, J + 3, 2) <> "ON" THEN CALL ERROROUT("Syntax error")
  OPTIONS$(T, 0) = TOKEN$(J + 3)
  IF MID$(Q$, J + 5, 1) = "V" THEN OPTIONS$(T, 1) = TOKEN$(J + 4): J = J + 1
  J = J + 4
END SUB

SUB CMD.SOUND
  '      1         2 3
  'SOUND CHANNEL(N), FREQUENCY(N/V)
  IF MID$(Q$, J + 2, 1) + TOKEN$(J + 2) <> "N," THEN CALL ERROROUT("Syntax error")
  T = DEC(TOKEN$(J + 1)): IF T > 3 THEN CALL ERROROUT("Out of range")
  T$ = CHR$(T + 49) 'channel
  J = J + 3: CALL LDX("A"): J = J - 3
  CALL PRINTOUT("STA", "AUDF" + T$ + COMMENT$(8))
  J = J + 4
END SUB

SUB CMD.SPRITES
  SELECT CASE TOKEN$(J + 1)
  CASE "ON"
    CALL PRINTOUT("LDA", "#$3E" + COMMENT$(2))
  CASE "OFF"
    CALL PRINTOUT("LDA", "#$22" + COMMENT$(2))
  CASE ELSE
    CALL ERROROUT("Syntax error")
  END SELECT
  CALL PRINTOUT("STA", "SDMCTL")
  J = J + 2
END SUB

SUB CMD.SUB
  IF MID$(Q$, J + 2, 1) <> "V" THEN CALL ERROROUT("Syntax error")
  FOR T = 0 TO OPTIONS
    IF TOKEN$(J + 1) = OPTIONS$(T, 1) THEN EXIT FOR
  NEXT T
  IF T > OPTIONS THEN CALL ERROROUT("SUB undefined")
  CALL PRINTOUT(".ORG    $" + OPTIONS$(T, 0), "~")
  CALL PRINTOUT(";-----------------------------------------------------------", "~")
  CALL PRINTOUT("; SUB " + TOKEN$(J + 1), "~")
  CALL PRINTOUT(";-----------------------------------------------------------", "~")
  CALL PRINTOUT(TOKEN$(J + 1) + ":", "~")
  J = J + 2
END SUB

SUB CMD.TITLE
  SELECT CASE MID$(Q$, J + 2, 1)
  CASE "S"
    TITLE$ = TOKEN$(J + 1)
    IF LEN(TITLE$) > 20 THEN CALL ERROROUT("TITLE too long (20 chars max)")
  CASE "V"
    IF TOKEN$(J + 1) = "OFF" THEN TITLE = FALSE
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
  J = J + 2
END SUB

SUB CMD.UP
  MID$(INCLUDE$, 9, 1) = "*"
  CALL PRINTOUT("JSR", "UP" + COMMENT$(1)): J = J + 1
END SUB

SUB CMD.VOLUME
  '      1         2 3         4 5
  'SOUND CHANNEL(N), NOISE(N/V), VOLUME(N/V)
  IF MID$(Q$, J + 2, 1) + TOKEN$(J + 2) + TOKEN$(J + 4) <> "N,," THEN CALL ERROROUT("Syntax error")
  T = DEC(TOKEN$(J + 1)): IF T > 3 THEN CALL ERROROUT("Out of range")
  T$ = CHR$(T + 49) 'channel
  SELECT CASE MID$(Q$, J + 4, 1) + MID$(Q$, J + 6, 1)
  CASE "NN"
    T = 16 * DEC(TOKEN$(J + 3)) + DEC(TOKEN$(J + 5))
    CALL PRINTOUT("LDA", "#$" + HEX2$(T) + COMMENT$(6))
  CASE "VN", "VV"
    IF MID$(Q$, J + 6, 1) = "N" THEN
      T2$ = "#$" + HEX2$(DEC(TOKEN$(J + 5)))
    ELSE
      T2$ = TOKEN$(J + 5)
    END IF
    CALL PRINTOUT("LDA", TOKEN$(J + 3) + COMMENT$(6))
    IF DASM = FALSE THEN
      CALL PRINTOUT("ASL", "A")
      CALL PRINTOUT("ASL", "A")
      CALL PRINTOUT("ASL", "A")
      CALL PRINTOUT("ASL", "A")
    ELSE
      CALL PRINTOUT("ASL", "")
      CALL PRINTOUT("ASL", "")
      CALL PRINTOUT("ASL", "")
      CALL PRINTOUT("ASL", "")
    END IF
    CALL PRINTOUT("CLC", "")
    CALL PRINTOUT("ADC", T2$)
  CASE "NV"
    T = 16 * DEC(TOKEN$(J + 3))
    CALL PRINTOUT("LDA", "#$" + HEX2$(T) + COMMENT$(6))
    IF MID$(Q$, J + 6, 1) = "N" THEN
      T2$ = "#$" + HEX2$(DEC(TOKEN$(J + 5)))
    ELSE
      T2$ = TOKEN$(J + 5)
    END IF
    CALL PRINTOUT("CLC", "")
    CALL PRINTOUT("ADC", T2$)
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
  CALL PRINTOUT("STA", "AUDC" + T$)
  J = J + 6
END SUB

FUNCTION COMMENT$ (N)
SHARED J
  A$ = ";"
  FOR A = J TO J + N - 1
    B$ = TOKEN$(A)
    FOR B = 1 TO LEN(B$)
      IF ASC(MID$(B$, B, 1)) < 32 THEN MID$(B$, B, 1) = "þ"
    NEXT B
    A$ = A$ + B$ + " "
  NEXT A
  COMMENT$ = A$
END FUNCTION

SUB CONSCOLOR (COLR)
SHARED DCOLOR
  IF DCOLOR = TRUE THEN
    IF COLR > 7 THEN COLR = COLR - 8: T$ = "1" ELSE T$ = "0"
    PRINT #3, "[" + T$ + ";3" + MID$("04261537", COLR + 1, 1) + "m";
  END IF
END SUB

FUNCTION DEC (H$)
  V = 0
  FOR A = 1 TO LEN(H$)
    V = V * 16 + VAL("&H" + MID$(H$, A, 1))
  NEXT A
  DEC = V
END FUNCTION

SUB ERROROUT (E$)
SHARED A$
  PRINT #3, ""
  PRINT #3, E$; " on line"; STR$(LINENUM); ":"
  PRINT #3, "["; LTRIM$(STR$(LINENUM)); "] "; A$
  END
END SUB

SUB FCN.INKEY
  IF TOKEN$(J) <> "A" THEN CALL ERROROUT("A expected")
  CALL PRINTOUT("LDA", "KEY" + COMMENT$(3))
  CALL PRINTOUT("LDX", "#$FF")
  CALL PRINTOUT("STX", "KEY")
  J = J + 3
END SUB

SUB FCN.JOYTRIG
  IF INSTR("AXY", TOKEN$(J)) = 0 THEN CALL ERROROUT("A/X/Y expected")
  T2$ = "LD" + TOKEN$(J)

  'CALL PRINTOUT(T2$, "JOYTRIG" + COMMENT$(3))
  'CALL PRINTOUT(".WORD", "$02F0")
  'CALL PRINTOUT("DEC", "JOYTRIG")
  'J = J + 3

  'IF TOKEN$(J) <> "A" THEN CALL ERROROUT("A expected")
  'CALL PRINTOUT("LDA", "SKSTAT" + COMMENT$(3))
  'CALL PRINTOUT("AND", "#$F7")
  'J = J + 3

  IF MID$(Q$, J + 4, 2) <> "N)" THEN CALL ERROROUT("Type mismatch")
  T = DEC(TOKEN$(J + 3))
  IF T < 0 OR T > 3 THEN CALL ERROROUT("Illegal function call")
  CALL PRINTOUT(T2$, "TRIG" + LTRIM$(STR$(T)) + COMMENT$(5))  'T*2???
  J = J + 5
END SUB

SUB FCN.JOYTRIG2
  IF TOKEN$(J) <> "A" THEN CALL ERROROUT("A expected")
  CALL PRINTOUT("LDA", "SKSTAT" + COMMENT$(3))
  CALL PRINTOUT("AND", "#$08")
  J = J + 3
END SUB

SUB FCN.JOYX
  IF INSTR("AXY", TOKEN$(J)) = 0 THEN CALL ERROROUT("A/X/Y expected")
  T2$ = "LD" + TOKEN$(J)
  IF MID$(Q$, J + 4, 2) <> "N)" THEN CALL ERROROUT("Type mismatch")
  T = DEC(TOKEN$(J + 3))
  IF T < 0 OR T > 3 THEN CALL ERROROUT("Illegal function call")
  CALL PRINTOUT(T2$, "PADDL" + LTRIM$(STR$(T * 2)) + COMMENT$(5))
  J = J + 5
END SUB

SUB FCN.JOYY
  IF INSTR("AXY", TOKEN$(J)) = 0 THEN CALL ERROROUT("A/X/Y expected")
  T2$ = "LD" + TOKEN$(J)
  IF MID$(Q$, J + 4, 2) <> "N)" THEN CALL ERROROUT("Type mismatch")
  T = DEC(TOKEN$(J + 3))
  IF T < 0 OR T > 3 THEN CALL ERROROUT("Illegal function call")
  CALL PRINTOUT(T2$, "PADDL" + LTRIM$(STR$(T * 2 + 1)) + COMMENT$(5))
  J = J + 5
END SUB

SUB FCN.PEEK
'AXY=PEEK(NV)
'012    3  456
'AY=PEEK(NV+X)
'AX=PEEK(NV+Y)
'A=PEEK(NV*Y) or A=PEEK(NV,Y)
  SELECT CASE MID$(Q$, J + 4, 1)
  CASE "N"
    IF TOKEN$(J + 4) = "*" OR TOKEN$(J + 4) = "," THEN
      T$ = "$" + BYTE$(TOKEN$(J + 3))
    ELSE
      T$ = "$" + TOKEN$(J + 3)
    END IF
  CASE "V"
    SELECT CASE TOKEN$(J + 1)
    CASE "A", "X", "Y"
      CALL ERROROUT("Illegal variable A/X/Y")
    CASE ELSE
      T$ = TOKEN$(J + 3)
    END SELECT
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
  SELECT CASE TOKEN$(J + 4) + TOKEN$(J + 5) + TOKEN$(J + 6)
  CASE "+X)"
    IF TOKEN$(J) <> "A" AND TOKEN$(J) <> "Y" THEN CALL ERROROUT("A/Y expected")
    CALL PRINTOUT("LD" + TOKEN$(J), T$ + ",X" + COMMENT$(7))
    J = J + 7
  CASE "+Y)"
    IF TOKEN$(J) <> "A" AND TOKEN$(J) <> "X" THEN CALL ERROROUT("A/X expected")
    CALL PRINTOUT("LD" + TOKEN$(J), T$ + ",Y" + COMMENT$(7))
    J = J + 7
  CASE "*Y)", ",Y)"
    IF TOKEN$(J) <> "A" THEN CALL ERROROUT("A expected")
    CALL PRINTOUT("LDA", "(" + T$ + "),Y" + COMMENT$(7))
    J = J + 7
  CASE ELSE
    IF TOKEN$(J + 4) = ")" THEN
      IF INSTR("AXY", TOKEN$(J)) = 0 THEN CALL ERROROUT("A/X/Y expected")
      CALL PRINTOUT("LD" + TOKEN$(J), T$ + COMMENT$(5))
      J = J + 5
    ELSE
      CALL ERROROUT("Type mismatch")
    END IF
  END SELECT
END SUB

SUB FCN.SCREEN
  IF INSTR("AXY", TOKEN$(J)) = 0 THEN CALL ERROROUT("A/X/Y expected")
  T2$ = "LD" + TOKEN$(J)
  SELECT CASE MID$(Q$, J + 4, 4)
  CASE "N,N)"
    T$ = HEX4$(40 * (DEC(TOKEN$(J + 3)) - 1) + DEC(TOKEN$(J + 5)) - 1 + DEC(OPTIONS$(14, 0)))
    CALL PRINTOUT("LDA", "#$" + MID$(T$, 1, 2) + COMMENT$(7))
    CALL PRINTOUT("STA", "SCREENH")
    CALL PRINTOUT("LDA", "#$" + MID$(T$, 3, 2))
    J = J + 7
  CASE "V,N)", "N,V)", "V,V)"
    MID$(INCLUDE$, 6, 1) = "*"
    J = J + 3: CALL LDX("Y"): J = J - 3
    J = J + 5: CALL LDX("A"): J = J + 2
    CALL PRINTOUT("STA", "TEMPL" + COMMENT$(7))
    CALL PRINTOUT("LDA", "#$28")
    CALL PRINTOUT("LDX", "#$" + MID$(OPTIONS$(14, 0), 1, 2))
    CALL PRINTOUT("JSR", "MULADD")
    CALL PRINTOUT("STX", "SCREENH")
  CASE ELSE
    CALL ERROROUT("Type mismatch")
  END SELECT
  CALL PRINTOUT("STA", "SCREENL")
  CALL PRINTOUT("LDY", "#$00")
  CALL PRINTOUT(T2$, "(SCREENL),Y")
END SUB

FUNCTION FINDTOKEN (TEXT$, START)
  FOR T = START TO 31
    IF TOKEN$(T) = TEXT$ THEN EXIT FOR
  NEXT T
  IF T > 31 THEN T = -1
  FINDTOKEN = T - START
END FUNCTION

SUB FIXSTR (A$)
  AA$ = "": B = 0
  FOR A = 1 TO LEN(A$)
    B$ = MID$(A$, A, 1)
    IF B = 0 THEN
      IF B$ = "{" THEN B = 1: BB$ = "" ELSE AA$ = AA$ + B$
    ELSE
      IF B$ = "}" THEN
        B = 0: CALL TOHEX(BB$): AA$ = AA$ + CHR$(DEC(BB$))
      ELSE
        BB$ = BB$ + B$
      END IF
    END IF
  NEXT A
  A$ = AA$
END SUB

FUNCTION HEX2$ (DECIMAL)
  A$ = HEX$(DECIMAL)
  A = LEN(A$): IF A > 2 THEN CALL ERROROUT("Number too large")
  HEX2$ = STRING$(2 - A, "0") + A$
END FUNCTION

FUNCTION HEX4$ (DECIMAL)
  IF DECIMAL < 0 THEN DECIMAL = DECIMAL + 65536
  A$ = HEX$(DECIMAL)
  A = LEN(A$): IF A > 4 THEN CALL ERROROUT("Number too large")
  HEX4$ = STRING$(4 - A, "0") + A$
END FUNCTION

FUNCTION INVERTSIGN$ (SIGN$)
  SELECT CASE SIGN$
  CASE "="
    INVERTSIGN$ = "<>"
  CASE "<"
    INVERTSIGN$ = ">="
  CASE "<="
    INVERTSIGN$ = ">"
  CASE ">"
    INVERTSIGN$ = "<="
  CASE ">="
    INVERTSIGN$ = "<"
  CASE "<>"
    INVERTSIGN$ = "="
  END SELECT
END FUNCTION

SUB J5 (W$)
  IF TOKEN$(J) <> "A" THEN ERROROUT ("Illegal operation")
  IF MID$(Q$, J + 5, 1) = "N" THEN
    CALL PRINTOUT(W$, "#$" + BYTE$(TOKEN$(J + 4)) + COMMENT$(5))
    J = J + 5
  ELSE
    IF MID$(Q$, J + 5, 1) <> "V" THEN CALL ERROROUT("Type mismatch")
    CALL PRINTOUT(W$, TOKEN$(J + 4) + COMMENT$(5))
    J = J + 5
  END IF
END SUB

'W$=variable to load into (AXY)  TOKEN$(J)=value to load (AXYVN)
SUB LDX (W$)
  IF MID$(Q$, J + 1, 1) = "N" THEN
    CALL PRINTOUT("LD" + W$, "#$" + BYTE$(TOKEN$(J)) + ";" + W$ + " = " + TOKEN$(J))
  ELSE
    IF MID$(Q$, J + 1, 1) <> "V" THEN CALL ERROROUT("Type mismatch")
    IF TOKEN$(J) <> W$ THEN
      SELECT CASE TOKEN$(J)
      CASE "A", "X", "Y"
        IF (W$ = "X" OR W$ = "Y") AND (TOKEN$(J) = "X" OR TOKEN$(J) = "Y") THEN CALL ERROROUT("Illegal XY assign")
        CALL PRINTOUT("T" + TOKEN$(J) + W$, ";" + W$ + " = " + TOKEN$(J))
      CASE ELSE
        CALL PRINTOUT("LD" + W$, TOKEN$(J) + ";" + W$ + " = " + TOKEN$(J))
      END SELECT
    END IF
  END IF
END SUB

FUNCTION PARSE$ (S$)
       c$ = "CLS     POKE    LOCATE  PRINT   PUT     CHAR    SPRITE  "
  c$ = c$ + "CHARSET TITLE   RETURN  SUB     GOSUB   PUT(    GOTO    "
  c$ = c$ + "IF      THEN    UP      DOWN    LEFT    RIGHT   DEFINE  "
  c$ = c$ + "DO      LOOP    EXIT    KEYPAD  INPUT   FOR     TO      "
  c$ = c$ + "NEXT    DATA    POS     MOVEUP  }       SCREEN  PALETTE "
  c$ = c$ + "MEMCOPY MEMAREA IF_     FOR_    MULADD  PUSH    POP     "
  c$ = c$ + "SET     SPRITES INTERNALDLIST   ATTRACT END     DIV8    "
  c$ = c$ + "DIV16   MUL8    SELECT  CASE    CASE_   DO_     LOOP_   "
  c$ = c$ + "ELSE    AUTHOR  SOUND   VOLUME  MISSILES"
       F$ = "JOYX(   JOYY(   TRIG(   INKEY   CHR$(   SCREEN( PEEK(   "
  F$ = F$ + "JOYTRIG(JOYTRIG2"
       R$ = "LDA     LDX     LDY     STA     STX     STY     TAX     "
  R$ = R$ + "TAY     TXA     TYA     TXS     TSX     JMP     JSR     "
  R$ = R$ + "RTS     CMP     CPX     CPY     BNE     BEQ     BMI     "
  R$ = R$ + "BPL     BCC     BCS     BVC     BVS     ADC     SBC     "
  R$ = R$ + "AND     ORA     EOR     INC     DEC     INX     INY     "
  R$ = R$ + "DEX     DEY     ROR     ROL     LSR     ASL     CLC     "
  R$ = R$ + "SEC     CLD     SED     CLI     SEI     BRK     CLV     "
  R$ = R$ + "PHA     PLA     PHP     PLP     BRK     BIT     RTI     "
  R$ = R$ + "NOP     .ORG    .END    #INCLUDE.BYTE   .WORD   .TEXT   "
  IF DEBUG = 1 THEN CALL CONSCOLOR(6): PRINT #3, "": PRINT #3, S$
  L = LEN(S$): T = 1: Q = 0: Q$ = ""
  DO WHILE T < LEN(S$)
    TOKEN$ = "": TOKTYPE$ = ""
A:
    A$ = UCASE$(MID$(S$, T, 1))
    IF A$ = " " THEN T = T + 1: GOTO A 'strip spaces
    TOKEN$ = TOKEN$ + A$
    'combine functions, commands, labels
    IF ((A$ >= "G" AND A$ <= "Z") OR A$ = "." OR A$ = "#" OR A$ = "_" OR (((A$ >= "0" AND A$ <= "9") OR A$ = "$") AND TOKTYPE$ = "V") OR ((A$ >= "A" AND A$ <= "F") AND (TOKTYPE$ = "" OR TOKTYPE$ = "V"))) AND A$ <> "(" THEN
      TOKTYPE$ = "V"
      IF T < L THEN
        A$ = UCASE$(MID$(S$, T + 1, 1))
        IF (A$ >= "A" AND A$ <= "Z") OR A$ = "$" OR A$ = "." OR A$ = "#" OR A$ = "_" OR (A$ = ":" AND Q = 0) OR A$ = "(" OR (A$ >= "0" AND A$ <= "9") THEN
          T = T + 1: GOTO A
        END IF
      END IF
      IF DEBUG = 1 THEN CALL CONSCOLOR(8)
    END IF
    'combine numbers
    IF ((A$ >= "0" AND A$ <= "9") AND TOKTYPE$ <> "V") OR ((A$ = "$" OR A$ = "@") AND TOKTYPE$ = "") OR ((A$ >= "A" AND A$ <= "F") AND TOKTYPE$ = "N") THEN
      TOKTYPE$ = "N"
      IF T < L THEN
        A$ = MID$(S$, T + 1, 1)
        IF (A$ >= "0" AND A$ <= "9") OR (A$ >= "A" AND A$ <= "F") THEN T = T + 1: GOTO A
      END IF
    END IF
    'combine relational operators
    IF TOKEN$ = "=" OR TOKEN$ = "<" OR TOKEN$ = ">" THEN
      A$ = MID$(S$, T + 1, 1)
      IF A$ = "=" OR A$ = "<" OR A$ = ">" THEN TOKEN$ = TOKEN$ + A$: T = T + 1
    END IF
    'combine ++ and --
    IF TOKEN$ = "+" OR TOKEN$ = "-" THEN
      A$ = MID$(S$, T + 1, 1)
      IF A$ = "+" OR A$ = "-" THEN TOKEN$ = TOKEN$ + A$: T = T + 1
    END IF
    'fine tune results
    SELECT CASE TOKEN$
    CASE "'", ";"
      EXIT DO
    CASE ":"
      TOKTYPE$ = ":": IF DEBUG = 1 THEN CALL CONSCOLOR(5)
    CASE ","
      TOKTYPE$ = ",": IF DEBUG = 1 THEN CALL CONSCOLOR(5)
    CASE ")"
      TOKTYPE$ = ")": IF DEBUG = 1 THEN CALL CONSCOLOR(5)
    CASE "(", "XOR", "OR", "AND", ">=", "<=", "<>", "<", ">", ">>=", "<<", "=", "-", "+", "--", "++", "/", "*"
      TOKTYPE$ = "O"
      IF DEBUG = 1 THEN CALL CONSCOLOR(1)
    CASE CHR$(34)       'string ""
      TOKTYPE$ = "S"
      B = INSTR(T + 1, S$, CHR$(34))
      IF B = 0 THEN CALL ERROROUT("String ended unexpectedly")
      TOKEN$ = MID$(S$, T + 1, B - T - 1)       'extract string
      T = B                                     'skip to end of string
    CASE ELSE
      IF MID$(TOKEN$, LEN(TOKEN$), 1) = ":" THEN
        TOKEN$ = MID$(TOKEN$, 1, LEN(TOKEN$) - 1)
        TOKTYPE$ = "L": IF DEBUG = 1 THEN CALL CONSCOLOR(4)
      END IF
      IF LEN(TOKEN$) <= 8 THEN
        B = INSTR(1, c$, TOKEN$ + SPACE$(8 - LEN(TOKEN$)))
        IF B > 0 THEN TOKTYPE$ = "C": IF DEBUG = 1 THEN CALL CONSCOLOR(3)
        B = INSTR(1, F$, TOKEN$ + SPACE$(8 - LEN(TOKEN$)))
        IF B > 0 THEN TOKTYPE$ = "F": IF DEBUG = 1 THEN CALL CONSCOLOR(3)
        B = INSTR(1, R$, TOKEN$ + SPACE$(8 - LEN(TOKEN$)))
        IF B > 0 THEN TOKTYPE$ = "R": IF DEBUG = 1 THEN CALL CONSCOLOR(2)
      END IF
      IF TOKTYPE$ = "L" THEN TOKEN$ = TOKEN$ + ":"
    END SELECT
    IF TOKTYPE$ = "N" THEN
      CALL TOHEX(TOKEN$)
      IF DEBUG = 1 THEN CALL CONSCOLOR(15)
    END IF
    T = T + 1
    IF DEBUG = 1 THEN PRINT #3, "["; TOKEN$; " "; TOKTYPE$; "] "; : CALL CONSCOLOR(7)
    Q$ = Q$ + TOKTYPE$
    TOKEN$(Q) = TOKEN$: Q = Q + 1
  LOOP
  IF DEBUG = 1 THEN CALL CONSCOLOR(7)
  PARSE$ = Q$ + "E"
'(S)tring
'(N)umber
'(V)ariable
'(O)perator
'(F)unction
'(C)ommand
'(L)abel
'(:)
'(,)
'(E)nd
END FUNCTION

SUB PRINTASC (S$)
  A$ = ""
  FOR A = 1 TO LEN(S$)
    B = ASC(MID$(S$, A, 1))
    SELECT CASE B
    CASE 0 TO 31, 34, 92, 255 '<32, ", \
      IF A$ <> "" THEN CALL PRINTOUT(".TEXT", CHR$(34) + A$ + CHR$(34)): A$ = ""
      CALL PRINTOUT(".BYTE", "$" + HEX2$(B))
    CASE ELSE
      A$ = A$ + CHR$(B)
      IF LEN(A$) >= 40 THEN CALL PRINTOUT(".TEXT", CHR$(34) + A$ + CHR$(34)): A$ = ""
    END SELECT
  NEXT A
  IF A$ <> "" THEN CALL PRINTOUT(".TEXT", CHR$(34) + A$ + CHR$(34))
END SUB

SUB PRINTOUT (OPCODE$, OPERANDS$)
SHARED DASM
  IF DASM = TRUE THEN
    IF MID$(OPCODE$, 1, 5) = ".TEXT" THEN MID$(OPCODE$, 1, 5) = ".BYTE"
    IF MID$(OPCODE$, 1, 5) = ".WORD" OR MID$(OPCODE$, 1, 5) = ".BYTE" OR MID$(OPCODE$, 1, 4) = ".ORG" THEN OPCODE$ = " " + OPCODE$
  END IF
  IF MID$(OPERANDS$, 1, 1) = "~" THEN
    OPERANDS$ = MID$(OPERANDS$, 2)
    IF LEN(OPCODE$) < 32 THEN OPCODE$ = OPCODE$ + SPACE$(32 - LEN(OPCODE$))
  ELSE
    PRINT #2, SPACE$(8);
    OPCODE$ = OPCODE$ + SPACE$(8 - LEN(OPCODE$) MOD 8)
    A = INSTR(OPERANDS$, ";")
    IF A > 0 AND A <= 17 THEN
      OPERANDS$ = MID$(OPERANDS$, 1, A - 1) + SPACE$(17 - A) + MID$(OPERANDS$, A)
    END IF
  END IF
  PRINT #2, OPCODE$; OPERANDS$
END SUB

SUB STX (W$)
'W$=value to store (AXY), TOKEN$(J)=variable to store into (AXYV)
  IF MID$(Q$, J + 1, 1) <> "V" THEN CALL ERROROUT("Type mismatch")
  IF TOKEN$(J) <> W$ THEN
    SELECT CASE TOKEN$(J)
    CASE "A", "X", "Y"
      IF (W$ = "X" OR W$ = "Y") AND (TOKEN$(J) = "X" OR TOKEN$(J) = "Y") THEN CALL ERROROUT("Illegal XY assign")
      CALL PRINTOUT("T" + W$ + TOKEN$(J), "")
    CASE ELSE
      CALL PRINTOUT("ST" + W$, TOKEN$(J))
    END SELECT
  END IF
END SUB

SUB TOHEX (A$)
   SELECT CASE MID$(A$, 1, 1)
   CASE "$" 'hex
     A$ = MID$(A$, 2)
     A = LEN(A$): IF A > 4 THEN CALL ERROROUT("Number too large")
     A$ = STRING$(4 - A, "0") + A$
   CASE "@" 'bin
     B = 0: FOR A = 2 TO LEN(A$): B = B * 2 + VAL(MID$(A$, A, 1)): NEXT A
     A$ = HEX4$(B)
   CASE ELSE 'dec
     A$ = HEX4$(VAL(A$))
   END SELECT
END SUB

