# 5200BAS -- 5200 Basic compiler
# Copyright 2001-2002 by Jeffry Johnston
# Python conversion by James Higgs 2016

# Conversion issues:
# - Nondescriptive variable names
# - GOTO
# - Array indexing (MID$() to s[n:m])
# - Stack objects
# - include system (INCLUDE$)

# orginal includek.bas was split off because of 64k page limit in QBX
# reincorporated into this file

import sys

def VERSION():              # VERSION constant
    return "1.96"

# output devices:
# PRINT #1 = ?
# PRINT #2 = stderr
# PRINT #3 = CONS:
# PRINT #4 = OUTPUT FILE

# 5200Bas memory map:
# 0000-0018      reserved
# 0019-00FF      free
# 0100-01FF      reserved (stack)
# 0200-021B      reserved (shadows)
# 021C-3FFF      free
# 4000-BFFF      ROM
# 4000-xxFF      sprites
# xx00-xxFF      character sets
# xx00-xxxx      code & DLI's
# xxxx-xxxx      display lists
# xxxx-BFE6      keypad IRQ

# replacements:
# A$ replaced by currentParseChar
# TOKEN$() replaced by tokenArray
# Q$ replaced by tokenTypes
# DASM replaced by useDASM 

# global variables (such as Python understands the concept)
gCurrentParseLine = ' '
tokenArray = { }
tokenTypes = ""             # NB: tokenTypes index = Q$ index - 1!
DOSTACK = []                # list as stack (use append() and pop())
FORSTACK = []
SELSTACK = []               # 2-dimensional array
IFSTACK = []
numOPTIONS = 19             # 18 in QBX
# 2-dimensional array
# note: strings address option "B000" changed to "BB00"
sOPTIONS = [ ["FC03","FCB8","BC20","FEA1","FD02","BC00","0000",
            "0000","0000","0000","0000","0000","0000","0000",
            "1000","3000","B800","BB00","BFB4"], 
            ["", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""] ]

# Note: user-defined subroutine addresses seem to be kept in sOPTIONS[1][]  !
J = -1
LINENUM = 0      # ; FIRSTORG = True
DOCOUNT = 0 ; FORCOUNT = 0 ; SELCOUNT = 0; IFCOUNT = 0
#DODEPTH = 0 ; FORDEPTH = 0 ; SELDEPTH = 0; IFDEPTH = 0      # use len(stack/list)
SCRMODE = 2 ; TITLE = True ; SPRRES = 0
sST = "" ; sCHAR = "" ; sSPRITE = ""
sTITLE = "UNTITLED"
sCOPYRIGHT = "COPYRIGHT 2002 ATARI"
# INCLUDES$ replaced with includeArray (intialised with a list of Falses)
includeArray = [ False for i in range(0, 23) ]                   # 23 spaces
DEBUG = True #False
useDASM = True
useANSIDebugColours = False                 # not supported in Python IDE's?

# Main function (to allow forward declarations)
# args = concatenated arguments
def main(args):
    #OPEN "CONS:" FOR OUTPUT AS #3
    print ("5200BAS Basic Compiler, version " + VERSION() + ".  Copyright 2001-2002 by Jeffry Johnston.")
    print ("")

    # Parse options
    #replaced F$ with args
    args.lstrip()
    args.rstrip()

    # replaced STARTADDR with startAddr
    # replaced T$ with romSize
    startAddr = "$4000"
    romSize = "32"
    if (args.find("/16 ") > -1):
           startAddr = "$8000"
           romSize = "16"
           args = args.replace("/16 ", "")

    # replaced DASM with useDASM
    useDASM = False
    if (args.find("/D ") > -1):
           useDASM = True
           args = args.replace("/D ", "")

    # replaced DCOLOR with useANSIDebugColours
    useANSIDebugColours = True
    if (args.find("/M ") > -1):
           useANSIDebugColours = False
           args = args.replace("/M ", "")

    # replaced DOT with pos
    global DEBUG
    DEBUG = False
    if (args.find("*") > -1):
           DEBUG = True
           args = args.replace("*", "")

    # Show help if no arguments, and exit
    if (0 == len(args)):
      print ("Usage: 5200BAS [/16] [/D] [/M] file[.bas][*]")
      print ("  /16  Compile for 16k ROM (default: 32k)")
      print ("  /D   Compile for DASM assembler (default: TASM)")
      print ("  /M   Don't use ANSI colors in debug output")
      print ("  *    Debug output")
      print ("")
      print ("Error: No input file given")
      exit()

    # converted F$ to inFileName
    # converted F2$ to outFileName
    # converted F3$ to postIncFileName
    # converted F4$ to preIncFileName
           
    ##DOT = INSTR(F$, ".")
    ##IF DOT = 0 THEN
    ##  F2$ = F$ + ".ASM": F3$ = F$ + ".1": F4$ = F$ + ".2": F$ = F$ + ".BAS"
    ##ELSE
    ##  F2$ = MID$(F$, 1, DOT - 1): F3$ = F2$ + ".1": F4$ = F2$ + ".2"
    ##  F2$ = F2$ + ".ASM"
    ##END IF

    # Handle case where .BAS is supplied or not
    pos = args.find(".")
    if (-1 == pos):
        outFileName = args + ".ASM"
        postIncFileName = args + ".1"
        preIncFileName = args + ".2"
        inFileName = args + ".BAS"
    else:
        outFileName = args[0:pos]
        postIncFileName = outFileName + ".1"
        preIncFileName = outFileName + ".2"
        outFileName = outFileName + ".ASM"
        inFileName = args

    if (useDASM):
        print ("Compiling " + romSize + "k ROM for DASM 2.12")
    else:
        print ("Compiling " + romSize + "k ROM for TASM 2.2")
                                                                               
    print ("Input file  : ", inFileName)
    print ("Output file : ", outFileName)
    print ("Pre-include : ", preIncFileName)
    print ("Post-include: ", postIncFileName)
           
    ##OPEN F$ FOR INPUT AS #1
    ##OPEN F2$ FOR OUTPUT AS #2
    global inFile, outFile
    inFile = open(inFileName, 'r')
    outFile = open(outFileName, 'w')

    PRINTOUT(";========================================================", "~")
           
    sT = "; ASM Code Produced by 5200BAS " + VERSION()
    ##T = VAL(MID$(TIME$, 1, 2)): T2$ = "a"
    ##IF T > 11 THEN T = T - 12: T2$ = "p"
    ##IF T = 0 THEN T = 12
    ##T3$ = LTRIM$(STR$(T))
    ##IF T > 9 THEN T = 10 ELSE T = 11
    ##T$ = T$ + SPACE$(T - LEN(VERSION$)) + MID$(DATE$, 4, 2)
    ##T$ = T$ + MID$("  JanFebMarAprMayJunJulAugSepOctNovDec", 3 * VAL(MID$(DATE$, 1, 2)), 3)
    ##T$ = T$ + MID$(DATE$, 7, 4) + "  "
    #PRINTOUT(T$ + T3$ + MID$(TIME$, 3, 3) + T2$, "~")
    PRINTOUT(sT, "~")
    PRINTOUT(";========================================================", "~")

    if (useDASM):
        PRINTOUT("processor", "6502")
        
    ##IF STARTADDR$ <> "$4000" THEN
    ##  CALL PRINTOUT(".ORG    $4000", "~")
    ##  CALL PRINTOUT("LDA", "#$00;this is included for the assembler")
    ##END IF
    if (startAddr != "$4000"):
        PRINTOUT(".ORG    $4000", "~")
        PRINTOUT("LDA", "#$00;this is included for the assembler")
        
    PRINTOUT(".ORG    " + startAddr, "~")
    PRINTOUT("ATARI   .EQU    1", "~")          # 0 = Atari XL, 1 = 5200
    PRINTOUT("#INCLUDE        EQUATES.INC", "~")
    PRINTOUT("#INCLUDE        HEADER.INC", "~")
    if (useDASM):
      PRINTOUT('#INCLUDE        "' + preIncFileName + '"', "~")
    else:
      PRINTOUT('#INCLUDE        "' + BACKSLASH(preIncFileName) + '"', "~")

    FIRSTORG = True
    
    # Main file parsing loop
    global J, tokenTypes, tokenArray, LINENUM
    for line in inFile:
        s = line.rstrip(" \n")
        s = s.lstrip();
        #s += " "
        LINENUM += 1
        if (len(s) < 2):        # handle empty lines
            J = -1
            continue
        elif (s[0] == "'" or s[0] == ";"):
            outFile.write(";" + s[1:] + "\n")
            J = -1
            continue
        elif (s[0:2].upper() == "RAW"):
            outFile.write("        " + s[3:].lstrip() + "\n")
            J = -1
            continue
        # Reset token array and token type array for this line
        tokenArray = [] #.removeAll()
        tokenTypes = PARSE(s)
        J = 0
    #D0:
        while (J > -1 and J < len(tokenTypes)):
            if (DEBUG):
                print (tokenTypes[J:] + " ")

            #SELECT CASE MID$(Q$, J + 1, 1)
            tokenType = tokenTypes[J]
            if (tokenType == 'E'):      # End of line
                J = -1
            elif (tokenType == ':'):    # :
                J = J + 1
            elif (tokenType == 'L'):    # LABEL
                PRINTOUT(tokenArray[J], "~")
                J = J + 1
            elif (tokenType == 'R'):    # ASM / REG
                T = 8
                if (s[0] == '#'):
                    T = 0
                if (s[1:3] == "ORG"):
                    if (FIRSTORG):
                        PRINTOUT("ENDMAIN JMP     ENDMAIN         ;Endless loop", "~")
                        if (useDASM):
                            PRINTOUT('#INCLUDE        "' + sF3 + '"~')
                        else:
                            PRINTOUT('#INCLUDE        "' + BACKSLASH(sF3) + '"~')
                        FIRSTORG = False
                    T = 0
                outFile.write("        " + s + "\n")
                J = -1
                continue
            elif (tokenType == 'V'):
                if (tokenArray[J+1] != '='):
                    ERROROUT("Missing = (equivalence operator)")
                lhsToken = tokenArray[J]
                rhsToken = tokenArray[J+2]
         # TODO - Use name to function map?
                if (lhsToken != rhsToken):
                    if (rhsToken == "JOYTRIG2"):
                        FCN_JOYTRIG2()
                    elif (rhsToken == "JOYTRIG("):
                        FCN_JOYTRIG()
                    elif (rhsToken == "SCREEN("):
                        FCN_SCREEN()
                    elif (rhsToken == "JOYX("):
                        FCN_JOYX()
                    elif (rhsToken == "JOYY("):
                        FCN_JOYY()
                    elif (rhsToken == "PEEK("):
                        FCN_PEEK()
                    elif (rhsToken == "INKEY"):
                        FCN_INKEY()
                    else:
                        if (lhsToken in "AXY"):
                            J += 2
                            LDX(tokenArray[J-2])
                            J += 1
                        else:
                            if (rhsToken in "AXY"):
                                PRINTOUT("ST" + rhsToken, lhsToken + COMMENT(3))
                                J += 3
                            else:
                                J += 2
                                LDX("A")
                                J -= 2
                                PRINTOUT("STA", lhsToken + COMMENT(3))
                                J += 3
                else:
                    token3 = tokenArray[J + 3]
                    if (token3 == '+'):
                        if (tokenArray[J+4] == "0001" and lhsToken != "A"):
                            if (lhsToken in "XY"):
                                PRINTOUT("IN" + lhsToken, COMMENT(5))
                                J = J + 5
                            else:
                                PRINTOUT("INC", lhsToken + COMMENT(5))
                                J = J + 5
                        else:
                            PRINTOUT("CLC", "")
                            J5("ADC")
                    elif (token3 == '-'):
                        if (tokenArray[J+4] == "0001" and lhsToken != "A"):
                            if (lhsToken in "XY"):
                                PRINTOUT("DE" + lhsToken, COMMENT(5))
                                J = J + 5
                            else:
                                PRINTOUT("DEC", lhsToken + COMMENT(5))
                                J = J + 5
                        else:
                            PRINTOUT("SEC", "")
                            J5("SBC")
                    elif (token3 == "++"):
                        if (lhsToken != "A"): ERROROUT("Type mismatch")
                        J5("ADC")
                    elif (token3 == "--"):
                        if (lhsToken != "A"): ERROROUT("Type mismatch")
                        J5("SBC")
                    elif (token3 == "*"):
                        if (lhsToken != "A"): ERROROUT("Type mismatch")
                        if (tokenTypes[J+4] != 'N'): ERROROUT("Type mismatch")
                        T = log(DEC(tokenArray[J+4])) / log(2)
                        T2 = 1
                        while (T2 <= T):
                            if (T2 == 1):
                                if (useDASM): PRINTOUT("ASL", COMMENT(5))
                                else: PRINTOUT("ASL", "A" + COMMENT(5))
                            else:
                                if (useDASM): PRINTOUT("ASL", "")
                                else: PRINTOUT("ASL", "A")
                            T2 += 1
                        J += 5
                    elif (token3 == "/"):
                        if (lhsToken != "A"): ERROROUT("Type mismatch")
                        if (tokenTypes[J+4] != 'N'): ERROROUT("Type mismatch")
                        T = log(DEC(tokenArray[J+4])) / log(2)
                        T2 = 1
                        while (T2 <= T):
                            if (T2 == 1):
                                if (useDASM): PRINTOUT("LSR", COMMENT(5))
                                else: PRINTOUT("LSR", "A" + COMMENT(5))
                            else:
                                if (useDASM): PRINTOUT("LSR", "")
                                else: PRINTOUT("LSR", "A")
                            T2 += 1
                        J += 5
                    elif (token3 == "AND"):
                        J5("AND")
                    elif (token3 == "OR"):
                        J5("ORA")
                    elif (token3 == "XOR"):
                        J5("EOR")
                    else:
                        ERROROUT("Invalid operator")
            elif (tokenType == 'F'):
                ERROROUT("Illegal function call")               
            elif (tokenType == 'C'):                    # Command
                token = tokenArray[J]
                if (token == "SOUND"): CMD_SOUND()
                elif (token == "VOLUME"): CMD_VOLUME()
                elif (token == "MUL8"): CMD_MUL8()
                elif (token == "DIV8"): CMD_DIV8()
                elif (token == "DIV16"): CMD_DIV16()
                elif (token == "ATTRACT"): CMD_ATTRACT()
                elif (token == "DLIST"): CMD_DLIST()
                elif (token == "PUT"): CMD_PUT()
                elif (token == "INTERNAL"): CMD_INTERNAL()
                elif (token == "SPRITES"): CMD_SPRITES()
                elif (token == "MISSILES"): CMD_MISSILES()
                elif (token == "SET"): CMD_SET()
                elif (token == "PUSH"): CMD_PUSH()
                elif (token == "POP"): CMD_POP()
                elif (token == "MULADD"): CMD_MULADD()
                elif (token == "MEMAREA"): CMD_MEMAREA()
                elif (token == "MEMCOPY"): CMD_MEMCOPY()
                elif (token == "PALETTE"): CMD_PALETTE()
                elif (token == "SCREEN"): CMD_SCREEN()
                elif (token == "}"): CMD_RIGHTBRACE()
                elif (token == "MOVEUP"): CMD_MOVEUP()
                elif (token == "POS"): CMD_POS()
                elif (token == "CHARSET"): CMD_CHARSET()
                elif (token == "POKE"): CMD_POKE()
                elif (token == "INPUT"): CMD_INPUT()
                elif (token == "KEYPAD"): CMD_KEYPAD()
                elif (token == "DEFINE"): CMD_DEFINE()
                elif (token == "DATA"): CMD_DATA()
                elif (token == "CLS"): CMD_CLS()
                elif (token == "LOCATE"): CMD_LOCATE()
                elif (token == "RIGHT"): CMD_RIGHT()
                elif (token == "LEFT"): CMD_LEFT()
                elif (token == "DOWN"): CMD_DOWN()
                elif (token == "UP"): CMD_UP()
                elif (token == "PRINT"): CMD_PRINT()
                elif (token == "TITLE"): CMD_TITLE()
                elif (token == "AUTHOR"): CMD_AUTHOR()
                elif (token == "IF"): CMD_IF(False)
                elif (token == "IF_"): CMD_IF(True)
                elif (token == "ELSE"): CMD_ELSE()
                elif (token == "GOTO"): CMD_GOTO()
                elif (token == "GOSUB"): CMD_GOSUB()
                elif (token == "RETURN"): CMD_RETURN()
                elif (token == "SUB"): CMD_SUB()
                elif (token == "END"): CMD_END()
                elif (token == "SELECT"): CMD_SELECT()
                elif (token == "CASE_"): CMD_CASE(True)
                elif (token == "CASE"): CMD_CASE(False)
                elif (token == "DO"): CMD_DO(False)
                elif (token == "DO_"): CMD_DO(True)
                elif (token == "LOOP"): CMD_LOOP(False)
                elif (token == "LOOP_"): CMD_LOOP(True)
                elif (token == "EXIT"): CMD_EXIT()
                elif (token == "FOR"): CMD_FOR(False)
                elif (token == "FOR_"): CMD_FOR(True)
                elif (token == "NEXT"): CMD_NEXT()
                else:
                    ERROROUT("Unknown command")
            else:
                ERROROUT("Syntax error")
            #end while

    if (FIRSTORG):
        PRINTOUT("ENDMAIN JMP     ENDMAIN         ;Endless loop", "~")
        if (useDASM):
            PRINTOUT('#INCLUDE        "' + postIncFileName + '"', '~')
        else:
            PRINTOUT('#INCLUDE        "' + BACKSLASH(postIncFileName) + '"', '~')

                              
    PRINTOUT("", "~")
    PRINTOUT(";-----------------------------------------------------------", "~")
    PRINTOUT("; Strings", "~")
    PRINTOUT(";-----------------------------------------------------------", "~")
    PRINTOUT(".ORG    $" + sOPTIONS[0][17], "~")
    if (DEBUG):
        print ("")
        CONSCOLOR(15)
        print ('"' + sST + '"')
        CONSCOLOR(7)

    PRINTASC(sST)

    if (sOPTIONS[1][5] == ""):
        INCLUDES(sOPTIONS, useDASM, 4)       # KEYPAD

    if (sOPTIONS[1][2] == ""):
        INCLUDES(sOPTIONS, useDASM, 3)       # DEFERVBI

    #'IF OPTIONS$(6, 1) = "" THEN
    #'  CALL INCLUDES(INCLUDE$, OPTIONS$(), DASM, 5) 'JOYSTICK
    #'END IF

    # "Default" display list
    if (sOPTIONS[1][18] == ""):
        PRINTOUT("", "~")
        PRINTOUT(";-----------------------------------------------------------", "~")
        PRINTOUT("; Display List", "~")
        PRINTOUT(";-----------------------------------------------------------", "~")
        PRINTOUT(".ORG    $" + sOPTIONS[0][18], "~")
        PRINTOUT(".WORD", "$7070;skip 24 scan lines")
        PRINTOUT(".BYTE", "$70")
        PRINTOUT(".BYTE", "$" + HEX2(0x40 + SCRMODE) + ";set up gr.mode" + SCRMODE + " screen")
        PRINTOUT(".WORD", "$" + sOPTIONS[0][14] + ";address of screen memory")
        for i in range(23):
            PRINTOUT(".BYTE", "$" + HEX2(SCRMODE))
        PRINTOUT(".BYTE", "$41")
        PRINTOUT(".WORD", "$" + sOPTIONS[0][18] + ";jump back to top of list")

    PRINTOUT("", "~")
    PRINTOUT(";-----------------------------------------------------------", "~")
    PRINTOUT("; Monitor Information", "~")
    PRINTOUT(";-----------------------------------------------------------", "~")
    if (TITLE):
        PRINTOUT(".ORG    $BFD4", "~")
        sT = sCOPYRIGHT
        sT.center(20)
        sT = ATASCII(sT)
        PRINTASC(sT)
        sT = sTITLE
        sT.center(20)
        sT = ATASCII(sT)
        PRINTASC(sT)
        PRINTOUT(".BYTE", "$B5")

    PRINTOUT(".ORG    $BFFD", "~")
    PRINTOUT(".BYTE", "$FF" + "; Skip title")
    PRINTOUT(".WORD", startAddr + ";MAIN")
    PRINTOUT(".END", "~")

    outFile.close()

    # Write post-include flle ("filename.1")
    outFile = open(postIncFileName, 'w')

    INCLUDES(sOPTIONS, useDASM, 0)
    if (TITLE):
        INCLUDES(sOPTIONS, useDASM, 1)   # Y2K

    INCLUDES(sOPTIONS, useDASM, 2)       # CLEARRAM
    PRINTOUT(";-----------------------------------------------------------", "~")
    PRINTOUT("; IRQ VECTORS", "~")
    PRINTOUT(";-----------------------------------------------------------", "~")
    PRINTOUT("OPTIONS:", "~")
    for i in range(14):
        PRINTOUT(".WORD   $" + sOPTIONS[0][i], "~")

    if (sOPTIONS[1][16] == ""):
        PRINTOUT("", "~")
        PRINTOUT(".ORG    $" + sOPTIONS[0][16], "~")
        PRINTOUT("#INCLUDE        ASCIISET.INC", "~")

    outFile.close()

    # Write pre-Include file ("filename.2")
    outFile = open(preIncFileName, 'w')

    PRINTOUT(";-----------------------------------------------------------", "~")
    PRINTOUT("; INIT", "~")
    PRINTOUT(";-----------------------------------------------------------", "~")
    if (TITLE):
        PRINTOUT("TITLE   .EQU    1", "~")
    else:
        PRINTOUT("TITLE   .EQU    0", "~")

    PRINTOUT("LDA", "#$" + sOPTIONS[0][18][0:2] + "; Display List high")
    PRINTOUT("STA", "SDLSTH")
    PRINTOUT("LDA", "#$" + sOPTIONS[0][18][2:4] + "; Display List low")
    PRINTOUT("STA", "SDLSTL")
    if (sOPTIONS[1][5] == ""):
        #'IF sOPTIONS(6, 1) = "" THEN
        #'  PRINTOUT("LDA", "#$C0; Enable deferred keyboard & trigger")
        #'ELSE
        PRINTOUT("LDA", "#$40; Enable deferred keyboard")
        #'END IF
        PRINTOUT("STA", "POKMSK")
        PRINTOUT("STA", "IRQEN")
    #'ELSE
      #'IF sOPTIONS(6, 1) = "" THEN
      #'  PRINTOUT("LDA", "#$80; Enable joystick trigger")
      #'  PRINTOUT("STA", "POKMSK")
      #'  PRINTOUT("STA", "IRQEN")
      #'END IF

    PRINTOUT("LDA", "#$" + sOPTIONS[0][16][0:2])
    PRINTOUT("STA", "CHBASE")
    PRINTOUT("LDA", "#$22")
    PRINTOUT("STA", "SDMCTL")
    if (sOPTIONS[1][3] != ""):
        PRINTOUT("LDA", "#$C0 ; Enable DLI & VBI")
        PRINTOUT("STA", "NMIEN")
    else:
        PRINTOUT("LDA", "#$40 ; Enable VBI")
        PRINTOUT("STA", "NMIEN")

    PRINTOUT("LDA", "#$" + sOPTIONS[0][14][0:2])
    PRINTOUT("STA", "LOCATEH")
    PRINTOUT("LDA", "#$0E")
    PRINTOUT("STA", "COLOR0")
    PRINTOUT("STA", "COLOR1")
    PRINTOUT("LDA", "#$" + sOPTIONS[0][15][0:2] + "; Sprite page")
    PRINTOUT("STA", "PMBASE")
    PRINTOUT("LDA", "#$03")
    PRINTOUT("STA", "GRACTL")
    PRINTOUT("CLI", "; Enable Interrupts")
    outFile.close()             # CLOSE #2
    print ("")
    print ("Done.")
#end def main()

# Convert ASCII string to ATASCII
def ATASCII(s):
    sOut = ""
    for c in s:
        cn = ord(c)
        if (cn in range(32, 96)):
            cn += 32
        elif (cn == 96):
            cn = 71
        elif (cn in range(97, 123)):
            cn = cn - 64
        elif (cn == 126):
            cn = 141
        sOut = sOut + chr(cn)
        
    return sOut
#end def

#         TOKEN$: J+1    J+2    J+3    J+6    Q$@J+4     COMMENT(7)
def BRANCH(MINUS, sVAR1, sSIGN, sVAR2, sDEST, sVAR2TYPE, sCOMM):
    # local defs
    def CARRYSET():
        if (not MINUS):
            PRINTOUT("BCS", sDEST + sCOMM)
        else:
            PRINTOUT(".WORD", "$0390;BCC past JMP")
            PRINTOUT("JMP", sDEST + sCOMM)
    #end local def

    def CARRYCLEAR():
        if (not MINUS):
            PRINTOUT("BCC", sDEST + sCOMM)
        else:
            PRINTOUT(".WORD", "$03B0;BCS past JMP")
            PRINTOUT("JMP", sDEST + sCOMM)
    #end local def

    def ZEROSET():
        if (not MINUS):
            PRINTOUT("BEQ", sDEST + sCOMM)
        else:
            PRINTOUT(".WORD", "$03D0;BNE past JMP")
            PRINTOUT("JMP", sDEST + sCOMM)
    #end local def

    def ZEROCLEAR():
        if (not MINUS):
            PRINTOUT("BNE", sDEST + sCOMM)
        else:
            PRINTOUT(".WORD", "$03F0;BEQ past JMP")
            PRINTOUT("JMP", sDEST + sCOMM)
    #end local def

    def NEGSET():
        if (not MINUS):
            PRINTOUT("BMI", sDEST + sCOMM)
        else:
            PRINTOUT(".WORD", "$0310;BPL past JMP")
            PRINTOUT("JMP", sDEST + sCOMM)
    #end local def

    def NEGCLEAR():
        if (not MINUS):
            PRINTOUT("BPL", sDEST + sCOMM)
        else:
            PRINTOUT(".WORD", "$0330;BMI past JMP")
            PRINTOUT("JMP", sDEST + sCOMM)
    #end local def
    # end local defs
     
    # JH - Refactor out flag true/false state (too much repeated code)
    # JH - Also refactored out GOTOs
    flagState = -1      # "no value set"
    if (sVAR2 in { "0001", "TRUE" }): flagState = 1
    elif (sVAR2 in { "0000", "FALSE" }): flagState = 0
    sFlagError = "C/Z/N flag test must be 0 or 1"

    s = sVAR1 + sSIGN
    if (s == "CFLAG="):
        if (flagState == 1): CARRYSET()
        elif (flagState == 0): CARRYCLEAR()
        else: ERROROUT(sFlagError)
    elif (s == "CFLAG<>"):
        if (flagState == 1): CARRYCLEAR()
        elif (flagState == 0): CARRYSET()
        else: ERROROUT(sFlagError)
    elif (s == "ZFLAG="):
        if (flagState == 1): ZEROSET()
        elif (flagState == 0): ZEROCLEAR()
        else: ERROROUT(sFlagError)
    elif (s == "ZFLAG<>"):
        if (flagState == 1): ZEROCLEAR()
        elif (flagState == 0): ZEROSET()
        else: ERROROUT(sFlagError)
    elif (s == "NFLAG="):
        if (flagState == 1): NEGSET()
        elif (flagState == 0): NEGCLEAR()
        else: ERROROUT(sFlagError)
    elif (s == "NFLAG<>"):
        if (flagState == 1): NEGCLEAR()
        elif (flagState == 0): NEGSET()
        else: ERROROUT(sFlagError)
    else:
        T = "AXY".find(sVAR1)
        if (T == -1): ERROROUT("A/X/Y expected")
        sT = ["CMP", "CPX", "CPY"][T]
        if (sVAR2TYPE == 'N'):
            PRINTOUT(sT, "#$" + BYTE(sVAR2) + sCOMM)
            sCOMM = ""
        else:
            if (sVAR2TYPE != 'V'): ERROROUT("Type mismatch")
            if (sVAR2 in "AXY"):
                ERROROUT("Illegal variable A/X/Y")
            else:
                PRINTOUT(sT, sVAR2 + sCOMM)
                sCOMM = ""

        # SELECT CASE SIGN$
        if (sSIGN == "="):          # BEQ LABEL
            ZEROSET()
        elif (sSIGN == "<>"):       # BNE LABEL
            ZEROCLEAR()
        elif (sSIGN == "<"):        # BCC LABEL
            CARRYCLEAR()
        elif (sSIGN == ">"):        # .WORD $0290,BNE LABEL
            if (not MINUS):
                PRINTOUT(".WORD", "$0290;BCC past BNE")
                PRINTOUT("BNE", sDEST)
            else:
                PRINTOUT(".WORD", "$0590;BCC past JMP")
                PRINTOUT(".WORD", "$03F0;BEQ past JMP")
                PRINTOUT("JMP", sDEST)
        elif (sSIGN == "<="):       # BCC LABEL,BEQ LABEL
            if (not MINUS):
                PRINTOUT("BCC", sDEST)
                PRINTOUT("BEQ", sDEST)
            else:
                PRINTOUT(".WORD", "$03B0;BCS past JMP")
                PRINTOUT("JMP", sDEST)
                PRINTOUT(".WORD", "$03D0;BNE past JMP")
                PRINTOUT("JMP", sDEST)
        elif (sSIGN == ">="):       # BCS LABEL
            CARRYSET()
        elif (sSIGN == "<<"):       # BMI LABEL
            NEGSET()
        elif (sSIGN == ">>"):       # BPL LABEL
            NEGCLEAR()
        else:
            ERROROUT("Invalid compare")
#end def

def BYTE(s):
    a = DEC(s)
    if (a > 255): ERROROUT("Number too large")
    return HEX2(a)
#end def

def CMD_ATTRACT():
    PRINTOUT("LDA", "#$00" + COMMENT(1))
    PRINTOUT("STA", "ATRACT")
    J = J + 1
#end def

def CMD_AUTHOR():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] == "S"):
        sCOPYRIGHT = tokenArray[J+1]
        # was bug here - TITLE was being tested instead of copyright
        if (len(sCOPYRIGHT) > 20): ERROROUT("AUTHOR too long (20 chars max)")
    else:
        ERROROUT("Type mismatch")
    J = J + 2
#end def

def CMD_CASE(MINUS):
    global J, tokenTypes, tokenArray
    token = tokenArray[J+1]
    if (token in "AXY"):
        ERROROUT("Illegal variable A/X/Y")

    if (len(SELSTACK) < 1): ERROROUT("CASE outside SELECT CASE")
    #if (SELSTACK(SELDEPTH - 1, 2) > SELSTACK(SELDEPTH - 1, 0)):
    if (SELSTACK[2][-1] > SELSTACK[0][-1]):
        PRINTOUT("JMP", "SC" + str(SELSTACK[0][-1]).lstrip())
        sT2 = str(SELSTACK[2][-1]).lstrip()          # SELSTACK(SELDEPTH - 1, 2)
        PRINTOUT("SC" + sT2 + ":", "~")

    if (tokenTypes[J+1] == 'N'):
        token = "#$" + BYTE(token)

    SELCOUNT += 1
    SELSTACK[2][-1] = SELCOUNT                # SELSTACK(SELDEPTH - 1, 2)

    if (token != "ELSE"):
        index = SELSTACK[1][-1]               # SELSTACK(SELDEPTH - 1, 1)
        sT2 = {"CMP", "CPX", "CPY"}[index]
        PRINTOUT(sT2, token + COMMENT(2))
        token = str(SELCOUNT).lstrip()

        if (not MINUS):
            PRINTOUT("BNE", "SC" + token)
        else:
            PRINTOUT(".WORD", "$03F0;BEQ past JMP")
            PRINTOUT("JMP", "SC" + token)

    J += 2
#end def

def CMD_CHARSET():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'N'): ERROROUT("Type mismatch")
    PRINTOUT("LDA", "#$" + tokenArray[J + 1][0:2] + COMMENT(2))
    PRINTOUT("STA", "CHBASE")
    # TODO - reset charset option so ATASCII.INC not included?
    sOPTIONS[1][16] = tokenArray[J + 1][0:2] + "00"
    J = J + 2
#end def

def CMD_CLS():
    includeArray[0] = True
    PRINTOUT("JSR", "CLS" + COMMENT(1))
    J = J + 1
#end def

def CMD_DATA():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+2:J+18] != "N,N,N,N,N,N,N,N"): ERROROUT("Invalid data")
    token = ""
    for i in range(2, 4, 6, 8, 10, 12, 14, 16):
        #T$ = T$ + CHR$(DEC(BYTE$(TOKEN$(T))))
        token += DEC(BYTE(tokenArray[i]))

    if (tokenArray[J + 1] == "CHAR"):
        sCHAR = sCHAR + token
    elif (tokenArray[J + 1] == "SPRITE"):
        sSPRITE = sSPRITE + token
    else:
        ERROROUT("Unknown statement in data")
#end def

def CMD_DEFINE():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1:J+4] != "V,N"): ERROROUT("Type mismatch")
    if (tokenArray[J+1] in "AXY"):
        ERROROUT("Invalid variable")
    else:
        if (tokenArray[J+3][0:2] == "00"):
            token = tokenArray[J+3][2:4]
        else:
            token = tokenArray[J+3]
        PRINTOUT(tokenArray[J+1].ljust(8) + ".EQU    $" + token, "~")

    J += 4
#end def

def CMD_DIV16():
    includeArray[17] = True                 # MID$(INCLUDE$, 18, 1) = "*"
    PRINTOUT("JSR", "DIV16" + COMMENT(1))
    J += 1
#end def

def CMD_DIV8():
    includeArray[6] = True                 # MID$(INCLUDE$, 7, 1) = "*"
    PRINTOUT("JSR", "DIV8" + COMMENT(1))
    J += 1
#end def

def CMD_DLIST():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'N'): ERROROUT("Type mismatch")
    #'PRINTOUT("SEI", COMMENT(2))
    #'PRINTOUT("LDX", "SDMCTL" + COMMENT(2))
    PRINTOUT("LDA", "#$" + tokenArray[J+1][0:2] + COMMENT(2))
    PRINTOUT("STA", "SDLSTH")
    PRINTOUT("LDA", "#$" + tokenArray[J+1][2:4])
    PRINTOUT("STA", "SDLSTL")
    #'PRINTOUT("STX", "SDMCTL")
    #'PRINTOUT("CLI", "")
    J = J + 2
#end def

def CMD_DO(MINUS):
    global J, tokenTypes, tokenArray, DOCOUNT, DOSTACK
    DOCOUNT += 1
    #DOSTACK[DODEPTH] = DOCOUNT; DODEPTH = DODEPTH + 1
    DOSTACK.append(DOCOUNT)
    sT = str(DOCOUNT).lstrip()
    nextToken = ""
    if (J < len(tokenArray) - 1):
        nextToken = tokenArray[J+1]
    if (nextToken == "WHILE"):
        PRINTOUT("DO" + sT + ":", "~")
        BRANCH(MINUS, tokenArray[J+2], INVERTSIGN(tokenArray[J+3]), tokenArray[J+4], "ED" + sT, tokenTypes[J+5], COMMENT(5))
        J = J + 4
    elif (nextToken == "UNTIL"):
        PRINTOUT("DO" + sT + ":", "~")
        BRANCH(MINUS, tokenArray[J+2], tokenArray[J+3], tokenArray[J+4], "ED" + sT, tokenTypes[J+5], COMMENT(5))
        J = J + 4
    else:
        PRINTOUT("DO" + sT + ":", "~" + COMMENT(1))

    J += 1
#end def

def CMD_DOWN():
    includeArray[7] = True          # MID$(INCLUDE$, 8, 1) = "*" 
    PRINTOUT("JSR", "DOWN" + COMMENT(1))
    J = J + 1
#end def

def CMD_ELSE():
    global J, IFSTACK, IFCOUNT
    #if (IFDEPTH < 1): ERROROUT("ELSE without IF")
    #sT = IFSTACK[IFDEPTH - 1].tostring().lstrip()
    #IFCOUNT += 1
    #IFSTACK[IFDEPTH - 1] = IFCOUNT
    if (len(IFSTACK) < 1): ERROROUT("ELSE without IF")
    sT = str(IFSTACK[-1]).lstrip()      # last element in list/stack
    IFCOUNT += 1
    IFSTACK[-1] = IFCOUNT
    PRINTOUT("JMP", "IF" + str(IFCOUNT).lstrip() + COMMENT(1))
    PRINTOUT("IF" + sT + ":", "~")
    J = J + 1
#end def

def CMD_END():
    global J, tokenTypes, tokenArray, IFSTACK, SELSTACK
    nextToken = tokenArray[J+1]
    if (nextToken == "IF"):
        if (len(IFSTACK) < 1): ERROROUT("END IF without IF")
        #IFDEPTH = IFDEPTH - 1
        #PRINTOUT("IF" + IFSTACK[IFDEPTH].tostring().lstrip() + ":", "~" + COMMENT(2))
        n = IFSTACK[-1]
        IFSTACK.pop()
        PRINTOUT("IF" + str(n).lstrip() + ":", "~" + COMMENT(2))
        J = J + 2
    elif (nextToken == "SELECT"):
        if (len(SELSTACK) < 1): ERROROUT ("END SELECT without SELECT CASE")
        #SELDEPTH = SELDEPTH - 1
        #if (SELSTACK(SELDEPTH, 2) > SELSTACK(SELDEPTH, 0)):
        #    sT2 = SELSTACK(SELDEPTH, 2).tostring().lstrip()
        #    PRINTOUT("SC" + sT2 + ":", "~")
        #sT2 = SELSTACK(SELDEPTH, 0).tostring().lstrip()
        n2 = SELSTACK[2].pop()
        n0 = SELSTACK[0].pop()
        if (n2 > n0):
            sT2 = str(n2).lstrip()
            PRINTOUT("SC" + sT2 + ":", "~")
        sT2 = str(n0).lstrip()
        PRINTOUT("SC" + sT2 + ":", "~" + COMMENT(2))
        J = J + 2
    elif (nextToken == "SUB"):
        PRINTOUT("RTI", COMMENT(2))
        J = J + 2
    else:
        ERROROUT("Invalid END type")
#end def

def CMD_EXIT():
    global J, tokenArray
    nextToken = tokenArray[J+1]
    if (nextToken == "DO"):
        if (len(DOSTACK) < 1): ERROROUT("EXIT DO outside DO..LOOP")
        PRINTOUT("JMP", "ED" + str(DOSTACK[-1]).lstrip() + COMMENT(2))
    if (nextToken == "FOR"):
        if (len(FORSTACK) < 1): ERROROUT("EXIT FOR outside FOR..NEXT")
        PRINTOUT("JMP", "EF" + str(FORSTACK[-1]).lstrip() + COMMENT(2))
    if (nextToken == "SUB"):
        PRINTOUT("RTI", COMMENT(2))
    if (nextToken == "SELECT"):
        if (len(SELSTACK) < 1): ERROROUT ("EXIT SELECT outside SELECT CASE")
        PRINTOUT("JMP", "SC" + str(SELSTACK[0][-1]).lstrip() + COMMENT(2))
    else:
        ERROROUT("Syntax error")

    J = J + 2
#end def

def CMD_FOR(MINUS):
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'V'): ERROROUT("Type mismatch")
    if (tokenArray[J+2] != "TO"): ERROROUT("Syntax Error")
    if (tokenArray[J+1] in "AXY"): ERROROUT("Illegal variable A/X/Y")
    FORCOUNT += 1
    FORSTACK.append(FORCOUNT)
    sT = str(FORCOUNT).lstrip()
    PRINTOUT("FR" + sT + ":", "~" + COMMENT(4))
    J = J + 3
    LDX("A")
    PRINTOUT("CMP", tokenArray[J+2])
    if (not MINUS):
        PRINTOUT("BCC", "EF" + sT)
    else:
        PRINTOUT(".WORD", "$03B0;BCS past JMP")
        PRINTOUT("JMP", "EF" + sT)
    J = J + 1
#end def

def CMD_GOSUB():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'V'): ERROROUT("Missing GOSUB label")
    PRINTOUT("JSR", tokenArray[J+1] + COMMENT(2))
    J = J + 2
#end def

def CMD_GOTO():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'V'): ERROROUT("Missing GOTO label")
    PRINTOUT("JMP", tokenArray[J+1] + COMMENT(2))
    J = J + 2
#end def

def CMD_IF(MINUS):
#'----------------------------------------------------------------------------
#'IF A=1 THEN                                    IF A<>1 THEN GOTO J1
#'  ...                                          ...
#'END IF                                         J1:
#'----------------------------------------------------------------------------
#'IF A=1 AND B=1 THEN                            IF A<>1 THEN GOTO J1
#'  ...                                          IF B<>1 THEN GOTO J1
#'END IF                                         ...
#'                                               J1:
#'----------------------------------------------------------------------------
#'IF A=1 OR B=1 THEN                             IF A=1 THEN GOTO J1
#'  ...                                          IF B<>1 THEN GOTO J2
#'END IF                                         J1:
#'                                               ...
#'                                               J2:
#'----------------------------------------------------------------------------
#'IF A=1 AND B=1 AND C=1 THEN                    IF A<>1 THEN GOTO J1
#'  ...                                          IF B<>1 THEN GOTO J1
#'END IF                                         IF C<>1 THEN GOTO J1
#'                                               ...
#'                                               J1:
#'----------------------------------------------------------------------------
#'IF A=1 AND B=1 OR C=1 THEN                     IF A<>1 THEN GOTO J2
#'                                               J1:
#'  ...                                          IF B=1 THEN GOTO J3
#'END IF                                         J2:
#'                                               IF C<>1 THEN GOTO J4
#'                                               J3:
#'                                               ...
#'                                               J4:
#'----------------------------------------------------------------------------
#'IF A=1 OR B=1 AND C=1 THEN                     IF A=1 THEN GOTO J1
#'  ...                                          IF B<>1 THEN GOTO J2
#'END IF                                         J1:
#'                                               IF C<>1 THEN GOTO J2
#'                                               ...
#'                                               J2:
#'----------------------------------------------------------------------------
#'IF A=1 OR B=1 OR C=1 THEN                      IF A=1 THEN GOTO J1
#'  ...                                          IF B=1 THEN GOTO J1
#'END IF                                         IF C<>1 THEN GOTO J2
#'                                               J1:
#'                                               ...
#'                                               J2:
#'----------------------------------------------------------------------------
#'IF A=1 OR B=1 AND C=1 OR D=1 AND E=1 THEN      IF A=1 THEN GOTO J1
#'                                               IF B<>1 THEN GOTO J2
#'                                               J1:
#'                                               IF C=1 THEN GOTO J3
#'                                               J2:
#'                                               IF D<>1 THEN GOTO J4
#'                                               J3:
#'                                               IF E<>1 THEN GOTO J4
#'                                               ...
#'                                               J4:
#'----------------------------------------------------------------------------
#'block: { test } { AND | OR | THEN }
#'rules:
#'if AND then invert sign, jump past next OR, to ELSE, or to END IF
#'if OR then same sign, jump past next AND or to THEN
#'if THEN then invert sign, jump to ELSE or to END IF
#'----------------------------------------------------------------------------
#'0  1 2 3 4    5 6 7 8
#'IF A = 0 THEN :
#'IF A = 0 AND  Y = 1 THEN
#'----------------------------------------------------------------------------
    global J, tokenTypes, tokenArray
    global IFSTACK, IFCOUNT, DOSTACK, FORSTACK, SELSTACK
    T2 = 4 + J
    while (T2 < len(tokenTypes) - 1):
        if (tokenTypes[T2] == "E"): ERROROUT("IF without THEN")
        if (tokenArray[T2] == "THEN"): break
        T2 = T2 + 1
    # wend
    
    if (T2 < len(tokenTypes) - 1 and tokenTypes[T2+1] in "E:"):  # multiline IF...END IF
        IFCOUNT = IFCOUNT + 1
        ##IFSTACK[IFDEPTH] = IFCOUNT
        ##IFDEPTH = IFDEPTH + 1
        IFSTACK.append(IFCOUNT)
        T = J + 1
        P = -1
        # TODO - Better loop check here (T < len(tokenTypes) - 2)
        while (True):
            if (tokenTypes[T:T+2] != "VO"): ERROROUT("Syntax error")
            P = P + 1
            if (P > 0): PRINTOUT("IF" + str(IFCOUNT) + "P" + str(P) + ":", "~")
            token = tokenArray[T+3]
            if (token == "AND"):
                T2 = T + 7
                P2 = P + 1
                while(T2 < len(tokenArray)):
                    token = tokenArray[T2]
                    if (token == "AND" or token == "OR"):
                        P2 = P2 + 1
                        break
                    elif (token == "THEN"):
                        P2 = 0
                        break
                    else:
                        ERROROUT("Syntax error")
                    T2 = T2 + 4
                    P2 = P2 + 1
                # wend
                if (P2 == 0):
                    BRANCH(MINUS, tokenArray[T], INVERTSIGN(tokenArray[T+1]), tokenArray[T+2], "IF" + str(IFCOUNT), tokenTypes[T+2], COMMENT(4))
                else:
                    BRANCH(FALSE, tokenArray[T], INVERTSIGN(tokenArray[T+1]), tokenArray[T+2], "IF" + str(IFCOUNT) + "P" + str(P2), tokenTypes[T+2], COMMENT(4))
            elif (token == "OR"):
                T2 = T + 7
                P2 = P + 1
                while(T2 < len(tokenArray)):
                    token = tokenArray[T2]
                    if (token == "OR" or token == "AND" or token == "THEN"):
                        P2 = P2 + 1
                        break
                    else:
                        ERROROUT("Syntax error")
                    T2 = T2 + 4
                    P2 = P2 + 1
                # wend
                BRANCH(False, tokenArray[T], tokenArray[T+1], tokenArray[T+2], "IF" + str(IFCOUNT) + "P" + str(P2), tokenTypes[T+2], COMMENT(4))
            elif (token == "THEN"):
                BRANCH(MINUS, tokenArray[T], INVERTSIGN(tokenArray[T+1]), tokenArray[T+2], "IF" + str(IFCOUNT), tokenTypes[T+2], COMMENT(5))
                P = P + 1
                PRINTOUT("IF" + str(IFCOUNT) + "P" + str(P) + ":", "~")
                J = J + 4
                break           #EXIT DO
            else:
                ERROROUT("Syntax error")
            #END SELECT
            T = T + 4
            J = J + 4
        # wend
        J = J + 1
    else:       #'single line IF...GOTO or IF..EXIT
        if (tokenTypes[J+1:J+3] != "VO"): ERROROUT("Syntax error")
        if (tokenArray[J+5] == "GOTO"):
            if (tokenTypes[J+6] != "V"): ERROROUT("Missing label")
        elif (tokenArray[J+5] == "EXIT"):
            #SELECT CASE TOKEN$(J + 6)
            token = tokenArray[J+6]
            if (token == "DO"):
                if (len(DOSTACK) < 1): ERROROUT("EXIT DO outside DO..LOOP")
                tokenArray[J+6] = "ED" + str(DOSTACK[-1])
            elif (token == "FOR"):
                if (len(FORSTACK) < 1): ERROROUT("EXIT FOR outside FOR..NEXT")
                tokenArray[J+6] = "EF" + str(FORSTACK[-1])
            elif (token == "SELECT"):
                if (len(SELSTACK) < 1): ERROROUT ("EXIT SELECT outside SELECT CASE")
                tokenArray[J+6] = "SC" + str(SELSTACK[0][-1])
            else:
                ERROROUT("Syntax error")
        else:
            ERROROUT("Syntax error")

        BRANCH(MINUS, tokenArray[J+1], tokenArray[J+2], tokenArray[J+3], tokenArray[J+6], tokenTypes[J+3], COMMENT(7))
        J = J + 7
#end def

def CMD_INPUT():
    global J, tokenTypes
    if (tokenTypes[J+1] != 'V'): ERROROUT("Type mismatch")
    includeArray[4] = True           # MID$(INCLUDE$, 5, 1) = "*"
    PRINTOUT("JSR", "INPUT" + COMMENT(2))
    J = J + 1
    STX("A")
    J = J + 1
#end def

def CMD_INTERNAL():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'N'): ERROROUT("Type mismatch")
    T = DEC(tokenArray[J+1])
    if (T < 1 or T > INCLUDE): ERROROUT("Out of range")
    includeArray[T-1] = True         # MID$(INCLUDE$, T, 1) = "*"
    J = J + 2
#end def

def CMD_KEYPAD():
    global J, tokenTypes, tokenArray
    if (tokenArray[J+1] == "FIX"):
        PRINTOUT("LDA", "KEYDB;check key debounce counter")
        PRINTOUT(".WORD", "$02F0;if zero, skip")
        PRINTOUT("DEC", "KEYDB;otherwise decrease by one")
    else:
        if (tokenTypes[J+1] != 'N'): ERROROUT("Type mismatch")
        T = DEC(tokenArray[J+1])
        if (T < 0 or T > 3): ERROROUT("Invalid keypad")
        PRINTOUT("LDA", "#$" + HEX2(4 + T) + COMMENT(2))
        PRINTOUT("STA", "CONSOL")
    J = J + 2
#end def

def CMD_LEFT():
    includeArray[3] = True           # MID$(INCLUDE$, 4, 1) = "*"
    PRINTOUT("JSR", "LEFT" + COMMENT(1))
    J = J + 1
#end def

def CMD_LOCATE():
    global J, tokenTypes, tokenArray
    tt = tokenTypes[J+1:J+4]                #MID$(Q$, J + 2, 3)
    if (tt == "N,N"):
        sT = HEX4(40 * DEC(tokenArray[J+1]) + DEC(tokenArray[J+3]) + DEC(sOPTIONS[0][14]))
        PRINTOUT("LDA", "#$" + sT[0:2] + COMMENT(4))
        PRINTOUT("STA", "LOCATEH")
        PRINTOUT("LDA", "#$" + sT[2:4])
        J = J + 4
    elif (tt in { "V,N", "N,V", "V,V" }):
        J = J + 1; LDX("Y"); J = J - 1
        J = J + 3; LDX("A"); J = J - 3
        PRINTOUT("STA", "TEMPL" + COMMENT(4))
        J = J + 4
        PRINTOUT("LDA", "#$28")
        PRINTOUT("LDX", "#$" + sOPTIONS[0][14][0:2])
        includeArray[5] = True               #MID$(INCLUDE$, 6, 1) = "*"
        PRINTOUT("JSR", "MULADD")       # MULADD: (X:A)=A*Y+(X:TEMPL) if Y>0
        PRINTOUT("STX", "LOCATEH")
    else:
        ERROROUT("Type mismatch")

    PRINTOUT("STA", "LOCATEL")
#end def

def CMD_LOOP(MINUS):
    global J, tokenArray, DOSTACK
    if (len(DOSTACK) < 1): ERROROUT("LOOP without DO")
    #DODEPTH = DODEPTH - 1
    #sT = DOSTACK[DODEPTH].tostring().lstrip()
    sT = str(DOSTACK.pop()).lstrip()
    nextToken = ""
    if (J < len(tokenArray) - 1):
        nextToken = tokenArray[J+1] 
    if (nextToken == "WHILE"):
        BRANCH(MINUS, tokenArray[J+2], tokenArray[J+3], tokenArray[J+4], "DO" + sT, tokenTypes[J+5], COMMENT(5))
        J = J + 4
    elif (nextToken == "UNTIL"):
        BRANCH(MINUS, tokenArray[J+2], INVERTSIGN(tokenArray[J+3]), tokenArray[J+4], "DO" + sT, tokenTypes[J+5], COMMENT(5))
        J = J + 4
    else:
        PRINTOUT("JMP", "DO" + sT + COMMENT(1))

    PRINTOUT("ED" + sT + ":", "~")
    J = J + 1
#end def

def CMD_MEMAREA():
    global J, tokenTypes, tokenArray
    #IF MID$(Q$, J + 2, 9) <> "N,N,N,N,N" THEN CALL ERROROUT("Illegal function call")
    if (tokenTypes[J+1:J+10] != "N,N,N,N,N"): ERROROUT("Illegal function call")
    includeArray[15] = True            # MID$(INCLUDE$, 16, 1) = "*"
    tokenArray[J + 5] = HEX4(DEC(tokenArray[J + 5]) - 1)
    PRINTOUT("LDA", "#$" + tokenArray[J+1][0:2] + COMMENT(10))
    PRINTOUT("STA", "FROMH")
    PRINTOUT("LDA", "#$" + tokenArray[J+1][2:4])
    PRINTOUT("STA", "FROML")
    PRINTOUT("LDA", "#$" + tokenArray[J+3][0:2])
    PRINTOUT("STA", "TOH")
    PRINTOUT("LDA", "#$" + tokenArray[J+3][2:4])
    PRINTOUT("STA", "TOL")
    PRINTOUT("LDA", "#$" + tokenArray[J+5][0:2])
    PRINTOUT("STA", "COUNTH")
    PRINTOUT("LDA", "#$" + tokenArray[J+5][2:4])
    PRINTOUT("STA", "COUNTL")
    PRINTOUT("LDA", "#$" + tokenArray[J+7][2:4])
    PRINTOUT("STA", "COPYLEN")
    PRINTOUT("LDA", "#$" + tokenArray[J+9][2:4])
    PRINTOUT("STA", "SKIP")
    PRINTOUT("JSR", "MEMAREA")
    J = J + 10
#end def

def CMD_MEMCOPY():
    global J, tokenTypes, tokenArray, includeArray
    if (tokenTypes[J+1:J+6] != "N,N,N"): ERROROUT("Illegal function call")
    includeArray[14] = True          # MID$(INCLUDE$, 15, 1) = "*"
    tokenArray[J + 5] = HEX4(DEC(tokenArray[J + 5]) - 1)
    PRINTOUT("LDA", "#$" + tokenArray[J+1][0:2] + COMMENT(6))
    PRINTOUT("STA", "FROMH")
    PRINTOUT("LDA", "#$" + tokenArray[J+1][2:4])
    PRINTOUT("STA", "FROML")
    PRINTOUT("LDA", "#$" + tokenArray[J+3][0:2])
    PRINTOUT("STA", "TOH")
    PRINTOUT("LDA", "#$" + tokenArray[J+3][2:4])
    PRINTOUT("STA", "TOL")
    PRINTOUT("LDA", "#$" + tokenArray[J+5][0:2])
    PRINTOUT("STA", "COUNTH")
    PRINTOUT("LDA", "#$" + tokenArray[J+5][2:4])
    PRINTOUT("STA", "COUNTL")
    PRINTOUT("JSR", "MEMCOPY")
    J = J + 6
#end def

def CMD_MISSILES():
    global J, tokenArray
    if (tokenArray[J+1] == "ON"):
        PRINTOUT("LDA", "#$03" + COMMENT(2))
    elif (tokenArray[J+1] == "OFF"):
        PRINTOUT("LDA", "#$02" + COMMENT(2))
    else:
        ERROROUT("Syntax error")

    PRINTOUT("STA", "GRACTL")
    J = J + 2
#end def

def CMD_MOVEUP():
    includeArray[10] = True              # MID$(INCLUDE$, 11, 1) = "*"
    PRINTOUT("JSR", "MOVEUP")
    J = J + 1
#end sub

def CMD_MUL8():
    includeArray[9] = True               # MID$(INCLUDE$, 10, 1) = "*"
    PRINTOUT("JSR", "MUL8" + COMMENT(1))
    J = J + 1
#end sub

def CMD_MULADD():
  includeArray[5] = True                 # MID$(INCLUDE$, 6, 1) = "*"
  PRINTOUT("JSR", "MULADD" + COMMENT(1))
  J = J + 1
#end def

def CMD_NEXT():
    global J, tokenTypes, tokenArray
    if (len(FORSTACK) < 1): ERROROUT("NEXT without FOR")
    if (tokenTypes[J+1] != 'V'): ERROROUT("Missing variable")
    if (tokenArray[J+1] in "AXY"):                  # CASE "A", "X", "Y"
        ERROROUT("Illegal variable A/X/Y")
    PRINTOUT("INC", tokenArray[J+1] + COMMENT(2))
    #FORDEPTH = FORDEPTH - 1
    n = FORSTACK.pop()
    PRINTOUT("JMP", "FR" + str(n).lstrip())
    PRINTOUT("EF" + str(n).lstrip() + ":", "~")
    J = J + 2
#end def

def CMD_PALETTE():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'N'): ERROROUT("Illegal function call")
    T = DEC(tokenArray[J+1])
    if (T < 0 or T > 8): ERROROUT("Illegal function call")
    colRegs = [ "COLOR4", "COLOR0", "COLOR1", "COLOR2", "COLOR3", "PCOLR0", "PCOLR1", "PCOLR2", "PCOLR3" ]
    sT = colRegs[T]
    J = J + 3; LDX("A"); J = J - 3
    PRINTOUT("STA", sT + COMMENT(4))
    J = J + 4
#end def

def CMD_POKE():
    #'0    1  2345
    #'POKE NV,AXY
    #'POKE NV+X,A
    #'POKE NV+Y,A
    #'POKE NV*Y,A
    global J, tokenArray
    sT = ""
    argType = tokenTypes[J+1]
    if (argType == 'N'):
        if (tokenArray[J+2] == "*"):
            sT = "$" + BYTE(tokenArray[J+1])
        else:
            sT = "$" + tokenArray[J+1]
    elif (argType == 'V'):
        if (tokenArray[J+1] in "AXY"):
            ERROROUT("Illegal variable A/X/Y")
        else: 
            sT = tokenArray[J+1]
    else:
        ERROROUT("Type mismatch")

    nextArgs = tokenArray[J+2] + tokenArray[J+3]
    if (nextArgs == "+X"):
        if (tokenArray[J+4] + tokenArray[J+5] != ",A"): ERROROUT("Type mismatch")
        PRINTOUT("STA", sT + ",X" + COMMENT(6))
        J = J + 6
    elif (nextArgs == "+Y"):
        if (tokenArray[J+4] + tokenArray[J+5] != ",A"): ERROROUT("Type mismatch")
        PRINTOUT("STA", sT + ",Y" + COMMENT(6))
        J = J + 6
    elif (nextArgs == "*Y"):
        if (tokenArray[J+4] + tokenArray[J+5] != ",A"): ERROROUT("Type mismatch")
        PRINTOUT("STA", "(" + sT + "),Y" + COMMENT(6))
        J = J + 6
    elif (nextArgs in ",A,X,Y"):
        PRINTOUT("ST" + tokenArray[J+3], sT + COMMENT(4))
        J = J + 4
    else:
        ERROROUT("Type mismatch")
#end def

def CMD_POP():
    global J, tokenArray
    nextToken = tokenArray[J+1]
    if (nextToken == "ALL"):
        PRINTOUT("PLA", COMMENT(2))
        PRINTOUT("TAY", "")
        PRINTOUT("PLA", "")
        PRINTOUT("TAX", "")
        PRINTOUT("PLA", "")
    elif (nextToken == "A"):
        PRINTOUT("PLA", COMMENT(2))
    else:
        ERROROUT("Syntax error")

    J = J + 2
#end def

def CMD_POS():
    includeArray[13] = True          # MID$(INCLUDE$, 14, 1) = "*"
    PRINTOUT("JSR", "POS" + COMMENT(1))
    J = J + 1
#end def

def CMD_PRINT():
    global J, tokenTypes, tokenArray, sST
    nextToken = tokenArray[J+1]
    argType = tokenTypes[J+1]
    if (argType == "S"):          #PRINT ""
        includeArray[1] = True           # MID$(INCLUDE$, 2, 1) = "*"
        FIXSTR(nextToken)
        nextToken = nextToken + chr(255)
        T = sST.find(nextToken)
        if (T == -1):
            T = len(sST)
            sST = sST + nextToken
        sT = HEX4(T + DEC(sOPTIONS[0][17]))         # strings base address
        PRINTOUT("LDA", "#$" + sT[2:4] + COMMENT(2))
        PRINTOUT("STA", "TEMPL")
        PRINTOUT("LDA", "#$" + sT[0:2])
        PRINTOUT("STA", "TEMPH")
        PRINTOUT("JSR", "PRINT")
        J = J + 2
    elif (argType == "F"):        # 'PRINT CHR$()
        J = J + 2; LDX("A")
        if (nextToken != ")"): ERROROUT("Mismatched parenthesis")
        J = J - 2
        PRINTOUT("LDY", "#$00" + COMMENT(4))
        PRINTOUT("STA", "(LOCATEL),Y")
        J = J + 4
    else:   # 'PRINT
        includeArray[12] = True          # MID$(INCLUDE$, 13, 1) = "*"
        PRINTOUT("JSR", "CRLF" + COMMENT(1))
        J = J + 1
#end def

def CMD_PUSH():
    global J, tokenArray
    nextToken = tokenArray[J+1]
    if (nextToken == "ALL"):
        PRINTOUT("PHA", COMMENT(2))
        PRINTOUT("TXA", "")
        PRINTOUT("PHA", "")
        PRINTOUT("TYA", "")
        PRINTOUT("PHA", "")
    elif (nextToken == "A"):
        PRINTOUT("PHA", COMMENT(2))
    else:
        ERROROUT("Syntax error")

    J = J + 2
#end def

def CMD_PUT():
    #'PUT ( 2 , 4 ) , 7 , 9 , 11
    global J, tokenTypes, tokenArray
    syntaxCheck = tokenArray[J+1] + tokenArray[J+3] + tokenArray[J+5] + tokenArray[J+6] + tokenArray[J+8] + tokenTypes[J+10]
    if (syntaxCheck != "(,),,N"): ERROROUT("Syntax error")
    FROMH = 0
    if (tokenTypes[J+10:J+12] == ",V"):
        if (tokenArray[J+11] == "FROMH"):
            tokenTypes[J+11] = "N"
            FROMH = 1
        else:
            ERROROUT("Variable must be FROMH")

    if (tokenTypes[J+10:J+13] == ",OV"):
        if (tokenArray[J+12] == "FROMH"):
            tokenTypes[J+12] = "N"
            FROMH = 1
        else:
            ERROROUT("Variable must be FROMH")

    # height specified?
    T3 = 0
    if (tokenTypes[J+10:J+12] == ",N"):         #'height specified?
        T3 = 2
    elif (tokenTypes[J+10:J+13] == ",ON"):      #'negative height (invert)
        T3 = 3

    T = DEC(tokenArray[J+9])
    if (T > 7): ERROROUT("Out of range")
    sT = str(chr(T % 4 + 48))
    T2 = T + 4; sT3 = "P"
    if (T > 3):
       T2 = 3; sT3 = "M"

    sT2 = HEX2(DEC(sOPTIONS[0][15][0:2]) + T2)

    if (tokenTypes[J+2] == "N"):
        T1 = DEC(tokenArray[J+2][2:4]) + 0x30
        PRINTOUT("LDA", "#$" + HEX2(T1) + COMMENT(10 + T3))
    elif (tokenTypes[J+2] == "V"):
        PRINTOUT("LDA", tokenArray[J+2] + COMMENT(10 + T3))
        PRINTOUT("ADC", "#$2F")
    else:
        ERROROUT("Type mismatch")

    PRINTOUT("STA", "HPOS" + sT3 + sT)
    J = J + 4
    LDX("X")

    if (T > 3):
        PRINTOUT("STX", "FROML")
        PRINTOUT("LDX", "#$0" + chr(T + 44))

    PRINTOUT("LDA", "#$" + sT2)
    PRINTOUT("STA", "SPRITEH")
    J = J + 3
    LDX("A")
    PRINTOUT("STA", "TEMPH")

    if (T > 3):
        if (T3 == 0):
            PRINTOUT("JSR", "PUTMSL")
            includeArray[18] = True                  #MID$(INCLUDE$, 19, 1) = "*"
        else:
            if (T3 == 2):
                if (FROMH == 0):
                    PRINTOUT("LDA", "#$" + HEX2(DEC(tokenArray[J+4])))
                    PRINTOUT("STA", "FROMH")
                PRINTOUT("JSR", "PUTMSH")
                includeArray[19] = True              # MID$(INCLUDE$, 20, 1) = "*"
            else:
                if (FROMH == 0):
                    PRINTOUT("LDA", "#$" + HEX2(DEC(tokenArray[J+5]) - 1))
                    PRINTOUT("STA", "FROMH")
                PRINTOUT("JSR", "PUTMSI")
                includeArray[21] = True              # MID$(INCLUDE$, 22, 1) = "*"
    else:
        if (T3 == 0):
            PRINTOUT("JSR", "PUTSPR")
            includeArray[16] = True                  # MID$(INCLUDE$, 17, 1) = "*"
        else:
            if (T3 == 2):
                if (FROMH == 0):
                    PRINTOUT("LDA", "#$" + HEX2(DEC(tokenArray[J+4])))
                    PRINTOUT("STA", "FROMH")
                PRINTOUT("JSR", "PUTSPH")
                includeArray[20] = True                  #MID$(INCLUDE$, 21, 1) = "*"
            else:
                if (FROMH == 0):
                    PRINTOUT("LDA", "#$" + HEX2(DEC(tokenArray[J+5]) - 1))
                    PRINTOUT("STA", "FROMH")
                PRINTOUT("JSR", "PUTSPI")
                includeArray[22] = True                  # MID$(INCLUDE$, 23, 1) = "*"

    J = J + 3 + T3
#end def

def CMD_RETURN():
    global J
    PRINTOUT("RTS", COMMENT(1))
    J = J + 1
#end def

def CMD_RIGHT():
    global J, includeArray
    includeArray[2] = True                   # MID$(INCLUDE$, 3, 1) = "*"
    PRINTOUT("JSR", "RIGHT" + COMMENT(1))
    J = J + 1
#end def

# what is this?
def CMD_RIGHTBRACE():
    global includeArray
    includeArray[2] = True                   # MID$(INCLUDE$, 3, 1) = "*"
    PRINTOUT("JSR", "RIGHT" + COMMENT(1))
    includeArray[11] = True                   # MID$(INCLUDE$, 12, 1) = "*"
    PRINTOUT("JSR", "CHKROW")
    J = J + 1
#end def

def CMD_SCREEN():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'N'): ERROROUT("Illegal function call")
    SCRMODE = DEC(tokenArray[J+1]) & 0xF
    J = J + 2
#end def

def CMD_SELECT():
    global J, tokenArray
    if (tokenArray[J+1] != "CASE"): ERROROUT("Syntax error")
    T = "AXY".find(tokenArray[J+2])
    if (T == -1): ERROROUT("A/X/Y expected")
    SELCOUNT = SELCOUNT + 1
    SELSTACK[0].append(SELCOUNT)
    SELSTACK[2].append(SELCOUNT)
    SELSTACK[1].append(T)
    #SELDEPTH = SELDEPTH + 1
    PRINTOUT("", COMMENT(3))
    J = J + 3
#end def

def CMD_SET():
    # SET option=location [label]
    global J, tokenTypes, tokenArray
    setOpts = ["VIMIRQ", "VVBLKI", "VVBLKD", "VDSLST", "VKYBDI", "VKYBDF", "VTRIGR",
            "VBRKOP", "VSERIN", "VSEROR", "VSEROC", "VTIMR1", "VTIMR2", "VTIMR4",
            "SCREEN", "SPRITES", "CHARSET", "STRINGS", "DLIST"]
    T = setOpts.index(tokenArray[J+1])
    if (T == -1): ERROROUT("Syntax error")
    if (tokenTypes[J+2:J+4] != "ON"): ERROROUT("Syntax error")
    sOPTIONS[0][T] = tokenArray[J+3]
    if (tokenTypes[J+4] == "V"):
        sOPTIONS[1][T] = tokenArray[J+4]
        J = J + 1
    J= J + 4
#end def

def CMD_SOUND():
    #       1         2 3
    # SOUND CHANNEL(N), FREQUENCY(N/V)
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] + tokenArray[J+2] != "N,"): ERROROUT("Syntax error")
    T = DEC(tokenArray[J+1])
    if (T > 3): ERROROUT("Out of range")
    sT = str(chr(T + 49))            # 'channel
    J = J + 3; LDX("A"); J = J - 3
    PRINTOUT("STA", "AUDF" + sT + COMMENT(8))
    J = J + 4
#end def

def CMD_SPRITES():
    global J, tokenArray
    if (tokenArray[J+1] == "ON"):
        PRINTOUT("LDA", "#$3E" + COMMENT(2))
    elif (tokenArray[J+1] == "OFF"):
        PRINTOUT("LDA", "#$22" + COMMENT(2))
    else:
        ERROROUT("Syntax error")

    PRINTOUT("STA", "SDMCTL")
    J = J + 2
#end def

def CMD_SUB():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'V'): ERROROUT("Syntax error")
    # TODO - refactor this in a more Python-friendly way!
    nextToken = tokenArray[J+1]
    T = 0
    while (T < numOPTIONS):
        if (nextToken == sOPTIONS[1][T]):
            break
        T += 1
    if (T >= numOPTIONS): ERROROUT("SUB undefined")
    PRINTOUT(".ORG    $" + sOPTIONS[0][T], "~")
    PRINTOUT(";-----------------------------------------------------------", "~")
    PRINTOUT("; SUB " + nextToken, "~")
    PRINTOUT(";-----------------------------------------------------------", "~")
    PRINTOUT(nextToken + ":", "~")
    J = J + 2
#end def

def CMD_TITLE():
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] == 'S'):
        sTITLE = tokenArray[J+1]
        if (len(sTITLE) > 20): ERROROUT("TITLE too long (20 chars max)")
    elif (tokenTypes[J+1] == 'V'):
        if (tokenArray[J+1] == "OFF"): TITLE = False
    else:
        ERROROUT("Type mismatch")

    J = J + 2
#end def

def CMD_UP():
    includeArray[8] = True           # MID$(INCLUDE$, 9, 1) = "*"
    PRINTOUT("JSR", "UP" + COMMENT(1))
    J = J + 1
#end def

def CMD_VOLUME():
    # TODO : finx commenting (says "SOUND")
    #'      1         2 3         4 5
    #'SOUND CHANNEL(N), NOISE(N/V), VOLUME(N/V)
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] + tokenArray[J+2] + tokenArray[J+4] != "N,,"): ERROROUT("Syntax error")
    T = DEC(tokenArray[J+1])
    if (T > 3): ERROROUT("Out of range")
    sT = str(chr(T + 49))            # 'channel
    sT2 = ""
    argTypes = tokenTypes[J+3] + tokenTypes[J+5]
    if (argTypes == "NN"):
        T = 16 * DEC(tokenArray[J+3]) + DEC(tokenArray[J+5])
        PRINTOUT("LDA", "#$" + HEX2(T) + COMMENT(6))
    elif (argTypes == "VN" or argTypes == "VV"):
        if (tokenTypes[J+5] == "N"):
          sT2 = "#$" + HEX2(DEC(tokenArray[J+5]))
        else:
          sT2 = tokenArray[J+5]
        PRINTOUT("LDA", tokenArray[J+3] + COMMENT(6))
        if (not useDASM):
            PRINTOUT("ASL", "A")
            PRINTOUT("ASL", "A")
            PRINTOUT("ASL", "A")
            PRINTOUT("ASL", "A")
        else:
            PRINTOUT("ASL", "")
            PRINTOUT("ASL", "")
            PRINTOUT("ASL", "")
            PRINTOUT("ASL", "")
        PRINTOUT("CLC", "")
        PRINTOUT("ADC", sT2)
    elif (argTypes == "NV"):
        T = 16 * DEC(tokenArray[J+3])
        PRINTOUT("LDA", "#$" + HEX2(T) + COMMENT(6))
        if (tokenTypes[J+5] == "N"):
            sT2 = "#$" + HEX2(DEC(tokenArray[J+5]))
        else:
            sT2 = tokenArray[J+5]
        PRINTOUT("CLC", "")
        PRINTOUT("ADC", sT2)
    else:
        ERROROUT("Type mismatch")

    PRINTOUT("STA", "AUDC" + sT)
    J = J + 6
#end def

def COMMENT(N):
    global J, tokenArray
    s = ";"
    for a in range(J, J+N):         #FOR A = J TO J + N - 1
        token = tokenArray[a]
        for i in range(len(token)):
            if (token[i] < ' '): token[i] = "þ"    #MID$(B$, B, 1) = "þ"
        s = s + token + " "
    return s
#end def

def CONSCOLOR(colour):
    global useANSIDebugColours
    if (useANSIDebugColours):
        sT = "0"
        if (colour > 7):
            colour = colour - 8
            sT = "1"
        print ("[" + sT + ";3" + "04261537"[colour] + "m")
#end def

def DEC(s):
    return int(s, 16)                   # base 16 to base 10
#end def

def ERROROUT(s):
    # A$ was some kind of global
    global LINENUM, gCurrentParseLine
    print("")
    print(s + " on line " + str(LINENUM) + ":")
    print("[" + str(LINENUM).lstrip() + "] " + gCurrentParseLine)          #A$)
    exit()          # explode into little pieces
#end def

def FCN_INKEY():
    global J, tokenArray
    if (tokenArray[J] != "A"): ERROROUT("'A' expected")
    PRINTOUT("LDA", "KEY" + COMMENT(3))
    PRINTOUT("LDX", "#$FF")
    PRINTOUT("STX", "KEY")
    J = J + 3
#end def

def FCN_JOYTRIG():
    global J, tokenTypes, tokenArray
    if (not tokenArray[J] in "AXY"): ERROROUT("A/X/Y expected")
    sT2 = "LD" + tokenArray[J]

    #'PRINTOUT(T2$, "JOYTRIG" + COMMENT(3))
    #'PRINTOUT(".WORD", "$02F0")
    #'PRINTOUT("DEC", "JOYTRIG")
    #'J = J + 3

    #'IF TOKEN$(J) <> "A" THEN CALL ERROROUT("A expected")
    #'PRINTOUT("LDA", "SKSTAT" + COMMENT(3))
    #'PRINTOUT("AND", "#$F7")
    #'J = J + 3

    if (tokenTypes[J+3] != 'N'): ERROROUT("Type mismatch")
    T = DEC(tokenArray[J+3])
    if (T < 0 or T > 3): ERROROUT("Illegal function call")
    PRINTOUT(sT2, "TRIG" + str(T).lstrip() + COMMENT(5))  # 'T * 2 ???
    J = J + 5
#end def

def FCN_JOYTRIG2():
    global J, tokenArray
    if (tokenArray[J] != "A"): ERROROUT("A expected")
    PRINTOUT("LDA", "SKSTAT" + COMMENT(3))
    PRINTOUT("AND", "#$08")
    J = J + 3
#end sub

def FCN_JOYX():
    global J, tokenTypes, tokenArray
    if (not tokenArray[J] in "AXY"): ERROROUT("A/X/Y expected")
    sT2 = "LD" + tokenArray[J]
    if (tokenTypes[J+3:J+5] != "N)"): ERROROUT("Type mismatch")
    T = DEC(tokenArray[J+3])
    if (T < 0 or T > 3): ERROROUT("Illegal function call")
    PRINTOUT(sT2, "PADDL" + str(T * 2).lstrip() + COMMENT(5))
    J = J + 5
#end def

def FCN_JOYY():
    global J, tokenTypes, tokenArray
    if (not tokenArray[J] in "AXY"): ERROROUT("A/X/Y expected")
    sT2 = "LD" + tokenArray[J]
    if (tokenTypes[J+3:J+5] != "N)"): ERROROUT("Type mismatch")
    T = DEC(tokenArray[J+3])
    if (T < 0 or T > 3): ERROROUT("Illegal function call")
    PRINTOUT(sT2, "PADDL" + str(T * 2 + 1).lstrip() + COMMENT(5))
    J = J + 5
#end def

def FCN_PEEK():
    #'AXY=PEEK(NV)
    #'012    3  456
    #'AY=PEEK(NV+X)
    #'AX=PEEK(NV+Y)
    #'A=PEEK(NV*Y) or A=PEEK(NV,Y)
    global J, tokenTypes, tokenArray
    sT = ""
    if (tokenTypes[J+3] == 'N'):
        if (tokenArray[J+4] in "*,"):
            sT = "$" + BYTE(tokenArray[J+3])
        else:
            sT = "$" + tokenArray[J+3]
    elif (tokenTypes[J+3] == 'V'):
        if (tokenArray[J+1] in "AXY"):
            ERROROUT("Illegal variable A/X/Y")
        else:
            sT = tokenArray[J+3]
    else:
        ERROROUT("Type mismatch")

    args = ""
    if (J + 6 < len(tokenArray)):
        args = tokenArray[J+4] + tokenArray[J+5] + tokenArray[J+6]
        
    if (args == "+X)"):
        if (tokenArray[J] != "A" and tokenArray[J] != "Y"): ERROROUT("A/Y expected")
        PRINTOUT("LD" + tokenArray[J], sT + ",X" + COMMENT(7))
        J = J + 7
    elif (args == "+Y)"):
        if (tokenArray[J] != "A" and tokenArray[J] != "X"): ERROROUT("A/X expected")
        PRINTOUT("LD" + tokenArray[J], sT + ",Y" + COMMENT(7))
        J = J + 7
    elif (args == "*Y)" or args == ",Y)"):
        if (tokenArray[J] != "A"): ERROROUT("A expected")
        PRINTOUT("LDA", "(" + sT + "),Y" + COMMENT(7))
        J = J + 7
    else:
        if (tokenArray[J+4] == ")"):
            if (not tokenArray[J] in "AXY"): ERROROUT("A/X/Y expected")
            PRINTOUT("LD" + tokenArray[J], sT + COMMENT(5))
            J = J + 5
        else:
            ERROROUT("Type mismatch")

#end def

def FCN_SCREEN():
    global J, tokenTypes, tokenArray
    if (not tokenArray[J].findOneOf("AXY")):
        ERROROUT("A/X/Y expected")
    sT2 = "LD" + tokenArray[J]
    tokenType = tokenTypes[J+3:J+7]
    if (tokenType == "N,N)"):
        sT = HEX4(40 * (DEC(tokenArray(J + 3)) - 1) + DEC(tokenArray(J + 5)) - 1 + DEC(sOPTIONS(14,0)))
        PRINTOUT("LDA", "#$" + sT[0:2] + COMMENT(7))
        PRINTOUT("STA", "SCREENH")
        PRINTOUT("LDA", "#$" + sT[2:4])
        J = J + 7
    elif (tokenType == "V,N)" or tokenType == "N,V)" or tokenType == "V,V)"):
        includeArray[5] = True
        J = J + 3
        LDX("Y")
        J = J - 3
        J = J + 5
        LDX("A")
        J = J + 2
        PRINTOUT("STA", "TEMPL" + COMMENT(7))
        PRINTOUT("LDA", "#$28")
        PRINTOUT("LDX", "#$" + sOPTIONS[0][14][0:2])        # MID$(sOPTIONS(14, 0), 1, 2))
        PRINTOUT("JSR", "MULADD")
        PRINTOUT("STX", "SCREENH")
    else:
        ERROROUT("Type mismatch")

    PRINTOUT("STA", "SCREENL")
    PRINTOUT("LDY", "#$00")
    PRINTOUT(sT2, "(SCREENL),Y")
#end def

def FINDTOKEN(s, start):
    global tokenArray
    T = start
    for token in tokenArray[start:]:
        if (tokenArray[T] == s):
            break
        T += 1

    if (T > 31): T = -1

    return (T - start)
#end def        
        

# something to do with curly braces?
def FIXSTR(s):
    sAA = ""
    openBrace = False
    for c in s:
        if (not openBrace):
            if (c == "{"):
                openBrace = True
                sBB = ""
            else:
                sAA = sAA + c
        else:
            if (c == "}"):
                openBrace = False
                sBB = TOHEX(sBB)
                sAA = sAA + chr(DEC(sBB))            # AA$ = AA$ + CHR$(DEC(BB$))
            else:
                sBB = sBB + c

    return sAA
#end def

def HEX2(decimal):
    s = hex(decimal).replace("0x", "")
    if (len(s) > 2):
        ERROROUT("Number too large")
    return s.rjust(2, '0')
#end def
 
# argument is a decimal number        
def HEX4(decimal):
    if (decimal < 0):
        decimal += 65536
    s = hex(decimal).replace("0x", "")
    if (len(s) > 4):
        ERROROUT("Number too large")
    return s.rjust(4, '0')
#end def

def INVERTSIGN(sign):
    if (sign == "="):
        return "<>"
    if (sign == "<"):
        return ">="
    if (sign == "<="):
        return ">"
    if (sign == ">"):
        return "<="
    if (sign == ">="):
        return "<"
    if (sign == "<>"):
        return "="

    ERROROUT("Cannot invert sign " + sign)
#end def         

# TODO - Document (is used with ADC/SBC/AND/ORA etc
# reg = W$
# tokenTypes = Q$
def J5(reg):
    global J, tokenTypes, tokenArray
    if (tokenArray[J] != 'A'):
        ERROROUT("Illegal operation")
         
    if (tokenTypes[J+4] == 'N'):
        PRINTOUT(reg, "#$" + BYTE(tokenArray[J + 4]) + COMMENT(5))
    else:
        if (tokenTypes[J+4] != 'V'):
            ERROROUT("Type mismatch")
        PRINTOUT(reg, tokenArray[J + 4] + COMMENT(5))

    J += 5
    return J
#end def

# reg = W$
# token = TOKEN$(J)
# tokenType = MID$(Q$, J + 1, 1)
def LDX(reg):           #, token, tokenType):
    global J, tokenTypes, tokenArray
    tokenType = tokenTypes[J];
    token = tokenArray[J]
    if (tokenType == 'N'):
        PRINTOUT("LD" + reg, "#$" + BYTE(token) + ";" + reg + " = " + token)
    else:
        if (tokenType != 'V'):
            ERROROUT("Type mismatch")
        if (token != reg):
            if (token == 'A' or token == 'X' or token == 'Y'):
                if ((reg == 'X' or reg == 'Y') and (token == 'x' or token == 'Y')):
                    ERROROUT("Illegal XY assign")
                PRINTOUT("T" + token + reg, ";" + reg + " = " + token)
            else:
                PRINTOUT("LD" + reg, token + ";" + reg + " = " + token)
#end def
            

##FUNCTION PARSE$ (S$)
##       c$ = "CLS     POKE    LOCATE  PRINT   PUT     CHAR    SPRITE  "
##  c$ = c$ + "CHARSET TITLE   RETURN  SUB     GOSUB   PUT(    GOTO    "
##  c$ = c$ + "IF      THEN    UP      DOWN    LEFT    RIGHT   DEFINE  "
##  c$ = c$ + "DO      LOOP    EXIT    KEYPAD  INPUT   FOR     TO      "
##  c$ = c$ + "NEXT    DATA    POS     MOVEUP  }       SCREEN  PALETTE "
##  c$ = c$ + "MEMCOPY MEMAREA IF_     FOR_    MULADD  PUSH    POP     "
##  c$ = c$ + "SET     SPRITES INTERNALDLIST   ATTRACT END     DIV8    "
##  c$ = c$ + "DIV16   MUL8    SELECT  CASE    CASE_   DO_     LOOP_   "
##  c$ = c$ + "ELSE    AUTHOR  SOUND   VOLUME  MISSILES"
##       F$ = "JOYX(   JOYY(   TRIG(   INKEY   CHR$(   SCREEN( PEEK(   "
##  F$ = F$ + "JOYTRIG(JOYTRIG2"
##       R$ = "LDA     LDX     LDY     STA     STX     STY     TAX     "
##  R$ = R$ + "TAY     TXA     TYA     TXS     TSX     JMP     JSR     "
##  R$ = R$ + "RTS     CMP     CPX     CPY     BNE     BEQ     BMI     "
##  R$ = R$ + "BPL     BCC     BCS     BVC     BVS     ADC     SBC     "
##  R$ = R$ + "AND     ORA     EOR     INC     DEC     INX     INY     "
##  R$ = R$ + "DEX     DEY     ROR     ROL     LSR     ASL     CLC     "
##  R$ = R$ + "SEC     CLD     SED     CLI     SEI     BRK     CLV     "
##  R$ = R$ + "PHA     PLA     PHP     PLP     BRK     BIT     RTI     "
##  R$ = R$ + "NOP     .ORG    .END    #INCLUDE.BYTE   .WORD   .TEXT   "
##  IF DEBUG = 1 THEN CALL CONSCOLOR(6): print ("": print (S$
##  L = LEN(S$): T = 1: Q = 0: Q$ = ""
##  DO WHILE T < LEN(S$)
##    TOKEN$ = "": TOKTYPE$ = ""
##A:
##    A$ = UCASE$(MID$(S$, T, 1))
##    IF A$ = " " THEN T = T + 1: GOTO A 'strip spaces
##    TOKEN$ = TOKEN$ + A$
##    'combine functions, commands, labels
##    IF ((A$ >= "G" AND A$ <= "Z") OR A$ = "." OR A$ = "#" OR A$ = "_" OR (((A$ >= "0" AND A$ <= "9") OR A$ = "$") AND TOKTYPE$ = "V") OR ((A$ >= "A" AND A$ <= "F") AND (TOKTYPE$ = "" OR TOKTYPE$ = "V"))) AND A$ <> "(" THEN
##      TOKTYPE$ = "V"
##      IF T < L THEN
##        A$ = UCASE$(MID$(S$, T + 1, 1))
##        IF (A$ >= "A" AND A$ <= "Z") OR A$ = "$" OR A$ = "." OR A$ = "#" OR A$ = "_" OR (A$ = ":" AND Q = 0) OR A$ = "(" OR (A$ >= "0" AND A$ <= "9") THEN
##          T = T + 1: GOTO A
##        END IF
##      END IF
##      IF DEBUG = 1 THEN CALL CONSCOLOR(8)
##    END IF
##    'combine numbers
##    IF ((A$ >= "0" AND A$ <= "9") AND TOKTYPE$ <> "V") OR ((A$ = "$" OR A$ = "@") AND TOKTYPE$ = "") OR ((A$ >= "A" AND A$ <= "F") AND TOKTYPE$ = "N") THEN
##      TOKTYPE$ = "N"
##      IF T < L THEN
##        A$ = MID$(S$, T + 1, 1)
##        IF (A$ >= "0" AND A$ <= "9") OR (A$ >= "A" AND A$ <= "F") THEN T = T + 1: GOTO A
##      END IF
##    END IF
##    'combine relational operators
##    IF TOKEN$ = "=" OR TOKEN$ = "<" OR TOKEN$ = ">" THEN
##      A$ = MID$(S$, T + 1, 1)
##      IF A$ = "=" OR A$ = "<" OR A$ = ">" THEN TOKEN$ = TOKEN$ + A$: T = T + 1
##    END IF
##    'combine ++ and --
##    IF TOKEN$ = "+" OR TOKEN$ = "-" THEN
##      A$ = MID$(S$, T + 1, 1)
##      IF A$ = "+" OR A$ = "-" THEN TOKEN$ = TOKEN$ + A$: T = T + 1
##    END IF
##    'fine tune results
##    SELECT CASE TOKEN$
##    CASE "'", ";"
##      EXIT DO
##    CASE ":"
##      TOKTYPE$ = ":": IF DEBUG = 1 THEN CALL CONSCOLOR(5)
##    CASE ","
##      TOKTYPE$ = ",": IF DEBUG = 1 THEN CALL CONSCOLOR(5)
##    CASE ")"
##      TOKTYPE$ = ")": IF DEBUG = 1 THEN CALL CONSCOLOR(5)
##    CASE "(", "XOR", "OR", "AND", ">=", "<=", "<>", "<", ">", ">>=", "<<", "=", "-", "+", "--", "++", "/", "*"
##      TOKTYPE$ = "O"
##      IF DEBUG = 1 THEN CALL CONSCOLOR(1)
##    CASE CHR$(34)       'string ""
##      TOKTYPE$ = "S"
##      B = INSTR(T + 1, S$, CHR$(34))
##      IF B = 0 THEN CALL ERROROUT("String ended unexpectedly")
##      TOKEN$ = MID$(S$, T + 1, B - T - 1)       'extract string
##      T = B                                     'skip to end of string
##    CASE ELSE
##      IF MID$(TOKEN$, LEN(TOKEN$), 1) = ":" THEN
##        TOKEN$ = MID$(TOKEN$, 1, LEN(TOKEN$) - 1)
##        TOKTYPE$ = "L": IF DEBUG = 1 THEN CALL CONSCOLOR(4)
##      END IF
##      IF LEN(TOKEN$) <= 8 THEN
##        B = INSTR(1, c$, TOKEN$ + SPACE$(8 - LEN(TOKEN$)))
##        IF B > 0 THEN TOKTYPE$ = "C": IF DEBUG = 1 THEN CALL CONSCOLOR(3)
##        B = INSTR(1, F$, TOKEN$ + SPACE$(8 - LEN(TOKEN$)))
##        IF B > 0 THEN TOKTYPE$ = "F": IF DEBUG = 1 THEN CALL CONSCOLOR(3)
##        B = INSTR(1, R$, TOKEN$ + SPACE$(8 - LEN(TOKEN$)))
##        IF B > 0 THEN TOKTYPE$ = "R": IF DEBUG = 1 THEN CALL CONSCOLOR(2)
##      END IF
##      IF TOKTYPE$ = "L" THEN TOKEN$ = TOKEN$ + ":"
##    END SELECT
##    IF TOKTYPE$ = "N" THEN
##      CALL TOHEX(TOKEN$)
##      IF DEBUG = 1 THEN CALL CONSCOLOR(15)
##    END IF
##    T = T + 1
##    IF DEBUG = 1 THEN print ("["; TOKEN$; " "; TOKTYPE$; "] "; : CALL CONSCOLOR(7)
##    Q$ = Q$ + TOKTYPE$
##    TOKEN$(Q) = TOKEN$: Q = Q + 1
##  LOOP
##  IF DEBUG = 1 THEN CALL CONSCOLOR(7)
##  PARSE$ = Q$ + "E"
##'(S)tring
##'(N)umber
##'(V)ariable
##'(O)perator
##'(F)unction
##'(C)ommand
##'(L)abel
##'(:)
##'(,)
##'(E)nd
##END FUNCTION

# Parse an input line
# Updates tokenArray
# returns a string of the tokenTypes used on this line
def PARSE(s):
    global gCurrentParseLine
    gCurrentParseLine = s
    #if (DEBUG):
    #    print("PARSING " + str(LINENUM) + ": " + s)
    print("PARSING " + str(LINENUM) + ": " + s)

    # replaced c$ with commands
    commands = { "CLS", "POKE", "LOCATE", "PRINT", "PUT", "CHAR", "SPRITE",
                "CHARSET", "TITLE", "RETURN", "SUB", "GOSUB", "PUT(", "GOTO",
                "IF", "THEN", "UP", "DOWN", "LEFT", "RIGHT", "DEFINE",
                "DO", "LOOP", "EXIT", "KEYPAD", "INPUT", "FOR", "TO",
                "NEXT", "DATA", "POS", "MOVEUP", "}", "SCREEN", "PALETTE",
                "MEMCOPY", "MEMAREA", "IF_", "FOR_", "MULADD", "PUSH", "POP",
                "SET", "SPRITES", "INTERNALDLIST", "ATTRACT", "END", "DIV8",
                "DIV16", "MUL8", "SELECT", "CASE", "CASE_", "DO_", "LOOP_",
                "ELSE", "AUTHOR", "SOUND", "VOLUME", "MISSILES" }
    # replaced F$ with functions
    functions = { "JOYX(", "JOYY(", "TRIG(", "INKEY", "CHR$(", "SCREEN(", "PEEK(",
                "JOYTRIG(", "JOYTRIG2" }
    # replaced R$ with opcodes
    opcodes = { "LDA", "LDX", "LDY", "STA", "STX", "STY", "TAX",
                "TAY", "TXA", "TYA", "TXS", "TSX", "JMP", "JSR",
                "RTS", "CMP", "CPX", "CPY", "BNE", "BEQ", "BMI",
                "BPL", "BCC", "BCS", "BVC", "BVS", "ADC", "SBC",
                "AND", "ORA", "EOR", "INC", "DEC", "INX", "INY",
                "DEX", "DEY", "ROR", "ROL", "LSR", "ASL", "CLC",
                "SEC", "CLD", "SED", "CLI", "SEI", "BRK", "CLV",
                "PHA", "PLA", "PHP", "PLP", "BRK ", "BIT", "RTI",
                "NOP", ".ORG", ".END", "#INCLUDE", ".BYTE", ".WORD", ".TEXT" }
    # for operatros case below
    operators = { "(", "XOR", "OR", "AND", ">=", "<=", "<>", "<", ">", ">>=", "<<", "=",
                  "-", "+", "--", "++", "/", "*" }

    # TODO - remove
    if (LINENUM == 543):
        print("breakpoint")

    if (DEBUG):
        CONSCOLOR(6)
        print ("")
        print (s + "[EOL]")

    s = s.rstrip(" \n");
    length = len(s)
    T = 0   # was 1
    Q = 0
    sQ = ""
    token = ""                  # moved out of while() below as we use continue
    tokenType = ""
    while (T < length):
        # replaced A$ with c
#A:
        c = s[T].upper()
        while (' ' == c and T < len(s) - 1):       # skip spaces
            T += 1
            c = s[T].upper()
        token = token + c
        # combine functions, commands, labels
# IF ((A$ >= "G" AND A$ <= "Z") OR A$ = "." OR A$ = "#" OR A$ = "_" OR (((A$ >= "0" AND A$ <= "9") OR A$ = "$") AND TOKTYPE$ = "V") OR ((A$ >= "A" AND A$ <= "F") AND (TOKTYPE$ = "" OR TOKTYPE$ = "V"))) AND A$ <> "(" THEN
        cn = ord(c)
        if (((cn >= ord('G') and cn <= ord('Z')) or c == '.' or c == '#' or c == '_' or (((cn >= ord('0') and cn <= ord('9')) or c == '$') and tokenType == 'V') or ((cn >= ord('A') and cn <= ord('F')) and (tokenType == "" or tokenType == 'V'))) and c != '('):
            tokenType = 'V'
            if (T < length - 1):
                c = s[T+1].upper()
# IF (A$ >= "A" AND A$ <= "Z") OR A$ = "$" OR A$ = "." OR A$ = "#" OR A$ = "_" OR (A$ = ":" AND Q = 0) OR A$ = "(" OR (A$ >= "0" AND A$ <= "9") THEN
                cn = ord(c)
                if ((cn >= ord('A') and cn <= ord('Z')) or c == '$' or c == '.' or c == '#' or c == '_' or (c == ':' and Q == 0) or c == '(' or (cn >= ord('0') and cn <= ord('9'))):
                    T += 1
                    continue    #was GOTO A:
            if (DEBUG):
                CONSCOLOR(8)

        # combine numbers
#IF ((A$ >= "0" AND A$ <= "9") AND TOKTYPE$ <> "V") OR ((A$ = "$" OR A$ = "@") AND TOKTYPE$ = "") OR ((A$ >= "A" AND A$ <= "F") AND TOKTYPE$ = "N") THEN
        cn = ord(c)
        if (((cn >= ord('0') and cn <= ord('9')) and tokenType != 'V') or ((c == '$' or c == '@') and tokenType == "") or ((cn >= ord('A') and cn <= ord('F')) and tokenType == 'N')):
            tokenType = 'N'
            if ( T < length - 1):
                c = s[T+1]
# IF (A$ >= "0" AND A$ <= "9") OR (A$ >= "A" AND A$ <= "F") THEN T = T + 1: GOTO A
                cn = ord(c)
                if ((cn >= ord('0') and cn <= ord('9')) or (cn >= ord('A') and cn <= ord('F'))):
                    T += 1
                    continue    # was GOTO A:

        # combine relational operators
        if (token == '=' or token == '<' or token == '>'):
            c = s[T+1]
# IF A$ = "=" OR A$ = "<" OR A$ = ">" THEN TOKEN$ = TOKEN$ + A$: T = T + 1
            if (c == '=' or c == '<' or c == '>'):
                token += c
                T += 1

        # combine ++ and --
        if (token == '+' or token == '-'):
            if (T < length - 1): c = s[T+1]
            else: c = ''
            if (c == '+' or c == '-'):
                token += c
                T += 1

        # fine tune results
        if (token == "'" or token == ";"):
            break
        elif (token == ":"):
            tokenType = ':'
            if (DEBUG): CONSCOLOR(5)
        elif (token == ","):
            tokenType = ','
            if (DEBUG): CONSCOLOR(5)
        elif (token == ")"):
            tokenType = ')'
            if (DEBUG): CONSCOLOR(5)
        elif (token in operators):
            tokenType = 'O'
            if (DEBUG): CONSCOLOR(1)
        elif (token == '"'):             # string quote
            tokenType = 'S'
            pos = s.find('"', T + 1)
            if (-1 == pos):
                ERROROUT("String ended unexpectedly")
            else:
                token = s[T:pos]
                T = pos
        else:
            if (token.endswith(":")):
                token = token.rstrip(":")
                tokenType = 'L'
                if (DEBUG): CONSCOLOR(4)
            if (len(token) <= 8):
                if (token in commands):
                    tokenType = 'C'
                    if (DEBUG): CONSCOLOR(3)
                elif (token in functions):
                    tokenType = 'F'
                    if (DEBUG): CONSCOLOR(3)
                elif (token in opcodes):
                    tokenType = 'R'
                    if (DEBUG): CONSCOLOR(2)
            if (tokenType == 'L'):                  # TODO - check indent!
                token += ':'

        if (tokenType == 'N'):
            token = TOHEX(token)
            if (DEBUG): CONSCOLOR(15)

        T += 1
        if (DEBUG):
            print ("[" + token + " " + tokenType + "] ")
            CONSCOLOR(7)
        sQ += tokenType
        tokenArray.append(token)
        token = ""
        tokenType = ""
        Q = Q + 1       # index, needed for if (... and Q == 0) above
        # end while

    if (DEBUG): CONSCOLOR(7)
    return (sQ + "E")
#end def


def PRINTASC(s):
    a = ""
    for c in s:
        cn = ord(c)
        if (cn < 32 or cn == 34 or cn == 92 or cn == 255):
            if (len(a) > 0):
                PRINTOUT('.TEXT', '"' + a + '"')
                a = ""
                PRINTOUT(".BYTE", "$" + HEX2(cn))
        else:
            a = a + c
            if (len(a) >= 40):
                PRINTOUT(".TEXT", '"' + a + '"')
                a = ""

    if (len(a) > 0):
        PRINTOUT(".TEXT", '"' + a + '"')
# end def

# Write opcode and operands to the output file
def PRINTOUT(opcode, operands):
    global useDASM, outFile
    if (useDASM):
        if (opcode.startswith(".TEXT")):
            opcode = opcode.replace(".TEXT", ".BYTE", 1)
        if (opcode.startswith(".WORD") or opcode.startswith(".BYTE") or opcode.startswith(".ORG")):
            opcode = " " + opcode

    if (operands.startswith("~")):
        operands = operands[1:]
        opcode = opcode.ljust(32)                    # pad to 32 chars with spaces
    else:
        outFile.write("        ")
        opcode = opcode.ljust(len(opcode) + (8 - (len(opcode) % 8)))
        pos = operands.find(";")
        if (pos > -1 and pos <= 16):
            s = operands[pos:]
            operands = operands[0:pos].ljust(16) + s

    outFile.write(opcode + operands + "\n")
#end def                  
         
# reg = W$
# token = TOKEN$(J)
# tokenType = MID$(Q$, J + 1, 1)
def STX(reg):
    # reg = value to store (AXY), token = variable to store into (AXYV)
    global J, tokenTypes, tokenArray
    if (tokenTypes[J+1] != 'V'):
        ERROROUT("Type mismatch")
    token = tokenArray[J]
    if (token != reg):
        if (token == 'A' or token == 'X' or token == 'Y'):
            if ((reg == 'X' or reg == 'Y') and (token == 'X' or token == 'Y')):
                ERROROUT("Illegal XY assign")
            PRINTOUT("T" + reg + token, "");
        else:
            PRINTOUT("ST" + reg, token)
#end def

# TOHEX changed to return the hex value
def TOHEX(s):
    if ('$' == s[0]):           # hex
        s = s[1:]
        len1 = len(s)
        if (len1 > 4):
            ERROROUT("Number too large")
        s = s.rjust(4, '0')
    elif ('@' == s[0]):         # binary
        b = 0
        s = s[1:]
        for c in s:
            v = 0
            if (c == '1'): v = 1
            b = b * 2 + v
        s = HEX4(b)
    else:                       # decimal
        s = HEX4(int(s))

    return s
#end def

# Replace backslashes with double backslashes?
def BACKSLASH(text):
    a = text
    a.replace("\\", "\\\\")
    return a;
#end def


def INCLUDES(sOPTIONS, useDASM, JUSTINC):
    global includeArray, outFile  
    # "local" function definition
    def GETNUM(s):
        n = 0
        length = 0
        for c in s:
            cn = ord(c)
            if (cn < 48 or cn > 57):
                break
            n = 10 * n + (cn - 48)
            length += 1

        return n, length
    #end local def

    # "local" function definition (parent = INCLUDES())
    def INCLUDEF(fileName):
        fileName = "INC\\" + fileName + ".INC"
        file = open(fileName, 'r')
        for line in file:
            tildePos = line.find('~')
            # TODO - output to output file
            if (-1 == tildePos):
                #print(line)
                outFile.write(line)
            else:
                # Note - have to use custom iterator here as it is not continuous -
                #        depends on characters consumed by GETNUM()
                b = line.find('~', tildePos + 1)
                sE = line[tildePos+1:b]
                sS = ""
                index = 0
                while (index < len(sE)):
                    c = sE[index]
                    if ('A' == c):                  #PRINT A (OR NOT)
                        if (useDASM): sS = ""
                        else: sS = "A"
                        break
                    elif ('O' == c):                #TEXT$=OPTIONS$(#)
                        n, m = GETNUM(sE[index+1:])       # TODO index of c
                        sS = sOPTIONS[0][n]
                        index += m
                    elif ('D' == c):                #NUMBER=DEC(TEXT$)
                        S = DEC(sS)
                    elif ('H' == c):                #TEXT$=HEX4$(NUMBER)
                        sS = HEX4(S)
                    elif ('<' == c):                #TEXT$=HIGH BYTE(TEXT$)
                        sS = sS[0:2]
                    elif ('>' == c):                #TEXT$=LOW BYTE(TEXT$)
                        sS = sS[2:4]
                    elif ('+' == c):                #NUMBER=NUMBER+#
                        n, m = GETNUM(sE[index+1:])
                        S += n
                        index += m
                    elif ('-' == c):                #NUMBER=NUMBER-#
                        n, m = GETNUM(sE[index+1:])
                        S -= n
                        index += m
                    elif ('~' == c):
                        break
                    else:
                        # print to error file
                        print ("")
                        print ("Invalid include expression in INC\\", sFILENAME, ".INC")
                        exit()
                    index += 1
                    
                # print to output file
                #print #2 (line[0:a-2], sS, line[b:])
                #outFile.write(line[0:tildePos] + sS + line[b+1:])
                PRINTOUT(line[0:tildePos] + sS + line[b+1:], "~")
        # end for        
        file.close()
    #end local def

    includeArray1 = [ "", "Y2K", "CLEARRAM", "DEFERVBI", "KEYPAD", "JOYSTICK" ]
    includeArray2 = [ "", "CLS", "PRINT", "RIGHT", "LEFT", "INPUT", "MULADD",
                      "DIVB", "DOWN", "UP", "MUL8", "MOVEUP", "CHKROW", "CRLF",
                      "POS", "MEMCOPY", "MEMAREA", "PUTSPR", "DIV16", "PUTMSL",
                      "PUTMSH", "PUTSPH", "PUTMSI", "PUTSPI" ]
    
    if (JUSTINC > 0 and JUSTINC < 6):
        sFILENAME = includeArray1[JUSTINC]
        INCLUDEF(sFILENAME)
    else:
        T = 0
        if (True in includeArray):
            T = includeArray.index(True) + 1
        while (T > 0):
            if (T < 24):
                sFILENAME = includeArray2[T]
                INCLUDEF(sFILENAME)
            # Handle dependancies
            if (8 == T):
                if (includeArray[2] == False):
                    includeArray[2] = True
            elif (9 == T):
                if (includeArray[3] == False):
                    includeArray[3] = True
            elif (12 == T):
                if (includeArray[13] == False):
                    includeArray[13] = True
                if (includeArray[10] == False):
                    includeArray[10] = True
            elif (13 == T):
                if (includeArray[13] == False):
                    includeArray[13] = True
                if (includeArray[5] == False):
                    includeArray[5] = True
                if (includeArray[11] == False):
                    includeArray[11] = True
            elif (14 == T):
                if (includeArray[17] == False):
                    includeArray[17] = True
            # update include string
            includeArray[T-1] = 'x'
            # Find next
            T = 0
            if (True in includeArray):
                T = includeArray.index(True) + 1
        # wend   
#end def

##INCLUDEF:
##  OPEN "INC\" + FILENAME$ + ".INC" FOR INPUT AS #4
##  DO UNTIL EOF(4)
##    LINE INPUT #4, A$
##    A = INSTR(A$, "~")
##    IF A = 0 THEN
##      PRINT #2, A$
##    ELSE
##      B = INSTR(A + 1, A$, "~")
##      E$ = MID$(A$, A + 1, B - A)
##      E = 1
##      DO
##        SELECT CASE MID$(E$, E, 1)
##        CASE "A"                'PRINT A (OR NOT)
##          IF DASM = FALSE THEN S$ = "A" ELSE S$ = ""
##          EXIT DO
##        CASE "O"                'TEXT$=OPTIONS$(#)
##          GOSUB GETNUM
##          S$ = OPTIONS$(N, 0)
##        CASE "D"                'NUMBER=DEC(TEXT$)
##          S = DEC(S$): E = E + 1
##        CASE "H"                'TEXT$=HEX4$(NUMBER)
##          S$ = HEX4$(S): E = E + 1
##        CASE "<"                'TEXT$=HIGH BYTE(TEXT$)
##          S$ = MID$(S$, 1, 2): E = E + 1
##        CASE ">"                'TEXT$=LOW BYTE(TEXT$)
##          S$ = MID$(S$, 3, 2): E = E + 1
##        CASE "+"                'NUMBER=NUMBER+#
##          GOSUB GETNUM: S = S + N
##        CASE "-"                'NUMBER=NUMBER-#
##          GOSUB GETNUM: S = S - N
##        CASE "~"
##          EXIT DO
##        CASE ELSE
##          PRINT #3, ""
##          PRINT #3, "Invalid include expression in INC\"; FILENAME$; ".INC"
##          END
##        END SELECT
##      LOOP
##      PRINT #2, MID$(A$, 1, A - 1); S$; MID$(A$, B + 1)
##    END IF
##  LOOP
##  CLOSE #4
##RETURN


#############################################################################
# run main with concatenated argument list
#############################################################################
args = ""
for s in sys.argv[1:]:
    args = args + s + ' ' 

#main(args)
main("/D jumpong2.bas")
exit()

