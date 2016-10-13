;========================================================
; ASM Code Produced by 5200BAS1.96       16May2014  7:57p
;========================================================
.ORG    $4000                   
ATARI   .EQU    1               
#INCLUDE        EQUATES.INC     
#INCLUDE        HEADER.INC      
#INCLUDE        "JOYTEST.2"     
;if title text is in lower case it rainbows.  If caps = solid 
;AUTHOR STATEMENT MUST CONTAIN 20 CHARS OR WILL NOT COMPILE 
;Program Automatically procedes after TITLE display for a brief moment 
; 
; 
;Value   Key 
;$00 	0 
;$01 	1 
;$02 	2 
;$03 	3 
;$04 	4 
;$05 	5 
;$06 	6 
;$07 	7 
;$08 	8 
;$09 	9 
;$0A 	* 
;$0B 	# 
;$0C 	Start 
;$0D 	Pause 
;$0E 	Reset 
;$FF 	No key waiting 
; 
; 
PX      .EQU    $FC             
PY      .EQU    $FA             
        LDA     #$3E            ;SPRITES ON 
        STA     SDMCTL
;PALETTE 5,$96 'CHANGES THE COLOR PLAYER1 AND MISSILE 1 TO BLUE 
;player 
        LDA     #$80            ;A = 0080
        STA     PX              ;PX = 0080 
        LDA     #$80            ;A = 0080
        STA     PY              ;PY = 0080 
        JSR     DRAWPLAYER0     ;GOSUB DRAWPLAYER0 
        JSR     DRAWPLAYER1     ;GOSUB DRAWPLAYER1 
        JSR     CLS             ;CLS 
        LDA     #$10            ;LOCATE 0001 , 0001 
        STA     LOCATEH
        LDA     #$29
        STA     LOCATEL
        LDA     #$00            ;PRINT     5200 JOYSTICK TEST BY SIO2 2014ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
DO1:                            ;DO 
        LDA     #$FF            ;A = 00FF
        LDA     KEY             ;A = INKEY 
        LDX     #$FF
        STX     KEY
                                ;SELECT CASE A 
        CMP     #$00            ;CASE 0000 
        BNE     SC2
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$2F            ;PRINT 0ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JMP     SC1
SC2:                            
        CMP     #$01            ;CASE 0001 
        BNE     SC3
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$31            ;PRINT 1ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC3:                            
        CMP     #$02            ;CASE 0002 
        BNE     SC4
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$33            ;PRINT 2ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC4:                            
        CMP     #$03            ;CASE 0003 
        BNE     SC5
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$35            ;PRINT 3ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC5:                            
        CMP     #$04            ;CASE 0004 
        BNE     SC6
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$22            ;PRINT 4ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC6:                            
        CMP     #$05            ;CASE 0005 
        BNE     SC7
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$37            ;PRINT 5ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC7:                            
        CMP     #$06            ;CASE 0006 
        BNE     SC8
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$39            ;PRINT 6ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC8:                            
        CMP     #$07            ;CASE 0007 
        BNE     SC9
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$3B            ;PRINT 7ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC9:                            
        CMP     #$08            ;CASE 0008 
        BNE     SC10
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$3D            ;PRINT 8ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC10:                           
        CMP     #$09            ;CASE 0009 
        BNE     SC11
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$3F            ;PRINT 9ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC11:                           
        CMP     #$0A            ;CASE 000A 
        BNE     SC12
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$41            ;PRINT *ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC12:                           
        CMP     #$0B            ;CASE 000B 
        BNE     SC13
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$43            ;PRINT #ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC13:                           
        CMP     #$0C            ;CASE 000C 
        BNE     SC14
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$45            ;PRINT STARTÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC14:                           
        CMP     #$0D            ;CASE 000D 
        BNE     SC15
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$4B            ;PRINT PAUSEÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
        JMP     SC1
SC15:                           
        CMP     #$0E            ;CASE 000E 
        BNE     SC16
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$24            ;PRINT           ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        LDA     #$10            ;LOCATE 0003 , 0001 
        STA     LOCATEH
        LDA     #$79
        STA     LOCATEL
        LDA     #$51            ;PRINT RESETÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JSR     RIGHT           ;} 
        JSR     CHKROW
SC16:                           
SC1:                            ;END SELECT 
        LDA     TRIG0           ;A = JOYTRIG( 0000 ) 
        CMP     #$01            ;IF A = 0001 THEN 
        BNE     IF1
IF1P1:                          
        LDA     #$10            ;LOCATE 0006 , 0001 
        STA     LOCATEH
        LDA     #$F1
        STA     LOCATEL
        LDA     #$26            ;PRINT         ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JMP     IF2             ;ELSE 
IF1:                            
        LDA     #$10            ;LOCATE 0006 , 0001 
        STA     LOCATEH
        LDA     #$F1
        STA     LOCATEL
        LDA     #$57            ;PRINT FIREÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
IF2:                            ;END IF 
        LDA     SKSTAT          ;A = JOYTRIG2 
        AND     #$08
        CMP     #$08            ;IF A = 0008 THEN 
        BNE     IF3
IF3P1:                          
        LDA     #$10            ;LOCATE 0005 , 0001 
        STA     LOCATEH
        LDA     #$C9
        STA     LOCATEL
        LDA     #$26            ;PRINT         ÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
        JMP     IF4             ;ELSE 
IF3:                            
        LDA     #$10            ;LOCATE 0005 , 0001 
        STA     LOCATEH
        LDA     #$C9
        STA     LOCATEL
        LDA     #$5C            ;PRINT TRIGGERÿ 
        STA     TEMPL
        LDA     #$B0
        STA     TEMPH
        JSR     PRINT
IF4:                            ;END IF 
        LDA     PADDL0          ;A = JOYX( 0000 ) 
        LSR     A               ;A = A / 0002 
        STA     PX              ;PX = A 
        JSR     DRAWPLAYER0     ;GOSUB DRAWPLAYER0 
        LDA     PADDL1          ;A = JOYY( 0000 ) 
        LSR     A               ;A = A / 0002 
        STA     PY              ;PY = A 
        JSR     DRAWPLAYER1     ;GOSUB DRAWPLAYER1 
        LDA     #$00            ;ATTRACT 
        STA     ATRACT
        JMP     DO1             ;LOOP 
ED1:                            
DRAWPLAYER0:                    
        LDA     PX              ;A = PX
        SEC     
        SBC     #$01            ;A = A - 0001 
        STA     PX              ;PX = A 
        LDA     PX              ;PUT ( PX , 0038 ) , 0093 , 0000 , 0008 
        ADC     #$2F
        STA     HPOSP0
        LDX     #$38            ;X = 0038
        LDA     #$34
        STA     SPRITEH
        LDA     #$93            ;A = 0093
        STA     TEMPH
        LDA     #$08
        STA     FROMH
        JSR     PUTSPH
;56 BECAUSE 0 TO 255 MIDDLE IS 128 AND WE SCALED BY / 2 SO HALF 128 IS 64 MINUS HALF THE WIDTH IS 56 
        LDA     PX              ;A = PX
        CLC     
        ADC     #$01            ;A = A + 0001 
        STA     PX              ;PX = A 
        RTS                     ;RETURN 
DRAWPLAYER1:                    
        LDA     PY              ;A = PY
        SEC     
        SBC     #$01            ;A = A - 0001 
        STA     PY              ;PY = A 
        LDA     #$6B            ;PUT ( 003B , PY ) , 0090 , 0004 , 0008 
        STA     HPOSM0
        LDX     PY              ;X = PY
        STX     FROML
        LDX     #$00
        LDA     #$33
        STA     SPRITEH
        LDA     #$90            ;A = 0090
        STA     TEMPH
        LDA     #$08
        STA     FROMH
        JSR     PUTMSH
;59 BECAUSE SAME AS ABOVE BUT LESS WIDTH. 
;LOOKS CENTER IN EMULATOR BUT ON ACTUAL HARDWARE IS OFF SLIGHTLY. 
;PERHAPS MY ATARI NEEDS CALIBRATED. 
        LDA     PY              ;A = PY
        CLC     
        ADC     #$01            ;A = A + 0001 
        STA     PY              ;PY = A 
        RTS                     ;RETURN 
ENDMAIN JMP     ENDMAIN         ;Endless loop
#INCLUDE        "JOYTEST.1"     
                                
;-----------------------------------------------------------
; Strings                       
;-----------------------------------------------------------
.ORG    $B000                   
        .TEXT   "    5200 JOYSTICK TEST BY SIO2 2014"
        .BYTE   $FF
        .TEXT   "          "
        .BYTE   $FF
        .TEXT   "0"
        .BYTE   $FF
        .TEXT   "1"
        .BYTE   $FF
        .TEXT   "2"
        .BYTE   $FF
        .TEXT   "3"
        .BYTE   $FF
        .TEXT   "5"
        .BYTE   $FF
        .TEXT   "6"
        .BYTE   $FF
        .TEXT   "7"
        .BYTE   $FF
        .TEXT   "8"
        .BYTE   $FF
        .TEXT   "9"
        .BYTE   $FF
        .TEXT   "*"
        .BYTE   $FF
        .TEXT   "#"
        .BYTE   $FF
        .TEXT   "START"
        .BYTE   $FF
        .TEXT   "PAUSE"
        .BYTE   $FF
        .TEXT   "RESET"
        .BYTE   $FF
        .TEXT   "FIRE"
        .BYTE   $FF
        .TEXT   "TRIGGER"
        .BYTE   $FF
;-----------------------------------------------------------
; KEYPAD -- Keypad buffer handler
; On entry: A=key just pressed  
;-----------------------------------------------------------
.ORG    $BC00                   
        CMP     KEYLAST         ;same key as last time?
        BNE     KEYOK
        LDX     KEYDB           ;same key, check debounce timer
        BNE     KEYDONE         ;if debounce isn't 0, reject the key
KEYOK   STA     KEY             
        STA     KEYLAST         ;store keypress
KEYDONE LDA     #$02            ;reset debounce timer
        STA     KEYDB
        PLA     
        TAY                     ;POP Y
        PLA     
        TAX                     ;POP X
        PLA                     ;POP A
        RTI     
;-----------------------------------------------------------
; Deferred VBI handler          
;-----------------------------------------------------------
.ORG    $BC20                   
        LDA     KEYDB           ;check key debounce counter
        BEQ     VBIDONE         ;if zero, skip
        DEC     KEYDB           ;otherwise decrease by one
VBIDONE PLA                     
        TAY                     ;POP Y
        PLA     
        TAX                     ;POP X
        PLA                     ;POP A
        RTI     
                                
;-----------------------------------------------------------
; Display List                  
;-----------------------------------------------------------
.ORG    $BFB4                   
        .WORD   $7070           ;skip 24 scan lines
        .BYTE   $70
        .BYTE   $42             ;set up gr.mode 2 screen
        .WORD   $1000           ;address of screen memory
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $41
        .WORD   $BFB4           ;jump back to top of list
                                
;-----------------------------------------------------------
; Monitor Information           
;-----------------------------------------------------------
.ORG    $BFD4                   
        .TEXT   "@@@HcI@RPQT@sioR@@@@"
        .TEXT   "@@@*/934)#+@4%34@@@@"
        .BYTE   $B5
.ORG    $BFFD                   
        .BYTE   $FF             ; Skip title
        .WORD   $4000           ;MAIN
.END                            
