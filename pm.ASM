;========================================================
; ASM Code Produced by 5200BAS 1.97
; 2016-10-25 21:21:55.527412    
;========================================================
        processor       6502
 .ORG    $4000                  
ATARI   .EQU    1               
#INCLUDE        EQUATES.INC     
#INCLUDE        HEADER.INC      
#INCLUDE        "pm.2"          
; Atari 5200 Player/Missile sample code
; Originally written for DASM by Daniel Boris (dboris@comcast.net)
; 5200BAS version by James Higgs 2016
; Compile with 5200BAS
; Note: DLI routine removed to make it noob-friendly
;SET DLIST=$B000						' SO that 5200BAS does not include a DLIST in the ASM
;*************** Variables ***********************
LINE    .EQU    $80             
PM0POS  .EQU    $81             
Z       .EQU    $82             
;SCREEN 4
        JSR     CLS             ;CLS 
        LDA     #$00            ;PRINT "PLAYER/MISSILE EXAMPLE" 
        STA     TEMPL
        LDA     #$bb
        STA     TEMPH
        JSR     PRINT
DO1:                            ;DO 
        JMP     DO1             ;LOOP 
ED1:                            
;;************* Setup hardware registers *************
;        lda     #$22            ;Set color PF0
;        sta     sCOLOR0
;        lda     #$0F            ;Set color PF1
;        sta     sCOLOR1
;        lda     #$84            ;Set color PF2
;        sta     sCOLOR2
;        lda     #$00            ;Set Display list pointer
;        sta     sDLISTL
;        sta     DLISTL
;        lda     #$10
;        sta     sDLISTH
;        sta     DLISTH
;        lda     #$f8            ;Set Charcter Set Base
;        sta     CHBASE
;        lda     #$22            ;Enable DMA
;        sta     sDMACTL
;        lda     #$C0            ;Enable NMI + DLI
;        sta     NMIEN
        LDA     #$22            ;A = 0022
        STA     COLOR0          ;PALETTE 0001 , 0022 
        LDA     #$0f            ;A = 000F
        STA     COLOR1          ;PALETTE 0002 , 000F 
        LDA     #$84            ;A = 0084
        STA     COLOR2          ;PALETTE 0003 , 0084 
; LEAVE DLIST at BF00, SCREEN DATA AT $1000
;A=0
;POKE SDLSTL,A
;POKE DLISTL,A
;A=$B0
;POKE SDLSTH,A
;POKE DLISTH,A
        LDA     #$F8            ;CHARSET F800 
        STA     CHBASE
; 5200BAS TODO - POKE N,N
        LDA     #$22            ;A = 0022
        STA     SDMCTL          ;POKE SDMCTL , A 
        LDA     #$40            ;A = 0040
        STA     NMIEN           ;POKE NMIEN , A 
;;************ Draw display graphics *******************
;        ldy     #$02            ;Draw bars on screen
;        lda     #$18            ;Screen memory starts at $1800
;        sta     $81
;        lda     #$00
;        sta     $80
;        ldx     #$18
;crloop5
;        lda     #$FF            ;Bar 4 pixels wide of color 3
;        sta     ($80),y         ;Store data
;        iny
;        iny                     ;Skip 4 pixels
;        lda     #$55            ;Bar 4 pixels wide of color 1
;        sta     ($80),y         ;Store data
;        iny
;        iny                     ;Skip 4 pixels
;        lda     #$AA            ;Bar 4 pixels wide of color 2
;        sta     ($80),y         ;Store data
;        tya
;        clc
;        adc     #$06            ;Move pointer to next line
;        tay
;        dex                     ;Next line
;        bne     crloop5         ;Branch if not done
;POKE $80,$1800
        LDY     #$02            ;Y = 0002
        LDA     #$00            ;A = 0000
        STA     Z               ;Z = 0000 
FR1:                            ;FOR Z TO 0017 
        LDA     #$17            ;A = 0017
        CMP     0017
        BCC     EF1
        LDA     #$ff            ;A = 00FF
        STA     $1000,Y         ;POKE 1000 + Y , A 
;Y = Y + 2		TODO 5200BAS - Y = Y + 2
        TYA                     ;A = Y
        CLC     
        ADC     #$02            ;A = A + 0002 
        TAY                     ;Y = A
        LDA     #$55            ;A = 0055
        STA     $1000,Y         ;POKE 1000 + Y , A 
;Y = Y + 2
        TYA                     ;A = Y
        CLC     
        ADC     #$02            ;A = A + 0002 
        TAY                     ;Y = A
        LDA     #$aa            ;A = 00AA
        STA     $1000,Y         ;POKE 1000 + Y , A 
;Y = Y + 6
        TYA                     ;A = Y
        CLC     
        ADC     #$06            ;A = A + 0006 
        TAY                     ;Y = A
        INC     Z               ;NEXT Z 
        JMP     FR1
EF1:                            
;;************* Setup Player/Missile registers ***************
;        lda     #$3A           ;Enable DMA (single line resolution/
;        sta     sDMACTL        ;normal background)
;        lda     #$20           ;Set PM base address ($200)
;        sta     PMBASE
;        lda     #$03           ;Enable players and missiles
;        sta     GRACTL
;        lda     #$16           ;Color of player 0
;        sta     sCOLPM0
;        ldy     #$00
;        lda     #$03           ;Size of player 0
;        sta     SIZEP0
;        lda     #$01           ;Give players priority over playfield
;        sta     PRIOR
        LDA     #$3a            ;A = 003A
        STA     SDMCTL          ;POKE SDMCTL , A 
        LDA     #$3E            ;SPRITES ON 
        STA     SDMCTL
        LDA     #$16            ;A = 0016
        STA     PCOLR0          ;PALETTE 0005 , 0016 
; TODO 5200BAS - Sprite size and priority (eg: "SPRITEATTR" command)
        LDA     #$03            ;A = 0003
        STA     SIZEP0          ;POKE SIZEP0 , A 
        LDA     #$01            ;A = 0001
        STA     PRIOR           ;POKE PRIOR , A 
;************ Copy player data to RAM ********************************
; TODO 5200BAS - MEMCOPY label, label, count
;MEMCOPY pm1,$2430,8
;MEMCOPY pm1,$24C0,8
        LDA     #$B1            ;MEMCOPY B100 , 2430 , 0007 
        STA     FROMH
        LDA     #$00
        STA     FROML
        LDA     #$24
        STA     TOH
        LDA     #$30
        STA     TOL
        LDA     #$00
        STA     COUNTH
        LDA     #$07
        STA     COUNTL
        JSR     MEMCOPY
        LDA     #$B1            ;MEMCOPY B100 , 24C0 , 0007 
        STA     FROMH
        LDA     #$00
        STA     FROML
        LDA     #$24
        STA     TOH
        LDA     #$C0
        STA     TOL
        LDA     #$00
        STA     COUNTH
        LDA     #$07
        STA     COUNTL
        JSR     MEMCOPY
;;************ Move player ********************************************
;        ldx     #$20            ;Starting position of player
;mvloop1
;        jsr     waitvb          ;Wait for a vertical bank
;        lda     #$00            ;Reset line counter
;        sta     line
;        stx     HPOSP0          ;Set position of player
;        stx     pm0pos          ;Save position for DLI
;        inx
;        cpx     #$B0            ;Check for end of move
;        bne     mvloop1         ;If not keep moving right
;        lda     #$04            ;Give playfield priority player
;        sta     PRIOR
;mvloop2
;        jsr     waitvb          ;Wait for a vertical blank
;        lda     #$00            ;Reset line counter
;        sta     line
;        stx     HPOSP0          ;Set position of player
;        stx     pm0pos          ;Save position for DLI
;        dex
;        cpx     #$40            ;Check for end of move
;        bne     mvloop2         ;If not keep moving left
;        lda     #$01            ;Give player priority over playfield
;        sta     PRIOR
;        jmp     mvloop1         ;Continue looping
; Move player (main loop)
        LDA     #$20            ;A = 0020
        STA     Z               ;Z = 0020 
DO2:                            ;DO 
DO3:                            ;DO 
        JSR     WAITVSYNC       ;GOSUB WAITVSYNC 
        LDA     #$00            ;A = 0000
        STA     LINE            ;LINE = 0000 
        LDA     Z               ;PUT ( Z , 0014 ) , PM1 , 0000 , 0008 
        ADC     #$2F
        STA     HPOSP0
        LDX     #$14            ;X = 0014
        LDA     #$24
        STA     SPRITEH
        LDA     PM1             ;A = PM1
        STA     TEMPH
        LDA     #$08
        STA     FROMH
        JSR     PUTSPH
        LDA     Z               ;A = Z
        STA     PM0POS          ;PM0POS = Z 
        INC     Z               ;Z = Z + 0001 
        LDA     Z               ;A = Z
        CMP     #$b0            ;IF A = 00B0 THEN EXIT ED3 
        BEQ     ED3
        JMP     DO3             ;LOOP 
ED3:                            
        LDA     #$04            ;A = 0004
        STA     PRIOR           ;POKE PRIOR , A 
DO4:                            ;DO 
        JSR     WAITVSYNC       ;GOSUB WAITVSYNC 
        LDA     #$00            ;A = 0000
        STA     LINE            ;LINE = 0000 
        LDA     Z               ;PUT ( Z , 0014 ) , PM1 , 0000 , 0008 
        ADC     #$2F
        STA     HPOSP0
        LDX     #$14            ;X = 0014
        LDA     #$24
        STA     SPRITEH
        LDA     PM1             ;A = PM1
        STA     TEMPH
        LDA     #$08
        STA     FROMH
        JSR     PUTSPH
        LDA     Z               ;A = Z
        STA     PM0POS          ;PM0POS = Z 
        DEC     Z               ;Z = Z - 0001 
        LDA     Z               ;A = Z
        CMP     #$40            ;IF A = 0040 THEN EXIT ED4 
        BEQ     ED4
        JMP     DO4             ;LOOP 
ED4:                            
        LDA     #$01            ;A = 0001
        STA     PRIOR           ;POKE PRIOR , A 
        JMP     DO2             ;LOOP 
ED2:                            
;;************ Wait for vertical blank ************************
;waitvb
;        lda     $02     ;Read timer (this is incremented during VB)
;waitvb2
;        cmp     $02         ;Did it change?
;        beq     waitvb2     ;If not keep waiting
;        rts
; wait for vsync (TODO: 5200BAS should have a "WAITVSYNC" commmand)
WAITVSYNC:                      
DO5:                            ;DO 
        LDA     VCOUNT          ;A = PEEK( VCOUNT ) 
        CMP     #$64            ;IF A = 0064 THEN EXIT ED5 
        BEQ     ED5
        STA        WSYNC
        JMP     DO5             ;LOOP 
ED5:                            
        RTS                     ;RETURN 
;;************ Display list interrupt ************************
;        org  $5000
;dli
;        pha             ;Save A
;        inc line        ;Increment the line counter
;        lda line        ;Past the fifth DLI?
;        cmp #$05
;        bne done        ;If not then exit DLI
;        lda pm0pos      ;Get player 0 position
;        eor #$FF        ;Invert it
;        sta HPOSP0      ;Set player 0 position
;        lda #$0F        ;Change player color
;        sta COLPM0      ;
;        ; Note: Player color is changed in hardware register not the shadow
;        ; register so it takes effect immediatly.
;done
;        pla             ;Restore A
;        rti             ;Done
;'************* Display list data ****************************
;.ORG    $B000
;dlist:
;        .BYTE     $70,$70,$70      ;24 blank scanlines
;        .BYTE     $48,$00,$18      ;Mode 8 and Load memory scan $1800
;        .BYTE     $88,$88,$88,$88,$88,$88,$88   ;23 more line of mode 8
;        .BYTE     $88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
;        .BYTE     $88,$88,$88
;        .BYTE     $41,$00,$B0       ;Jump back to start at $B000
;;************* Player shape *********************************
        .ORG    $B100
PM1:                            
        .BYTE     %00111100
        .BYTE     %01000010
        .BYTE     %10100101
        .BYTE     %10000001
        .BYTE     %10100101
        .BYTE     %10011001
        .BYTE     %01000010
        .BYTE     %00111100
ENDMAIN JMP     ENDMAIN         ;Endless loop
#INCLUDE        "pm.1"          
                                
;-----------------------------------------------------------
; Strings                       
;-----------------------------------------------------------
 .ORG    $BB00                  
         .BYTE  $FF
         .BYTE  "PLAYER/MISSILE EXAMPLE"
         .BYTE  $FF
         .BYTE  $FF
;-----------------------------------------------------------
; KEYPAD -- Keypad buffer handler
; On entry: A=key just pressed  
;-----------------------------------------------------------
 .ORG    $BC00                   

        LDX     KEYDB
        LDY     #$00
        STY     KEYDB           ;reset debounce timer to 0
        CPX     #$05            ;check previous debounce for 5
        BCC     KEYDONE        
        STA     KEY             ;reached, store keypress
KEYDONE PLA     
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
        BMI     VBIDONE         ;if zero, skip
        INC     KEYDB           ;otherwise increase by one
VBIDONE PLA                     
        TAY                     ;POP Y
        PLA     
        TAX                     ;POP X
        PLA                     ;POP A
        RTI     
                                
;-----------------------------------------------------------
; Display List                  
;-----------------------------------------------------------
 .ORG    $BF00                  
         .WORD  $7070           ;skip 24 scan lines
         .BYTE  $70
         .BYTE  $42             ;set up ANTIC2 screen
         .WORD  $1000           ;address of screen memory
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $02
         .BYTE  $41
         .WORD  $BF00           ;jump back to top of list
                                
;-----------------------------------------------------------
; Monitor Information           
;-----------------------------------------------------------
 .ORG    $BFD4                  
         .BYTE  "copyright@RPPR@atari"
         .BYTE  "untitled"
         .BYTE  $B5
 .ORG    $BFFD                  
         .BYTE  $FF             ; Skip title
         .WORD  $4000           ;MAIN
.END                            
