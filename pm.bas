' Atari 5200 Player/Missile sample code
' Originally written for DASM by Daniel Boris (dboris@comcast.net)
' 5200BAS version by James Higgs 2016
' Compile with 5200BAS
' Note: DLI routine removed to make it noob-friendly

TITLE OFF
'SET DLIST=$B000						' SO that 5200BAS does not include a DLIST in the ASM

'*************** Variables ***********************
DEFINE line, $80								' Current DLI line
DEFINE pm0pos, $81							' Current pos of P0
DEFINE Z,$82

'SCREEN 4
CLS
PRINT "PLAYER/MISSILE EXAMPLE"

DO
LOOP

';************* Setup hardware registers *************
'
'        lda     #$22            ;Set color PF0
'        sta     sCOLOR0
'        lda     #$0F            ;Set color PF1
'        sta     sCOLOR1
'        lda     #$84            ;Set color PF2
'        sta     sCOLOR2             
'        lda     #$00            ;Set Display list pointer
'        sta     sDLISTL
'        sta     DLISTL
'        lda     #$10
'        sta     sDLISTH
'        sta     DLISTH
'        lda     #$f8            ;Set Charcter Set Base
'        sta     CHBASE
'        lda     #$22            ;Enable DMA
'        sta     sDMACTL
'        lda     #$C0            ;Enable NMI + DLI
'        sta     NMIEN

PALETTE $01, $22
PALETTE $02, $0F
PALETTE $03, $84

' LEAVE DLIST at BF00, SCREEN DATA AT $1000
'A=0
'POKE SDLSTL,A
'POKE DLISTL,A
'A=$B0
'POKE SDLSTH,A
'POKE DLISTH,A

CHARSET $F800

' 5200BAS TODO - POKE N,N
A=$22
POKE SDMCTL,A								' Enable display list DMA, and normal background
A=$40
POKE NMIEN,A									' Enable VBI

';************ Draw display graphics *******************
'
'        ldy     #$02            ;Draw bars on screen
'        lda     #$18            ;Screen memory starts at $1800
'        sta     $81             
'        lda     #$00
'        sta     $80
'        ldx     #$18
'crloop5
'        lda     #$FF            ;Bar 4 pixels wide of color 3
'        sta     ($80),y         ;Store data
'        iny                     
'        iny                     ;Skip 4 pixels
'        lda     #$55            ;Bar 4 pixels wide of color 1
'        sta     ($80),y         ;Store data
'        iny
'        iny                     ;Skip 4 pixels
'        lda     #$AA            ;Bar 4 pixels wide of color 2
'        sta     ($80),y         ;Store data
'        tya
'        clc         
'        adc     #$06            ;Move pointer to next line
'        tay
'        dex                     ;Next line
'        bne     crloop5         ;Branch if not done

'POKE $80,$1800
Y=$02
Z=0
FOR Z TO $17
	A = $FF
    POKE $1000+Y,A						';Bar 4 pixels wide of color 3
	'Y = Y + 2		TODO 5200BAS - Y = Y + 2
	A = Y : A = A + 2 : Y = A
	A = $55
    POKE $1000+Y,A						';Bar 4 pixels wide of color 1
    'Y = Y + 2
	A = Y : A = A + 2 : Y = A
	A = $AA
    POKE $1000+Y,A						';Bar 4 pixels wide of color 2
    'Y = Y + 6
	A = Y : A = A + 6 : Y = A
NEXT Z

';************* Setup Player/Missile registers ***************
'
'        lda     #$3A           ;Enable DMA (single line resolution/
'        sta     sDMACTL        ;normal background)
'        lda     #$20           ;Set PM base address ($200)
'        sta     PMBASE
'        lda     #$03           ;Enable players and missiles
'        sta     GRACTL
'        lda     #$16           ;Color of player 0
'        sta     sCOLPM0
'        ldy     #$00
'        lda     #$03           ;Size of player 0
'        sta     SIZEP0
'        lda     #$01           ;Give players priority over playfield
'        sta     PRIOR

A=$3A
POKE SDMCTL,A				' Enable display list DMA + 2 line sprites resolution + normal width background
SET SPRITES=$2000				' TODO check this - sprites seem to be copied to $2430 & $24c0
SPRITES ON							' Enable PM graphics display
PALETTE 5, $16
' TODO 5200BAS - Sprite size and priority (eg: "SPRITEATTR" command)
A=$03
POKE SIZEP0,A
A=$01
POKE PRIOR,A


'************ Copy player data to RAM ********************************
' TODO 5200BAS - MEMCOPY label, label, count
'MEMCOPY pm1,$2430,8
'MEMCOPY pm1,$24C0,8
MEMCOPY $B100,$2430,8
MEMCOPY $B100,$24C0,8

';************ Move player ********************************************
'
'        ldx     #$20            ;Starting position of player
'mvloop1
'        jsr     waitvb          ;Wait for a vertical bank
'        lda     #$00            ;Reset line counter
'        sta     line
'        stx     HPOSP0          ;Set position of player
'        stx     pm0pos          ;Save position for DLI
'        inx
'        cpx     #$B0            ;Check for end of move
'        bne     mvloop1         ;If not keep moving right
'        lda     #$04            ;Give playfield priority player
'        sta     PRIOR
'
'mvloop2
'        jsr     waitvb          ;Wait for a vertical blank
'        lda     #$00            ;Reset line counter
'        sta     line
'        stx     HPOSP0          ;Set position of player
'        stx     pm0pos          ;Save position for DLI
'        dex
'        cpx     #$40            ;Check for end of move
'        bne     mvloop2         ;If not keep moving left
'        lda     #$01            ;Give player priority over playfield
'        sta     PRIOR
'        jmp     mvloop1         ;Continue looping

' Move player (main loop)
Z=$20
DO
	DO
		GOSUB WAITVSYNC
		line=0
		PUT (Z, 20), pm1, 0, 8
		pm0pos = Z
		Z=Z+1
		A = Z
		IF A=$B0 THEN EXIT DO
	LOOP
	A=$04 : POKE PRIOR,A
	DO
		GOSUB WAITVSYNC
		line=0
		PUT (Z, 20), pm1, 0, 8
		pm0pos = Z
		Z=Z-1
		A = Z
		IF A=$40 THEN EXIT DO
	LOOP
	A=$01 : POKE PRIOR,A
LOOP

';************ Wait for vertical blank ************************
'
'waitvb
'        lda     $02     ;Read timer (this is incremented during VB)
'waitvb2
'        cmp     $02         ;Did it change?
'        beq     waitvb2     ;If not keep waiting
'        rts

' wait for vsync (TODO: 5200BAS should have a "WAITVSYNC" commmand)
WAITVSYNC:
DO
A=PEEK(VCOUNT)
IF A=100 THEN EXIT DO
        STA        WSYNC
LOOP
RETURN

';************ Display list interrupt ************************
'        org  $5000
'dli
'        pha             ;Save A
'        inc line        ;Increment the line counter
'        lda line        ;Past the fifth DLI?
'        cmp #$05
'        bne done        ;If not then exit DLI
'        lda pm0pos      ;Get player 0 position
'        eor #$FF        ;Invert it
'        sta HPOSP0      ;Set player 0 position
'        lda #$0F        ;Change player color
'        sta COLPM0      ;
'        ; Note: Player color is changed in hardware register not the shadow
'        ; register so it takes effect immediatly. 
'done
'        pla             ;Restore A
'        rti             ;Done


''************* Display list data ****************************
'.ORG    $B000
'dlist:
'        .BYTE     $70,$70,$70      ;24 blank scanlines
'        .BYTE     $48,$00,$18      ;Mode 8 and Load memory scan $1800
'        .BYTE     $88,$88,$88,$88,$88,$88,$88   ;23 more line of mode 8
'        .BYTE     $88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
'        .BYTE     $88,$88,$88
'        .BYTE     $41,$00,$B0       ;Jump back to start at $B000

';************* Player shape *********************************
.ORG    $B100
pm1:
        .BYTE     %00111100
        .BYTE     %01000010
        .BYTE     %10100101
        .BYTE     %10000001
        .BYTE     %10100101
        .BYTE     %10011001
        .BYTE     %01000010
        .BYTE     %00111100

