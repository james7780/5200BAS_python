' Atari 5200 Player/Missile sample code
' Originally written for DASM by Daniel Boris (dboris@comcast.net)
' 5200BAS version by James Higgs 2016
' Compile with 5200BAS
' Note: DLI routine removed to make it noob-friendly

'TITLE OFF
TITLE "PM EXAMPLE"
'if title text is in lower case it rainbows.  If caps = solid
AUTHOR "   (C) ANON 2112    "

'*************** Variables ***********************
DEFINE line, $80								' Current DLI line
DEFINE pm0pos, $81							' Current pos of P0
DEFINE Z,$82

SET SCREEN=$1000								' Screen data address = $1000 (display list must load from here)
SET SPRITES=$2000								' Sprite data sddress
SET CHARSET=$F800 ROM						' Set character set address (must be page-aligned)

SCREEN 7
CLS
PRINT "PLAYER/MISSILE EXAMPLE"

'DO
'LOOP

' Set playfield colours
PALETTE $01, $22
PALETTE $02, $0F
PALETTE $03, $84

'CHARSET $F800

' 5200BAS TODO - POKE N,N
A=$22
POKE SDMCTL,A								' Enable display list DMA, and normal background
A=$40
POKE NMIEN,A									' Enable VBI

'************ Draw some playfield character graphics *******************
Y=$02
Z=1
' TODO - FIX FOR ... NEXT (DOES NOT WORK)
'FOR Z TO $17
DO
	A = $FF
	' TODO : POKE label,A and POKE label+Y,A
	POKE $1028+Y,A						';Bar 4 pixels wide of color 3
	'Y = Y + 2		TODO 5200BAS - Y = Y + 2
	A = Y : A = A + 2 : Y = A
	A = $55
	POKE $1028+Y,A						';Bar 4 pixels wide of color 1
	'Y = Y + 2
	A = Y : A = A + 2 : Y = A
	A = $AA
	POKE $1028+Y,A						';Bar 4 pixels wide of color 2
	'Y = Y + 6
	A = Y : A = A + 6 : Y = A
	Z = Z + 1
	A = Z
	IF A = 24 THEN EXIT DO
LOOP
'NEXT Z

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
SPRITES ON							' Enable PM graphics display
A=$03
POKE GRACTL,A
PALETTE $05, $1E
' TODO 5200BAS - Sprite size and priority (eg: "SPRITEATTR" command)
A=$03
POKE SIZEP0,A
A=$01
POKE PRIOR,A


'************ Copy player data to RAM ********************************
' TODO 5200BAS - MEMCOPY label, label, count
'MEMCOPY pm1,$2430,8
'MEMCOPY pm1,$24C0,8
MEMCOPY $B100,$2030,$08
MEMCOPY $B100,$20C0,$08

'DO
'LOOP

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
Z=0
DO
	DO
		GOSUB WAITVSYNC
		line=0
		'PUT (Z, 10), pm1, 0, 8
		pm0pos = Z
		Z=Z+1
		A = Z
    POKE GRAFP0,A
		POKE HPOSP0,A
		IF A=$80 THEN EXIT DO
	LOOP
	A=$04 : POKE PRIOR,A
	DO
		GOSUB WAITVSYNC
		line=0
		'PUT (Z, 10), pm1, 0, 8
		pm0pos = Z
		Z=Z-1
		A = Z
    POKE GRAFP0,A		
		POKE HPOSP0,A
		IF A=$10 THEN EXIT DO
	LOOP
	A=$01 : POKE PRIOR,A
LOOP

' ************ wait for vertical blank (TODO: 5200BAS should have a "WAITVSYNC" commmand)
WAITVSYNC:
DO
A=PEEK(VCOUNT)
IF A=120 THEN EXIT DO
        STA        WSYNC
LOOP
RETURN

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

