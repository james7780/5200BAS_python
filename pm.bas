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
DEFINE line, $80              ' Current DLI line
DEFINE pm0pos, $81            ' Current pos of P0
DEFINE Z,$82

SET SCREEN=$1000                 ' Screen data address = $1000 (display list must load from here)
SET SPRITES=$2000                ' Sprite data sddress
'SET CHARSET=$B400                ' Character set address (must be page-aligned)
' Note: Do not try to use 5200 charset at $F800, will not work as 5200BAS is set up to use ASCII charset

SCREEN 7
CLS
PRINT "PLAYER/MISSILE EXAMPLE"

' Set playfield colours
PALETTE $01, $22
PALETTE $02, $0F
PALETTE $03, $84

' 5200BAS TODO - POKE N,N
A=$40
POKE NMIEN,A									' Enable VBI (but not DLI)

'************ Draw some playfield character graphics *******************
Y=$02
Z=1
FOR Z TO 10
	A = $42
	' TODO : POKE label,A and POKE label+Y,A
	POKE $1028+Y,A
	POKE $1029+Y,A
	POKE $102A+Y,A
	POKE $102B+Y,A
	A = $82
	POKE $1030+Y,A
	POKE $1031+Y,A
	POKE $1032+Y,A
	POKE $1033+Y,A
	'Y = Y + 20
	A = Y : A = A + 20 : Y = A
NEXT Z

'************* Setup Player/Missile registers ***************
'SPRITES DOUBLE       ' Enable PM graphics display, double-height sprites
SPRITES ON           ' Enable PM graphics display, single-height sprites
A=$03
POKE GRACTL,A
PALETTE $05, $1E			' Set colour of sprite 0
' TODO 5200BAS - Sprite size and priority (eg: "SPRITEATTR" command)
A=$03
POKE SIZEP0,A         ' $3 = quad width
A=$01
POKE PRIOR,A


'************ Copy player data to RAM ********************************
' TODO 5200BAS - MEMCOPY label, label, count
MEMCOPY $B100,$2440,$8
MEMCOPY $B100,$24C0,$8
' Note: PM0 starts at PMBASE + 0x200 in double-line mode (PMBASE + 0x400 in single-line mode)

'************ Move player (main loop) ********************************************
Z=0
DO
	DO
		GOSUB WAITVSYNC
		line=0
		' TODO - Fix PUT
		'PUT (Z, 10), pm1, 0, 8
		pm0pos = Z
		Z=Z+1
		A = Z
'    POKE GRAFP0,A
		POKE HPOSP0,A
		IF A=$B0 THEN EXIT DO
	LOOP
	' 5200BAS TODO - SPRITEPRIOR 
	A=$04 : POKE PRIOR,A
	DO
		GOSUB WAITVSYNC
		line=0
		'PUT (Z, 10), pm1, 0, 8
		pm0pos = Z
		Z=Z-1
		A = Z
'   POKE GRAFP0,A		
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

