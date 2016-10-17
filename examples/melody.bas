' 5200BAS example program - Play a tune
TITLE "melody"
'if title text is in lower case it rainbows.  If caps = solid
AUTHOR "    (C) JH 2016     "
'AUTHOR STATEMENT MUST CONTAIN 20 CHARS OR WILL NOT COMPILE

' TODO - FINISH!

CLS
PRINT "SIMPLE SONG EXAMPLE"

OFFSET=0
DO
	GOSUB WAITVSYNC
	GOSUB WAITVSYNC
	GOSUB WAITVSYNC
	GOSUB WAITVSYNC
	
	' Update pokey registers with track data
	A = PEEK(TUNE+OFFSET)
	SOUND 0,A
	A = PEEK(TUNE+OFFSET+1)
	SOUND 1,A
	A = PEEK(TUNE+OFFSET+2)
	SOUND 2,A
	A = PEEK(TUNE+OFFSET+3)
	SOUND 3,A
	OFFSET=OFFSET+4
	IF OFFSET=100 THEN OFFSET=0
LOOP

' wait for vsync (no colourbars)
WAITVSYNC:
	DO
		A=PEEK(VCOUNT)
    STA WSYNC
		IF A=100 THEN EXIT DO	' Check after WSYNC 
	LOOP
RETURN

'---------------------------------------------------------------------
' DATA
'---------------------------------------------------------------------
.ORG    $B400                   
TUNE:                                ; data for a tune in ??? format
.BYTE   $FC,$FC,$FC,$FC
.BYTE   $CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC
.BYTE   $CC,$CC,$CC,$CC


' POKEY frequencies
' http://little-scale.blogspot.co.za/2009/01/chiptuning-atari-pokey.html
'Note 12-TET  Result Difference
'48 130.8128 130.61 -2.66
'49 138.5913 138.3 -3.7
'50 146.8324 146.94 1.25
'51 155.5635 155.26 -3.42
'52 164.8138 164.57 -2.55
'53 174.6141 174.15 -4.61
'54 184.9972 184.91 -0.8
'55 195.9977 195.92 -0.7
'56 207.6523 207.01 -5.38
'57 220.0000 219.43 -4.5					A3
'58 233.0819 233.43 2.62
'59 246.9417 247.48 3.74
'60 261.6256 261.22 -2.66
'61 277.1826 276.59 -3.7
'62 293.6648 293.88 1.25
'63 311.1270 310.51 -3.42
'64 329.6276 329.14 -2.55
'65 349.2282 350.15 4.57
'66 369.9944 369.82 -0.8
'67 391.9954 391.84 -0.7
'68 415.3047 416.64 5.54
'69 440.0000 438.86 -4.5					A4
'70 466.1638 463.58 -9.62
'71 493.8833 491.26 -9.23
'72 523.2511 522.45 -2.66
'73 554.3653 557.87 10.91
'74 587.3295 587.76 1.25
'75 622.2540 621.02 -3.42
'76 659.2551 658.29 -2.55
'77 698.4565 700.3 4.57
'78 739.9888 731.43 -20.14
'79 783.9909 783.67 -0.7
'80 830.6094 822.86 -16.23
'81 880.0000 889.58 18.74					A5
'82 932.3275 940.41 14.94
'83 987.7666 997.4 16.81
'84 1046.5023 1061.75 25.04
'85 1108.7305 1097.14 -18.19
'86 1174.6591 1265.93 129.55
'87 1244.5079 1316.57 97.45
'88 1318.5102 1496.1 218.76
'89 1396.9129 1567.35 199.3
'90 1479.9777 1645.71 183.77
'91 1567.9817 1732.33 172.57
'92 1661.2188 1936.13 265.12
'93 1760.0000 2057.14 270.08
'94 1864.6550 2194.29 281.81
'95 1975.5332 2351.02 301.25
'96 2093.0045 2531.87 329.55
'97 2217.4610 2742.86 368.12
'98 2349.3181 2992.21 418.76
'99 2489.0159 3291.43 483.77