@set GAME_NAME=sg_main

@REM Clean up
@del %GAME_NAME%.1
@del %GAME_NAME%.2
@del %GAME_NAME%.asm
@del %GAME_NAME%.bin

@REM Compile
5200BAS.EXE %GAME_NAME%.bas

@REM Assemble
@call A6502.BAT %GAME_NAME%

@REM Clean up
@del %GAME_NAME%.1
@del %GAME_NAME%.2
@del %GAME_NAME%.asm

@REM Run
@REM call run.bat

@REM Clean up emulator logs :D
@REM @del 5200.log