5200BAS Atari 5200 Basic Compiler 1.97 (Python alpha 1)

James Higgs 2016


General:

The original 5200BAS compiler was written around y2k in QBX / QuickBasic, compiled to an .exe.

The original 5200BAS.exe worked only on Windows, and does not work on Win10 (and probably Win8 and Win7 too).

This Python version is an attempt to make 5200BAS more portable and accessible. It should work on Windows, Mac or Linux.

The Python version has some minor tweaks, but will probably also have some bugs due to the conversion.

Currently it is at the state where it will compile all the included examples.


Requirements:

You will need Python 3 installed.

You will also need DASM or TASM assembler. At this point I have not been able to get TASM to work, and 
5200BAS_python is only "tested" with DASM 2.20.11 (20140304).


Usage:

Run 5200basl.py from the python interpreter/command line. The following options are supported: 

Usage: "python 5200basl.py [/16] [/D] [/M] file[.bas][*]"
/16  Compile for 16k ROM (default: 32k)
/D   Compile for DASM assembler (default: TASM)
/M   Don't use ANSI colors in debug output" (unsupported in python command line)
*    Debug output

Run 5200basl.py without options/arguments to display usage above.

If compilation is successfuly, you will get an output .asm file (an 2 include files .1 and .2).

Use DASM to compile the .ASM to a binary:

DASM filename.asm -f3

(-f3 option specifies that we want Atari 8-bit).

At this point, if there are no errors, you should have a file "a.out". Rename this to .bin and run in your favourite emulator (which should be Jum52).

Note: 5200basl.py may run DASM for you.
Note: You can also use a batch file like go.bat to run 5200basl.py on your own .bas file automatically.
Note: The .bas file you are compiling MUST be in the main 5200BAS folder (it relies on /inc folder path) 


Links and references:

http://sebastianmihai.com/main.php?t=62&n=Atari-5200-development-Shooting-Gallery
- From March 2012
- Source code for 5200Basic game
- "dev kit" 5200bas.zip contains TASM assembler (have not verified that it works)


Changes made (v1.97 conversion):
1. Straight conversion from QBX code to Python code
2. Combined includel.bas into the main python script
3. Modifications to strings and arrays to better suit Python lists and dictionaries
4. Descriptive variable names
5. Try reduce global variables and scope everything better
6. Most error messages updated to provide more specific info (user-friendliness)
7. Bug fixes to some small code and logic errors 
8. Python script now calls DASM/TASM with the compiled asm output as input to the assembler
9. SCREEN command now supports ANTIC modes 2 to 14 (sets up display list depending on mode)
10. Added some more examples (pm.bas, joytest.bas)
11. 5200basl.py now runs the assembler (DASM) if the BASIC code compiles to asm without error.
12. Added warning output if dlist/sprites/screen/charset not on correct page/1K boundary, or in invalid address 

[EOF]

