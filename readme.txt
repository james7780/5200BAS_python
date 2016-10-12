5200BAS Atari 5200 Basic Compiler (Python version)

James Higgs 2016


General:

The original 5200BAS compiler was written around y2k in QBX / QuickBasic, compiled to an .exe.

The original 5200BAS.exe worked only on Windows, and does not work on Win10 (and probably Win8 and Win7 too).

This Python version is an attempt to make 5200BAS more portable and accessible. It should work on Windows, Mac or Linux.


Requirements:

You will need Python 3 installed.

You will also need DASM or TASM assembler. At this point I have not been able to get TASM to work, and 5200BAS_python is only "tested" with DASM (version?)


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


Links and references:

http://sebastianmihai.com/main.php?t=62&n=Atari-5200-development-Shooting-Gallery
- From March 2012
- Source code for 5200Basic game
- "dev kit" 5200bas.zip contains TASM assembler (have not verified that it works)



[EOF]

