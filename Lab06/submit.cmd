@echo off
echo -
echo -
echo - Copying all files you should not have changed
copy p:\CS-1520-Ada\Lab06\Big_Natural.ads      Big_Natural.ads
copy p:\CS-1520-Ada\Lab06\Big_Natural-IO.ads   Big_Natural-IO.ads
copy p:\CS-1520-Ada\Lab06\Big_Natural-IO.adb   Big_Natural-IO.adb
copy p:\CS-1520-Ada\Lab06\Big_Natural_Test.adb Big_Natural_Test.adb
echo -
echo - Compiling to produce listings of Big_Natural package body
gcc -c -gnat05 -gnatcl -gnatwcdfgkmruvwz -gnaty3aAeiklM110rtx Big_Natural.adb  > Big_Natural.lst
echo -
echo - Compiling, binding, and linking the test program
gnatmake -f -gnato -gnat05 Big_Natural_Test
echo -
echo -
echo - Running the test program with my test data
Big_Natural_Test < p:\CS-1520-Ada\Lab06\data.txt > output.txt
echo - Please conserve paper.  Don't print until you have a working program.
echo -    You can check your output by looking at the files called Output.txt
echo -    You can check for compiler warnings by looking at the file called Big_Natural.lst
echo - 
set /p choice=Do you wish to print a copy to turn in? [Type "Yes" to print]  
if not "%choice%"=="Yes" goto end
echo -
echo - Printing results
C:\enscript\enscript --landscape --pretty-print=ada -fCourier9  Big_Natural.lst output.txt
echo -
:end
echo -
echo - Deleting all .ali, .o, and .exe files
gnatclean *
echo - All done
echo - Pick up your output from the printer