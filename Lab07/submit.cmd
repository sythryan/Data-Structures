@echo off
echo -
echo - Copying stack class files
copy P:\CS-1520-Ada\lab07\Postlab\stack.ads stack.ads
copy P:\CS-1520-Ada\lab07\Postlab\stack.adb stack.adb
echo -
echo - Compiling to produce listings
gcc -c -gnatcl -gnatwcdfgkmruvwz -gnaty3aAeiklM110rtx -gnat05 Assign07.adb > Assign07.lst
echo -
echo -
echo - Compiling, binding, and linking code
gnatmake -f -gnato -gnat05 Assign07
echo -
echo -
echo -
echo - Running program
Assign07 < P:\CS-1520-Ada\lab07\Postlab\Data.txt > Output.txt
echo -
echo -
echo -
echo - Please conserve paper.  Don't print until you have a working program.
echo -    You can check your output by looking at the file called Output.txt
echo -    You can check for compiler warnings by looking at the file called Assign07.lst
echo - 
set /p choice=Do you wish to print a copy to turn in? [Type "Yes" to print]  
if not "%choice%"=="Yes" goto end
echo - Printing listing and output file
C:\enscript\enscript --landscape --pretty-print=ada -fCourier9 Assign07.lst Output.txt
echo -
echo - Pick up your output from the printer
echo - Keep the pages in the order printed
:end
echo -
echo - Deleting all .o, .exe, and .ali files
gnatclean *
echo -
echo - All done