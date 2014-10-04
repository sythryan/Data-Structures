@echo off
echo -
echo -
echo - Copying Heart files
copy P:\CS-1520-Ada\Lab08\Postlab\heart.ads heart.ads
copy P:\CS-1520-Ada\Lab08\Postlab\heart.adb heart.adb
echo -
echo - Compiling to produce listings
gcc -c -gnatcl -gnatwcdfgkmruvwz -gnaty3aAeiklM110rtx  cardioverter.adb  > cardioverter.lst
echo -
echo -
echo - Compiling, binding, and linking code
gnatmake -f -gnato cardioverter
echo -
echo -
echo - Running program three times
echo - First run (this will take a while)
cardioverter < P:\CS-1520-Ada\Lab08\script1.txt > Output1.txt
echo - Second run (a quick one)
cardioverter < P:\CS-1520-Ada\Lab08\script2.txt > Output2.txt
echo - Third run (this will take a while)
cardioverter < P:\CS-1520-Ada\Lab08\script3.txt > Output3.txt
echo -
echo -
echo - Please conserve paper.  Don't print until you have a working program.
echo -    Your output is in the files Output1.txt, Output2.txt, and Output3.txt
echo -    Compiler warnings are in the file called cardioverter.lst
echo - 
set /p choice=Do you wish to print a copy to turn in? [Type "Yes" to print]  
if not "%choice%"=="Yes" goto end
echo -
echo - Printing results
C:\enscript\enscript --landscape --pretty-print=ada -fCourier9 cardioverter.lst Output1.txt Output2.txt Output3.txt
echo -
echo - Pick up your output from the printer
:end
echo -
echo -
echo - Deleting all .o, .exe, and .ali files
gnatclean *
echo -
echo - All done