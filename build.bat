@ECHO OFF

REM // make sure we can write to the file sonic3k.bin
REM // also make a backup to sonic3k.prev.bin
IF NOT EXIST "Z80 Sound Driver.bin" goto LABLNOCOPY
IF EXIST "Z80 Sound Driver.prev.bin" del "Z80 Sound Driver.prev.bin"
IF EXIST "Z80 Sound Driver.prev.bin" goto LABLNOCOPY
move /Y "Z80 Sound Driver.bin" "Z80 Sound Driver.prev.bin"
IF EXIST "Z80 Sound Driver.bin" goto LABLERROR2

:LABLNOCOPY
REM // delete some intermediate assembler output just in case
IF EXIST "Z80 Sound Driver.p" del "Z80 Sound Driver.p"
IF EXIST "Z80 Sound Driver.p" goto LABLERROR1

REM // clear the output window
REM cls

REM // run the assembler
REM // -xx shows the most detailed error output
REM // -q makes AS shut up
REM // -A gives us a small speedup
set AS_MSGPATH=AS\Win32
set USEANSI=n

REM // allow the user to choose to output error messages to file by supplying the -logerrors parameter
IF "%1"=="-logerrors" ( "AS\Win32\asw.exe" -U -xx -q -E -A -L "Z80 Sound Driver.asm" ) ELSE "AS\Win32\asw.exe" -U -xx -q -A -L "Z80 Sound Driver.asm"

REM // if there were errors, a log file is produced
IF "%1"=="-logerrors" ( IF EXIST "Z80 Sound Driver.log" goto LABLERROR3 )

REM // combine the assembler output into a rom
IF EXIST "Z80 Sound Driver.p" "AS\Win32\s3p2bin" "Z80 Sound Driver.p" "Z80 Sound Driver.bin" "Z80 Sound Driver.h"

REM // done -- pause if we seem to have failed, then exit
IF NOT EXIST "Z80 Sound Driver.p" goto LABLPAUSE
IF EXIST "Z80 Sound Driver.bin" goto LABLEXIT

:LABLPAUSE
pause
goto LABLEXIT

:LABLERROR1
echo Failed to build because write access to "Z80 Sound Driver.p" was denied.
pause
goto LABLEXIT

:LABLERROR2
echo Failed to build because write access to "Z80 Sound Driver.bin" was denied.
pause
goto LABLEXIT

:LABLERROR3
REM // display a noticeable message
echo.
echo **************************************************************************************
echo *                                                                                    *
echo *   There were build errors/warnings. See "Z80 Sound Driver.log" for more details.   *
echo *                                                                                    *
echo **************************************************************************************
echo.
pause

:LABLEXIT
exit /b
