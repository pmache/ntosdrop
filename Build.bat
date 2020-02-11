@ECHO OFF
IF [%2] EQU [] goto setenv
set path=%2\tools\%1;%2\tools;%path%
set wrkarch=%1
goto dothejob
:setenv
call WRKEnv.bat %1
:dothejob
REM
REM Set versions from file
REM
title [Building] Windows NT Build Environment - Kernel
for /f %%a in ('type lhMajorVersion.txt') do set lhMajorVersion=%%a
for /f %%a in ('type lhMinorVersion.txt') do set lhMinorVersion=%%a
for /f %%a in ('type lhBuild.txt') do set /a lhBuild=%%a+1
for /f %%a in ('type lhRevision.txt') do set lhRevision=%%a


set USERDOMAIN=svr03
set lhBuildLab=sp1_rtm
set ProjectName=openroad
set _Nt386Tree=B:\Bins.x86fre

REM
REM Update build in file
REM

echo %lhBuild%> .\lhBuild.txt

REM
REM Get our buildtag from lhBuildTag.exe
REM

for /f %%a in ('lhBuildTag %lhBuild% %USERDOMAIN%_%lhBuildLab%') do set lhBuildTag=%%a
echo %lhMajorVersion%.%lhMinorVersion%.%lhBuildTag% >> tags.txt
cd base\ntos\init
echo WINGEN: Making resource file for NTOSKRNL.EXE...
rc /r /i ..\..\..\public\sdk\inc ntoskrnl.rc 
copy ntoskrnl.res ..\build\prebuilt\i386 /Y
copy ntoskrnl.res ..\build\prebuilt\amd64 /Y
del ntoskrnl.res
cd ..\..\..
REM
REM Update ntverkp.h
REM

cd public\sdk\inc

echo /* > ntverkp.h
echo   BuildGen Generated Header >> ntverkp.h
echo   ntverkp.h - Version File >> ntverkp.h
echo   Generated at: %date% %time% >> ntverkp.h
echo         at lab: %lhBuildLab%(%USERNAME%) >> ntverkp.h
echo   for Nt Project: %ProjectName% >> ntverkp.h
echo */ >> ntverkp.h
echo #define HIROSHIMA_BUILD_NUMBER %lhBuild% >> ntverkp.h
echo #define HIROSHIMA_MAJOR %lhMajorVersion% >> ntverkp.h
echo #define HIROSHIMA_MINOR %lhMinorVersion% >> ntverkp.h
echo #define HIROSHIMA_TAG "%lhBuildTag%" >> ntverkp.h
echo #define HIROSHIMA_CPUS 256 >> ntverkp.h
echo #define HIROSHIMA_FULLVER "%lhMajorVersion%.%lhMinorVersion%.%lhBuildTag%" >> ntverkp.h
echo #define HIROSHIMA_REVISION %lhRevision% >> ntverkp.h
rem title [Finished] Windows NT Build Environment - Kernel
REM
REM Touch initos.c + recompile
REM
cd ..\..\..
set NTMAKEENV=%cd%\tools\%wrkarch%
build.exe -ZgPw
title [Publishing Bins] Windows NT Build Environment - Kernel
echo BINPLACE: BinPlacing bins in %_Nt386Tree% ... Please Wait, Copying Files.
rem copy .\base\ntos\build\exe\ntkrnlmp.exe %_Nt386Tree%\ntkrnlmp_%lhBuildTag%.exe
rem copy .\base\ntos\build\exe\ntkrnlmp.pdb %_Nt386Tree%\ntkrnlmp_%lhBuildTag%.pdb
title [Finished] Windows NT Build Environment - Kernel

