:START
	@echo off
	IF DEFINED wrkarch goto finish
	SET wrkarch=x86
	IF NOT [%1] EQU [] SET wrkarch=%1

	if "%PROCESSOR_ARCHITECTURE%" == "x86"   set PLAT_USERNAME=x86fre
	if "%PROCESSOR_ARCHITECTURE%" == "AMD64"   set PLAT_USERNAME=amd64fre
	if "%PROCESSOR_ARCHITECTURE%" == "x86"   set LOGNAME=HX86FIX
	if "%PROCESSOR_ARCHITECTURE%" == "AMD64"   set LOGNAME=HAMD64FIX

	SET path=%cd%\tools\%wrkarch%;%path%;C:\Program Files\Debugging Tools for Windows
	SET windbgargs=-k com:pipe,port=\\.\pipe\debug,resets=0,reconnect
	SET _NT_SYMBOL_PATH="%cd%\base\ntos\build\exe;%cd%\WS03SP1HALS\x86\halacpim"
	SET _NTDRIVE=W:

	if "%PROCESSOR_ARCHITECTURE%" == "x86"   set _nt386boot=%_ntdrive%\binaries
	if "%PROCESSOR_ARCHITECTURE%" == "AMD64"   set _ntamd64boot=%_ntdrive%\binaries
	
	@rem
	@rem If no directory has been specified for the NT development tree, assume
	@rem \nt.  To override this, place a SET _NTROOT=\nt in your CONFIG.SYS
	@rem
	set _NT_DRIVE=%cd:~0,2%
	set _NTROOT=%cd:~2%
	set _NTBINDIR=%cd%
	set BASEDIR=%cd%

	@rem
	@rem This command file assumes that the developer has already defined
	@rem the USERNAME environment variable to match their email name (e.g.
	@rem stevewo).
	@rem
	@rem We want to remember some environment variables so we can restore later
	@rem if necessary (see NTUSER.CMD)
	@rem
	set _NTUSER=%USERNAME%

	@rem
	@rem No hidden semantics of where to get libraries and include files.  All
	@rem information is included in the command lines that invoke the compilers
	@rem and linkers.
	@rem
	set LIB=
	set INCLUDE=

	@rem
	@rem Setup default build parameters.
	@rem
	set BUILD_DEFAULT=ntoskrnl ntkrnlmp daytona -e -i -nmake -i
	set BUILD_DEFAULT_TARGETS=-386
	set BUILD_MAKE_PROGRAM=nmake.exe
	set MASTER_VERSION_FILE=%cd%\public\sdk\inc\ntverkp.h
	set _Nt386Tree=B:\Bins.x86fre

	@rem
	@rem Setup default nmake parameters.
	@rem
:finish
        TITLE [Ready] RAZZLE - Windows Build Environment

	if "%NTMAKEENV%" == "" set NTMAKEENV=%_NTBINDIR%\TOOLS\%wrkarch%

	cls

	color 0E

	alias -f %_NTBINDIR%\cue.pub

	cls

	set PLAT_USERNAME=x86fre

	echo *******************************************************************************
	echo *                                                                             *
	echo *                           NtosBuild (Razzle)                                *
	echo *                                                                             *
	echo *******************************************************************************
	echo [PUBLIC Path: %BASEDIR%\public]
	echo [SRC Path: %CD%]
	echo [Drive Letter: %_NT_DRIVE%]
	echo [Base Folder: %_NTROOT%]
	echo [Release Folder: %_NT386TREE%]
	echo [Build Arch: %PLAT_USERNAME%]
	echo [Alias File: %CD%\cue.pub]
	echo Build Environment is configured.
	echo Commands:
	echo    make          - Builds the tree.
	echo    "module"      - Builds "module" (replace by rtl, vdm, vf, etc...)
	echo    release       - Builds the release directory 
	echo    clean         - Cleans up the tree.
	echo    up            - Goes up one directory.
	echo    ...           - Goes up two directories.
	echo    ....          - Goes up three directories.
:finish2

