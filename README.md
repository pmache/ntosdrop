# ntosdrop

![img](https://i.imgur.com/Ci8GtBI.jpg)

An implementation of NT research kernel, compatibile only with Windows Server 2003 SP1

Made posibble by fellow users of betaarchive forum. Uploaded for archive reason and possible future work.

GNU GPL v3.

### What is NTOSDrop?

Custom WRK kernel w/ lots of extra features:
This is totally portable, you can even run it from a USB drive. ;)

- Speed optimization
- Version customization
- Fixed bugs
- NT Kernel Monitor
- Windows Startup Progress Bar
- Enhanced debugging
- Enhanced Startup
- Hiroshima/xe XBOX application layer (XBOX 1 games).

You just check out WIN_RELEASE, and hit build.cmd, that simple, it uses VC2003 compilers. :P

But the boot.ini thing also works, i recommend personally this:
Code:
multi(0)disk(0)rdisk(0)partition(1)\WINDOWS="NTOSdrop Community Edition" /NoExecute=OptOut /KERNEL=wrkx86.exe /debug /debugport=com1 /baudrate=115200 /ntkm /sos
