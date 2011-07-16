    DEVLOAD 3.20 USER GUIDE
    -----------------------

    DEVLOAD is an utility for loading device drivers from command line.
It supports FAT16 & FAT32 block device drivers   (e.g. DI1000DD.SYS and
ASPIDISK.SYS),  COM & EXE  -  style  DOS  device drivers and has a very
compact binary file (less than 3Kb). 

    Usage
    -----
    Usage:    DEVLOAD [switches] filename [params]
    Emulates: DEVICE=filename [params] in CONFIG.SYS

    Switches:
      /? - display help message.
      /H - try to load driver to UMB.
           (equivalent of DEVICEHIGH=filename [params] in CONFIG.SYS)
      /Q - quiet mode.
      /V - verbose mode.
      /A - auto-mode (force to stay loaded).

    Examples:
      1) USBASPI from command line
         DEVLOAD /H USBASPI.SYS /V /W
         DEVLOAD /H DI1000DD.SYS

      2) ASPI driver for IDE (e.g. for CDRTOOLS)
         DEVLOAD ASPI.SYS

    System requirements
    -------------------
    * DOS 3.00 and higher
    * Intel 8086 CPU, 640Kb of RAM
    * MS-DOS 7.1/8.0, PC-DOS 7.1, LZ-DOS 7.1, 
      FreeDOS 1.0, EDR-DOS WIP 17.6.2007+
      for FAT32 block device drivers support
    
    WARNING: DEVLOAD cannot load FAT32 block device drivers under PTS-DOS 32!   

    Files
    -----
    DEVLOAD.COM  Binary file
    DEVLOAD.ASM  Source code for TASM and ArrowASM
    FMAKE.BAT    "Make" script for ArrowASM/WarpLink/UPX
    MAKE.BAT     "Make" script for TASM/TLINK/UPX
    README.TXT   This file

    Compilation
    -----------
    If you have TASM, TLINK and UPX, run MAKE.BAT.
    If you have ArrowASM, WarpLink and UPX, run FMAKE.BAT.

    ArrowASM: http://www.programmersheaven.com/download/1340/download.aspx
    WarpLink: http://www.devoresoftware.com/freesource/wlsrc.htm     
    UPX:      http://upx.sourceforge.net/
