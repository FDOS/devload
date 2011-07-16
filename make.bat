@ECHO OFF
rem if you have tasm: tasm  devload.asm
rem asm is ArrowASM - freeware
asm devload.asm devload.obj nul.lst nul.crf
rem tlink is part of Turbo C 2.01 - freeware
tlink devload.obj /t
rem upx is and open source binary packer
upx --8086 --best devload.com
