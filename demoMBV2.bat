@echo OFF
@echo Adding source files to ROM...
tools\padbin 0x100 PFmbv2_mb.gba
tools\gbfs boot.gbfs forth\*
copy /B PFmbv2_mb.gba + boot.gbfs PFdemoMBV2.mb
@echo Done! [ PFdemoMBV2.mb ] created.
mb.exe -s PFdemoMBV2.mb -c -w 50 -x 255 -m
