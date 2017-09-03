@echo OFF
@echo Adding source files to ROM...
tools\padbin 0x100 PF.gba
tools\gbfs boot.gbfs forth\*
copy /B PF.gba + boot.gbfs PFdemo.gba
@echo Done! [ PFdemo.gba ] created.
VisualBoyAdvance-SDL PFdemo.gba
