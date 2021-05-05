@echo OFF
@echo Adding source files to ROM...
padbin 0x100 rath.gba
gbfs boot.gbfs forth\lib\*
copy /B rath.gba + boot.gbfs rath-demo.gba
@echo Done! [ rath-demo.gba ] created.
mgba rath-demo.gba
