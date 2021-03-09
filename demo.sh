echo Adding source files to ROM...
padbin 0x100 PF.gba
gbfs boot.gbfs forth/demo/*
cat PF.gba boot.gbfs > PFdemo.gba
echo Done! [ PFdemo.gba ] created.
# mgba-qt PFdemo.gba
