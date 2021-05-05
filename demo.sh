echo Adding source files to ROM...
padbin 0x100 rath.gba
gbfs boot.gbfs forth/lib/*
cat rath.gba boot.gbfs > rath-demo.gba
echo Done! [ rath-demo.gba ] created.
# mgba-qt rath-demo.gba
