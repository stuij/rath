echo Adding source files to ROM...
tools/padbin 0x100 PF.gba
tools/gbfs boot.gbfs forth/a_gba.pf forth/z_demo.pf
cat PF.gba boot.gbfs > PFyes.gba
echo Done! [ PFyes.gba ] created.
#mgba-qt PFyes.gba
