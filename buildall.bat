@echo OFF
@echo Building CARTRIDGE - NO CABLE SUPPORT
make clean
make -r TARGET=PF LINKMODE=NONE
@echo Done [ PF.gba ]
@echo Building MULTIBOOT - MBV2
make clean
make -r TARGET=PFmbv2_mb LINKMODE=MBV2
@echo Done [ PFmbv2_mb.gba ]
make clean
