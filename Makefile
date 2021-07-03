#---------------------------------------------------------------------------------
.SUFFIXES:
#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
# TARGET is the name of the output, if this ends with _mb generates a multiboot image
# BUILD is the directory where object files & intermediate files will be placed
# SOURCES is a list of directories containing source code
# INCLUDES is a list of directories containing extra header files
#---------------------------------------------------------------------------------
TARGET		:=	rath
BUILD		  :=	build
SOURCES		:=	source
INCLUDES	:=	include

RATH_HOME := $${HOME}/code/rath
ASSETS		:= $(RATH_HOME)/assets
AAS_HOME 	:= $(RATH_HOME)/deps/apex-audio-system
AAS_BUILD := $(AAS_HOME)/build
UART_HOME	:= $(RATH_HOME)/deps/gba-serial-adventures
UART_LIB	:= $(UART_HOME)/build/libuart

#---------------------------------------------------------------------------------
# Link Mode : NONE, MBV2, XBOO, UART
#---------------------------------------------------------------------------------

LINKMODE	:=	UART

#---------------------------------------------------------------------------------
# options for code generation
#---------------------------------------------------------------------------------
ARCH	:=	-marm -mthumb-interwork -mlong-calls

CFLAGS	:=	-Wall -O3\
			-mcpu=arm7tdmi -mtune=arm7tdmi\
			-fomit-frame-pointer\
			-ffast-math \
			$(ARCH)

CFLAGS	+=	$(INCLUDE) -DLINK_$(LINKMODE) -DTARGET_$(TARGET)

AFLAGS	:=	$(ARCH)
LDFLAGS	=	$(ARCH) -Wl,-Map,$(notdir $@).map

#---------------------------------------------------------------------------------
# path to tools - this can be deleted if you set the path in windows
#---------------------------------------------------------------------------------
# export PATH		:=	/c/devkitARM_r11/bin:/bin:/c/bin

CONV2AAS := $(AAS_BUILD)/conv2aas/conv2aas
FCOMP := $(RATH_HOME)/tools/compiler.py
TILED2BIN := $(RATH_HOME)/tools/tiled2bin.py

#---------------------------------------------------------------------------------
# absolute path required since this makefile uses the build directory
# as the working directory
#---------------------------------------------------------------------------------
TONCLIB		:= $(DEVKITARM)/../libtonc
AAS       := $(AAS_BUILD)/aas
LIBUART   := $(UART_LIB)

#---------------------------------------------------------------------------------
# the prefix on the compiler executables
#---------------------------------------------------------------------------------
PREFIX			:=	arm-none-eabi-
#---------------------------------------------------------------------------------
# any extra libraries we wish to link with the project
#---------------------------------------------------------------------------------
LIBS	:=	-ltonc -lAAS -luart

#---------------------------------------------------------------------------------
# list of directories containing libraries, this must be the top level containing
# include and lib
#---------------------------------------------------------------------------------
LIBDIRS	:= $(TONCLIB) $(AAS) $(LIBUART)

#---------------------------------------------------------------------------------
# no real need to edit anything past this point unless you need to add additional
# rules for different file extensions
#---------------------------------------------------------------------------------
ifneq ($(BUILD),$(notdir $(CURDIR)))
#---------------------------------------------------------------------------------

export OUTPUT	:=	$(CURDIR)/$(TARGET)

export VPATH	:=	$(foreach dir,$(SOURCES),$(CURDIR)/$(dir))

export CC		:=	$(PREFIX)gcc
export CXX		:=	$(PREFIX)g++
export AR		:=	$(PREFIX)ar
export OBJCOPY	:=	$(PREFIX)objcopy
#---------------------------------------------------------------------------------
# use CXX for linking C++ projects, CC for standard C
#---------------------------------------------------------------------------------
#export LD		:=	$(CXX)
export LD		:=	$(CC)

CFILES		:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.c)))
CPPFILES	:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.cpp)))
SFILES		:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.s)))
PCXFILES	:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.pcx)))
BINFILES	:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.bin)))

ifneq ("$(wildcard source/AAS_Data.s)","")
SFILES := $(SFILES)
else
SFILES := $(SFILES) AAS_Data.s
endif

export OFILES	:=	$(BINFILES:.bin=.o) $(PCXFILES:.pcx=.o)\
					$(CPPFILES:.cpp=.o) $(CFILES:.c=.o) $(SFILES:.s=.o)

export INCLUDE	:=	$(foreach dir,$(INCLUDES),-I$(CURDIR)/$(dir)) \
					$(foreach dir,$(LIBDIRS),-I$(dir)/include) \
					-I$(CURDIR)/$(BUILD)

export LIBPATHS	:=	$(foreach dir,$(LIBDIRS),-L$(dir)/lib)

.PHONY: $(BUILD) clean

#---------------------------------------------------------------------------------
$(BUILD): music
	@[ -d $@ ] || mkdir -p $@
	@make --no-print-directory -C $(AAS_HOME)
	@make --no-print-directory -C $(UART_HOME)
	@make --no-print-directory -C $(BUILD) -f $(CURDIR)/Makefile

#---------------------------------------------------------------------------------
clean:
	@echo clean ...
	@rm -fr $(BUILD) *.elf source/AAS_Data*

#---------------------------------------------------------------------------------
music:
	$(CONV2AAS) $(RATH_HOME)/AAS_Data
	mv AAS_Data.* $(RATH_HOME)/source

#---------------------------------------------------------------------------------
else

DEPENDS	:=	$(OFILES:.o=.d)

#---------------------------------------------------------------------------------
# main targets
#---------------------------------------------------------------------------------
$(OUTPUT).gba	: $(OUTPUT).elf

$(OUTPUT).elf	: $(OFILES)

#---------------------------------------------------------------------------------
%.gba: %.elf
	@echo built ... $(notdir $@)
	@$(OBJCOPY) -O binary $< $@
	@gbafix $@

#---------------------------------------------------------------------------------
%_mb.elf:
	@echo linking multiboot
	@$(LD) -specs=gba_mb.specs $(LDFLAGS) $(OFILES) $(LIBPATHS) $(LIBS) -o $@

#---------------------------------------------------------------------------------
%.elf: ass
	@echo linking cartridge
	@$(LD) $(LDFLAGS) -specs=gba.specs $(OFILES) $(LIBPATHS) $(LIBS) -o $@

#---------------------------------------------------------------------------------
ass:
	$(FCOMP) $(RATH_HOME)/forth/to-asm/d-lib-constants.fth -o ass.asm
	$(TILED2BIN) $(ASSETS)/apartment-map.json -o apt-toi.bin # things of interest
	convert $(ASSETS)/apartment-map.png apartment-map.png
	convert $(ASSETS)/phone.png phone.png
	grit apartment-map.png phone.png -ftb -mR8 -mLs -pS -O shared
	grit $(ASSETS)/snaggle.png -ftb -gB8 -gT 000000 -Mw 2 -Mh 4
	grit $(ASSETS)/splash.png -gb -gB16 -ftb
	grit $(ASSETS)/end.png -gb -gB16 -ftb

#---------------------------------------------------------------------------------
# Compile Targets for C/C++
#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
%.o : %.cpp
	@echo $(notdir $<)
	@$(CXX) -MM $(CFLAGS) -o $*.d $<
	@$(CXX)  $(CFLAGS) -c $< -o$@

#---------------------------------------------------------------------------------
%.o : %.c
	@echo $(notdir $<)
	@$(CC) -MM $(CFLAGS) -o $*.d $<
	@$(CC)  $(CFLAGS) -c $< -o$@

#---------------------------------------------------------------------------------
%.o : %.s
	@echo $(notdir $<)
	@$(CC) -MM $(CFLAGS) -o $*.d $<
	@$(CC)  $(ASFLAGS) -c $< -o$@

define bin2o
	cp $(<) $(*).tmp
	$(OBJCOPY) -I binary -O elf32-littlearm -B arm \
	--rename-section .data=.rodata,readonly,data,contents \
	--redefine-sym _binary_$*_tmp_start=$*\
	--redefine-sym _binary_$*_tmp_end=$*_end\
	--redefine-sym _binary_$*_tmp_size=$*_size\
	$(*).tmp $(@)
	echo "extern const u8" $(*)"[];" > $(*).h
	echo "extern const u32" $(*)_size[]";" >> $(*).h
	rm $(*).tmp
endef

#---------------------------------------------------------------------------------
%.o	:	%.pcx
#---------------------------------------------------------------------------------
	@echo $(notdir $<)
	@$(bin2o)

#---------------------------------------------------------------------------------
%.o	:	%.bin
#---------------------------------------------------------------------------------
	@echo $(notdir $<)
	@$(bin2o)

-include $(DEPENDS)

#---------------------------------------------------------------------------------------
endif
#---------------------------------------------------------------------------------------
