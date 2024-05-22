#
#
#

CURRENTDIR = $(shell pwd)
ISPACK3_DIR =     $$(EXT_DIR)/ISPACK3_custum
FXPACK3_DIR = $$(ISPACK3_DIR)/ispack-3.0.1/fxpack/src
MXPACK3_DIR = $$(ISPACK3_DIR)/ispack-3.0.1/mxpack/src

LIB_ISPACK3 =       -lispack3_fft
LIB_ISPACK3_FILE =   libispack3_fft.a

SRC_FXPACK3_BASE = $(notdir $(shell ls ispack-3.0.1/fxpack/src/??????.f90)) \
                   fxr.f90 fxz.f90
SRC_FXPACK3_SSE4 = $(notdir $(shell ls ispack-3.0.1/fxpack/src/*-$(SSE4).*))
SRC_FXPACK3_SSE8 = $(notdir $(shell ls ispack-3.0.1/fxpack/src/*-$(SSE8).*))

SRC_MXPACK3_C =    $(notdir $(shell ls ispack-3.0.1/mxpack/src/??????.c))
SRC_MXPACK3_F =    $(notdir $(shell ls ispack-3.0.1/mxpack/src/??????.f90 ispack-3.0.1/mxpack/src/*-$(SSE_CPU).f90))
SRC_MXPACK3_SSE4 = $(notdir $(shell ls ispack-3.0.1/mxpack/src/*-$(SSE4).*))
SRC_MXPACK3_SSE8 = $(notdir $(shell ls ispack-3.0.1/mxpack/src/*-$(SSE8).*))

MOD_ISPACK3 = \
fxz.o \
fxr.o \
$(addsuffix .o,$(basename $(SRC_FXPACK3_BASE))) \
$(addsuffix .o,$(basename $(SRC_FXPACK3_SSE4))) \
$(addsuffix .o,$(basename $(SRC_FXPACK3_SSE8))) \
\
$(addsuffix .o,$(basename $(SRC_MXPACK3_C))) \
$(addsuffix .o,$(basename $(SRC_MXPACK3_F))) \
$(addsuffix .o,$(basename $(SRC_MXPACK3_SSE4))) \
$(addsuffix .o,$(basename $(SRC_MXPACK3_SSE8)))


F90EXT = '.f90'
ASMEXT = '.s'

F90BUILD = '$$(F90) -c $$(F90OPTFLAGS) -fno-range-check $$<'
ASMBUILD = '$$(AS) -c -x assembler $$< -o $$@'

ifeq ($(SSE4),fort)
  EXT4 = $(F90EXT)
  BUILD4 = $(F90BUILD)
else
  EXT4 = $(ASMEXT)
  BUILD4 = $(ASMBUILD)
endif

ifeq ($(SSE8),fort512)
  EXT8 = $(F90EXT)
  BUILD8 = $(F90BUILD)
else
  EXT8 = $(ASMEXT)
  BUILD8 = $(ASMBUILD)
endif

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'ISPACK3_DIR = $(ISPACK3_DIR)' >> $(MAKENAME)
	@echo 'FXPACK3_DIR = $(FXPACK3_DIR)' >> $(MAKENAME)
	@echo 'MXPACK3_DIR = $(MXPACK3_DIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_ISPACK3 =      $(LIB_ISPACK3)'      >> $(MAKENAME)
	@echo  'LIB_ISPACK3_FILE = $(LIB_ISPACK3_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_ISPACK3_FILE): $$(MOD_ISPACK3)'>> $(MAKENAME)

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ISPACK3)' >> $(MAKENAME)

mod_list:
	@echo  MOD_ISPACK3=  \\ >> $(MAKENAME)
	@echo  $(MOD_ISPACK3) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	echo 'm_ISPACK3_constants.o: $$(FXPACK3_DIR)/m_ISPACK3_constants.f90' >> $(MAKENAME); \
	echo '	'$(F90BUILD) >> $(MAKENAME)
	
	@for src in $(basename $(SRC_FXPACK3_BASE)) ; do \
		( \
		echo $$src'.o: $$(FXPACK3_DIR)/'$$src'.f90 m_ISPACK3_constants.o' >> $(MAKENAME); \
		echo '	'$(F90BUILD) >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	@for src in $(basename $(SRC_FXPACK3_SSE4)) ; do \
		( \
		echo $$src'.o: $$(FXPACK3_DIR)/'$$src'$(EXT4)' >> $(MAKENAME); \
		echo '	'$(BUILD4) >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	@for src in $(basename $(SRC_FXPACK3_SSE8)) ; do \
		( \
		echo $$src'.o: $$(FXPACK3_DIR)/'$$src'$(EXT8)' >> $(MAKENAME); \
		echo '	'$(BUILD8) >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	
	@for src in $(basename $(SRC_MXPACK3_C)) ; do \
		( \
		echo $$src'.o: $$(MXPACK3_DIR)/'$$src'.c' >> $(MAKENAME); \
		echo '	''$$(CC) -c $$(OPTFLAGS) $$<' >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	@for src in $(basename $(SRC_MXPACK3_F)) ; do \
		( \
		echo $$src'.o: $$(MXPACK3_DIR)/'$$src'.f90' >> $(MAKENAME); \
		echo '	'$(F90BUILD) >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	@for src in $(basename $(SRC_MXPACK3_SSE4)) ; do \
		( \
		echo $$src'.o: $$(MXPACK3_DIR)/'$$src'.f90' >> $(MAKENAME); \
		echo '	'$(BUILD) >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	@for src in $(basename $(SRC_MXPACK3_SSE8)) ; do \
		( \
		echo $$src'.o: $$(MXPACK3_DIR)/'$$src'.f90' >> $(MAKENAME); \
		echo '	'$(BUILD) >> $(MAKENAME); \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

depends:

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
