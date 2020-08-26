#
#
#

DCL_DIR =     $$(EXT_DIR)/DCL_custum
ISPACK_DIR =  $$(DCL_DIR)/ispack-0.931/ftpack/src
ISPACK3_DIR = $$(DCL_DIR)/ispack-3.0.1/fxpack/src
MXPACK3_DIR = $$(DCL_DIR)/ispack-3.0.1/mxpack/src

LIB_ISPACK =       -lispack_fft
LIB_ISPACK_FILE = libispack_fft.a

LIB_ISPACK3 =       -lispack3_fft
LIB_ISPACK3_FILE =   libispack3_fft.a

MOD_ISPACK = \
ftdmsg.o \
ftr.o \
ftz.o \
ftct.o \
ftst.o \
ftcm.o \
ftsm.o

SRC_FXPACK3_BASE = $(notdir $(shell ls ispack-3.0.1/fxpack/src/??????.f90))
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


F90BUILD = '$$(F90) -c $$(F90OPTFLAGS) $$<'
ASMBUILD = '$$(AS) $$< -o $$@'

ifdef SSE_FLAG
BUILD = $(ASMBUILD)
else
BUILD = $(F90BUILD)
endif

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DCL_DIR =     $(DCL_DIR)'    >> $(MAKENAME)
	@echo 'ISPACK_DIR =  $(ISPACK_DIR)' >> $(MAKENAME)
	@echo 'ISPACK3_DIR = $(ISPACK3_DIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_ISPACK =       $(LIB_ISPACK)'       >> $(MAKENAME)
	@echo  'LIB_ISPACK3 =      $(LIB_ISPACK3)'      >> $(MAKENAME)
	@echo  'LIB_ISPACK_FILE =  $(LIB_ISPACK_FILE)'  >> $(MAKENAME)
	@echo  'LIB_ISPACK3_FILE = $(LIB_ISPACK3_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_ISPACK_FILE): $$(MOD_ISPACK)'>> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ISPACK)' >> $(MAKENAME)

mod_list:
	@echo  MOD_ISPACK=  \\ >> $(MAKENAME)
	@echo  $(MOD_ISPACK) >> $(MAKENAME)
	@echo  MOD_ISPACK3=  \\ >> $(MAKENAME)
	@echo  $(MOD_ISPACK3) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@echo 'ftdmsg.o: $$(ISPACK_DIR)/ftdmsg.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftr.o: $$(ISPACK_DIR)/ftr.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftz.o: $$(ISPACK_DIR)/ftz.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftct.o: $$(ISPACK_DIR)/ftct.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftst.o: $$(ISPACK_DIR)/ftst.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftcm.o: $$(ISPACK_DIR)/ftcm.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftsm.o: $$(ISPACK_DIR)/ftsm.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo   >> $(MAKENAME)
	
	@for src in $(basename $(SRC_FXPACK3_BASE)) ; do \
		( \
		echo $$src'.o: $$(ISPACK3_DIR)/'$$src'.f90' >> $(MAKENAME); \
		echo '	'$(F90BUILD) >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	@for src in $(basename $(SRC_FXPACK3_SSE4)) ; do \
		( \
		echo $$src'.o: $$(ISPACK3_DIR)/'$$src'.f90' >> $(MAKENAME); \
		echo '	'$(BUILD) >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	@for src in $(basename $(SRC_FXPACK3_SSE8)) ; do \
		( \
		echo $$src'.o: $$(ISPACK3_DIR)/'$$src'.f90' >> $(MAKENAME); \
		echo '	'$(BUILD) >> $(MAKENAME); \
		 ) \
	done
	@echo   >> $(MAKENAME)
	
	@for src in $(basename $(SRC_MXPACK3_C)) ; do \
		( \
		echo $$src'.o: $$(MXPACK3_DIR)/'$$src'.c' >> $(MAKENAME); \
		echo '	''$$(CC) -c $$(OPTFLAGS)　$$<' >> $(MAKENAME); \
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
