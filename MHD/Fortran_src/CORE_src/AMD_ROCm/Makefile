#
#
#

AMD_ROCM_DIR = $$(CORE_SRCDIR)/AMD_ROCm

LIB_CALYPSO_ROCM =       -lkemo_ROCm
LIB_CALYPSO_ROCM_FILE =  liblkemo_ROCm.a

SOURCES = $(shell ls *.f90 *.F90)
MOD_AMD_ROCM = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'AMD_ROCM_DIR = $(AMD_ROCM_DIR)'  >> $(MAKENAME)

lib_name:
	@echo  'LIB_CALYPSO_ROCM =      $(LIB_CALYPSO_ROCM)'      >> $(MAKENAME)
	@echo  'LIB_CALYPSO_ROCM_FILE = $(LIB_CALYPSO_ROCM_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_AMD_ROCM)' >> $(MAKENAME)


mod_list:
	@echo  MOD_AMD_ROCM=  \\ >> $(MAKENAME)
	@echo  $(MOD_AMD_ROCM)   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(AMD_ROCM_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
