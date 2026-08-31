#
#
#

FFT_UTIL_DIR = $$(UTILS_SRCDIR)/FFT_utils
SOURCES = $(shell ls *.f90 *.F90)

MOD_FFT_UTILS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_FFT_UTILS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_UTIL_DIR = $(FFT_UTIL_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB)' '$$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_FFT_UTILS)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_UTIL_DIR)' $(SOURCES)

