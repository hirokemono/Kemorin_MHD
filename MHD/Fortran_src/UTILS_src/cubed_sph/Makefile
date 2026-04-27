#
#
#

CUBED_SPH_DIR =   $$(UTILS_SRCDIR)/cubed_sph

LIB_CUBED_SPH =       -lcubed_sph_kemo
LIB_CUBED_SPH_FILE = libcubed_sph_kemo.a

SOURCES = $(shell ls *.f90)
MOD_CUBED_SPH = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_CUBED_SPH'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CUBED_SPH_DIR = $(CUBED_SPH_DIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_CUBED_SPH =      $(LIB_CUBED_SPH)'      >> $(MAKENAME)
	@echo  'LIB_CUBED_SPH_FILE = $(LIB_CUBED_SPH_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_CUBED_SPH_FILE): $$('$(MODULE_LIST)')' >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_CUBED_SPH)'     >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(CUBED_SPH_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod $(EXE) $(LIB) *~

