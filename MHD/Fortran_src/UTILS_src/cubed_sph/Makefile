#
#
#

CUBED_SPH_DIR =   $$(UTILS_SRCDIR)/cubed_sph

LIB_CUBED_SPH =       -lcubed_sph_kemo
LIB_CUBED_SPH_FILE = libcubed_sph_kemo.a

SOURCES = $(shell ls *.f90)
MOD_CUBED_SPH = $(addsuffix .o,$(basename $(SOURCES)) )

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
	@echo  '$$(LIB_CUBED_SPH_FILE): $$(MOD_CUBED_SPH)' >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_CUBED_SPH)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_CUBED_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUBED_SPH)   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(CUBED_SPH_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod $(EXE) $(LIB) *~

