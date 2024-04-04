#
#
#

MHD_RHS_DIR = $$(MHD_SRCDIR)/fem_mhd_integration
SOURCES = $(shell ls *.f90)
MOD_RHS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_RHS_DIR = $(MHD_RHS_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_RHS)'  \
	>> $(MAKENAME)

mod_list:
	@echo MOD_RHS=  \\ >> $(MAKENAME)
	@echo $(MOD_RHS) >> $(MAKENAME)
	@echo '#'>> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_RHS_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
