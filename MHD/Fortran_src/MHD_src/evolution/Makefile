#
#
#

MHD_EVO_DIR = $$(MHD_SRCDIR)/evolution
SOURCES = $(shell ls *.f90)
MOD_EVOLVE = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_EVO_DIR = $(MHD_EVO_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_EVOLVE)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_EVOLVE=  \\ >> $(MAKENAME)
	@echo  $(MOD_EVOLVE) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_EVO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
