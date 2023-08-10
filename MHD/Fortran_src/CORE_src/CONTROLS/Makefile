#
#
#

CTL_DIR = $$(CORE_SRCDIR)/CONTROLS
SOURCES = $(shell ls *.f90 *.F90)
MOD_CTL = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CTL_DIR = $(CTL_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_CTL)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_CTL=  \\ >> $(MAKENAME)
	@echo  $(MOD_CTL) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(CTL_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
