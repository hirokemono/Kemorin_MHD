#
#
#

MHD_SF_BC_DIR = $$(MHD_SRCDIR)/surface_boundary
SOURCES = $(shell ls *.f90)
MOD_SF_BD = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SF_BC_DIR = $(MHD_SF_BC_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SF_BD)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SF_BD=  \\ >> $(MAKENAME)
	@echo $(MOD_SF_BD) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_SF_BC_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
