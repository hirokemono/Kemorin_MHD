#
#
#

MHD_NOD_BC_DIR = $$(MHD_SRCDIR)/nodal_boundary
SOURCES = $(shell ls *.f90)
MOD_NOD_BD = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_NOD_BC_DIR = $(MHD_NOD_BC_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_NOD_BD)'  \
	>> $(MAKENAME)

mod_list:
	@echo MOD_NOD_BD=  \\ >> $(MAKENAME)
	@echo $(MOD_NOD_BD) >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_NOD_BC_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
