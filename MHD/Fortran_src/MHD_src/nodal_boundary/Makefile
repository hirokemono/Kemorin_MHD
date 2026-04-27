#
#
#

MHD_NOD_BC_DIR = $$(MHD_SRCDIR)/nodal_boundary
SOURCES = $(shell ls *.f90)

MOD_NOD_BD = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_NOD_BD'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_NOD_BC_DIR = $(MHD_NOD_BC_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_NOD_BD)'    >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_NOD_BC_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
