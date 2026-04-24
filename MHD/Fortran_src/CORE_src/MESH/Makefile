#
#
#

MESHDIR = $$(CORE_SRCDIR)/MESH
SOURCES = $(shell ls *.f90)
MOD_MESH = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESHDIR = $(MESHDIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MESH)' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$(MOD_MESH)' >> $(MAKENAME)
	@echo  'MOD_MESH= '  \\ >> $(MAKENAME)
	@echo  '$(MOD_MESH)'    >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MESHDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
