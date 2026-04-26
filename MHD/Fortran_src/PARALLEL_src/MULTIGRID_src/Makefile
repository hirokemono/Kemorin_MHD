#
#
#

MG_SRCDIR = $$(PARA_SRCDIR)/MULTIGRID_src
SOURCES = $(shell ls *.f90)

MOD_MULTIGRID = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MULTIGRID'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MG_SRCDIR = $(MG_SRCDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MULTIGRID)'    >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MG_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
