#
#
#

MG_SRCDIR = $$(PARA_SRCDIR)/MULTIGRID_src
SOURCES = $(shell ls *.f90)
MOD_MULTIGRID = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MG_SRCDIR = $(MG_SRCDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MULTIGRID)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MULTIGRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_MULTIGRID) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MG_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
