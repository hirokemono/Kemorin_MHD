#
#
#

MG_SRCDIR = $$(PARA_SRCDIR)/MULTIGRID_src
SRC_MULTIGRID = $(shell ls *.f90)
MOD_MULTIGRID = $(addsuffix .o,$(basename $(SRC_MULTIGRID)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MG_SRCDIR = $(MG_SRCDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MULTIGRID)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MULTIGRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_MULTIGRID) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MG_SRCDIR)' $(SRC_MULTIGRID)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
