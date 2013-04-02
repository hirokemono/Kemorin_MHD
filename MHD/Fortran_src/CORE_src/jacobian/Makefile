#
#
#

JACOBIDIR = $$(CORE_SRCDIR)/jacobian

SRC_JACOBI = $(shell ls *.f90)
MOD_JACOBI = $(addsuffix .o,$(basename $(SRC_JACOBI)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'JACOBIDIR = $(JACOBIDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_JACOBI)' >> $(MAKENAME)

mod_list:
	@echo MOD_JACOBI=  \\ >> $(MAKENAME)
	@echo $(MOD_JACOBI) >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(JACOBIDIR)' $(SRC_JACOBI)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
