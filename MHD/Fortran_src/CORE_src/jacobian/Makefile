#
#
#

JACOBIDIR = $$(CORE_SRCDIR)/jacobian

SOURCES = $(shell ls *.f90)
MOD_JACOBI = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'JACOBIDIR = $(JACOBIDIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_JACOBI)' >> $(MAKENAME)

mod_list:
	@echo MOD_JACOBI=  \\ >> $(MAKENAME)
	@echo $(MOD_JACOBI) >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(JACOBIDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
