#
#
#

MHD_SOLVER_DIR = $$(MHD_SRCDIR)/solver_vector_smp
SOURCES = $(shell ls *.f90)
MOD_MHD_SOLVE = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SOLVER_DIR = $(MHD_SOLVER_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_MHD_SOLVE)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_MHD_SOLVE=  \\ >> $(MAKENAME)
	@echo $(MOD_MHD_SOLVE) >> $(MAKENAME)
	@echo '#'>> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_SOLVER_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
	rm -f */*.o */*.mod */*~ */*.par */*.diag */*.a
