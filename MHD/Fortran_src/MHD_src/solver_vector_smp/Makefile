#
#
#

MHD_SOLVER_DIR = $$(MHD_SRCDIR)/solver_vector_smp
SRC_MHD_SOLVE = $(shell ls *.f90)
MOD_MHD_SOLVE = $(addsuffix .o,$(basename $(SRC_MHD_SOLVE)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SOLVER_DIR = $(MHD_SOLVER_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_MHD_SOLVE)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_MHD_SOLVE=  \\ >> $(MAKENAME)
	@echo $(MOD_MHD_SOLVE) >> $(MAKENAME)
	@echo '#'>> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_SOLVER_DIR)' $(SRC_MHD_SOLVE)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
	rm -f */*.o */*.mod */*~ */*.par */*.diag */*.a
