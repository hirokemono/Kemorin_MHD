#
#
#

MHD_SGS_BC_DIR = $$(MHD_SRCDIR)/sgs_model_bc
SRC_SGS_BC = $(shell ls *.f90)
MOD_SGS_BC = $(addsuffix .o,$(basename $(SRC_SGS_BC)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SGS_BC_DIR = $(MHD_SGS_BC_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SGS_BC)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SGS_BC=  \\ >> $(MAKENAME)
	@echo $(MOD_SGS_BC) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_SGS_BC_DIR)' $(SRC_SGS_BC)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
