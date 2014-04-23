#
#
#

CTL_DIR = $$(CORE_SRCDIR)/CONTROLS
SRC_CTL = $(shell ls *.f90 *.F90)
MOD_CTL = $(addsuffix .o,$(basename $(SRC_CTL)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CTL_DIR = $(CTL_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_CTL)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_CTL=  \\ >> $(MAKENAME)
	@echo  $(MOD_CTL) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CTL_DIR)' $(SRC_CTL)
