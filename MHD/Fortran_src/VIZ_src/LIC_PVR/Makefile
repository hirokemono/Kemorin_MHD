#
#
#

LIC_DIR = $$(VIZ_SRCDIR)/LIC_PVR
SRC_LIC = $(shell ls *.f90)
MOD_LIC = $(addsuffix .o,$(basename $(SRC_LIC)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'LIC_DIR = $(LIC_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_LIC)' \
	>> $(MAKENAME)



mod_list:
	@echo  MOD_LIC=  \\ >> $(MAKENAME)
	@echo  $(MOD_LIC) >> $(MAKENAME)
	@echo  >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(LIC_DIR)' $(SRC_LIC)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
