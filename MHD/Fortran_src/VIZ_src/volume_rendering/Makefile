#
#
#

PVR_DIR = $$(VIZ_SRCDIR)/volume_rendering
SRC_PVR = $(shell ls *.f90)
MOD_PVR = $(addsuffix .o,$(basename $(SRC_PVR)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'PVR_DIR = $(PVR_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_PVR)' \
	>> $(MAKENAME)



mod_list:
	@echo  MOD_PVR=  \\ >> $(MAKENAME)
	@echo  $(MOD_PVR) >> $(MAKENAME)
	@echo  >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PVR_DIR)' $(SRC_PVR)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
