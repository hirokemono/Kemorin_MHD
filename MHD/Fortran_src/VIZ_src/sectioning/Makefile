#
#
#

PSF_SECTION_DIR = $$(VIZ_SRCDIR)/sectioning
SRC_PSF_RENDER = $(shell ls *.f90)
MOD_PSF_RENDER = $(addsuffix .o,$(basename $(SRC_PSF_RENDER)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'PSF_SECTION_DIR = $(PSF_SECTION_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_PSF_RENDER)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_PSF_RENDER=  \\ >> $(MAKENAME)
	@echo  $(MOD_PSF_RENDER) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PSF_SECTION_DIR)' $(SRC_PSF_RENDER)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
