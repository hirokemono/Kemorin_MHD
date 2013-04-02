#
#
#

FILTER_3D_MATDIR = $$(FILTER_SRCDIR)/filter_3d
SRC_3D_FILTER_MAT = $(shell ls *.f90)
MOD_3D_FILTER_MAT = $(addsuffix .o,$(basename $(SRC_3D_FILTER_MAT)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_3D_MATDIR = $(FILTER_3D_MATDIR)'   >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_3D_FILTER_MAT)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_3D_FILTER_MAT=  \\ >> $(MAKENAME)
	@echo  $(MOD_3D_FILTER_MAT) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FILTER_3D_MATDIR)' $(SRC_3D_FILTER_MAT)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
