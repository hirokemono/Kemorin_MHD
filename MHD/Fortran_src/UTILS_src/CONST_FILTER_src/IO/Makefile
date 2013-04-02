#
#
#

FILTER_3D_IODIR =  $$(FILTER_SRCDIR)/IO
SRC_GEN_FILTER_IO = $(shell ls *.f90)
MOD_GEN_FILTER_IO = $(addsuffix .o,$(basename $(SRC_GEN_FILTER_IO)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_3D_IODIR =  $(FILTER_3D_IODIR)'          >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_GEN_FILTER_IO)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_GEN_FILTER_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_FILTER_IO) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FILTER_3D_IODIR)' $(SRC_GEN_FILTER_IO)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
