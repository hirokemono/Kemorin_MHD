#
#
#

PLANE_IO_DIR = $$(PLANE_SRCDIR)/IO
SRC_PLANE_IO = $(shell ls *.f90)
MOD_PLANE_IO = $(addsuffix .o,$(basename $(SRC_PLANE_IO)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'PLANE_IO_DIR = $(PLANE_IO_DIR)' >> $(MAKENAME)

dir_list:

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_PLANE_IO)' >> $(MAKENAME)

mod_list:
	@echo  MOD_PLANE_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_PLANE_IO) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PLANE_IO_DIR)' $(SRC_PLANE_IO)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
