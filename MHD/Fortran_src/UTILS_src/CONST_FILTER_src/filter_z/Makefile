#
#
#

FILTER_Z_SRCDIR =  $$(FILTER_SRCDIR)/filter_z
SRC_Z_COMMUTE = $(shell ls *.f90)
MOD_Z_COMMUTE = $(addsuffix .o,$(basename $(SRC_Z_COMMUTE)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_Z_SRCDIR =  $(FILTER_Z_SRCDIR)'    >> $(MAKENAME)

lib_name:

lib_tasks:
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_Z_COMMUTE)' \
	>> $(MAKENAME)


mod_list:
	@echo 'MOD_Z_COMMUTE = $(MOD_Z_COMMUTE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FILTER_Z_SRCDIR)' $(SRC_Z_COMMUTE)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
