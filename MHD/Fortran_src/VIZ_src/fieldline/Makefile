#
#
#

FIELDLINE_DIR = $$(VIZ_SRCDIR)/fieldline
SRC_FIELDLINE = $(shell ls *.f90)
MOD_FIELDLINE = $(addsuffix .o,$(basename $(SRC_FIELDLINE)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'FIELDLINE_DIR =   $(FIELDLINE_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_FIELDLINE)' \
	>> $(MAKENAME)



mod_list:
	@echo  MOD_FIELDLINE=  \\ >> $(MAKENAME)
	@echo  $(MOD_FIELDLINE) >> $(MAKENAME)
	@echo  >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FIELDLINE_DIR)' $(SRC_FIELDLINE)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
