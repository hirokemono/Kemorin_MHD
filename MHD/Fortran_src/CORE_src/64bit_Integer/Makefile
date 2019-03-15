#
#
#

INT64DIR = $$(CORE_SRCDIR)/64bit_Integer
SRC_INT64 = $(shell ls *.f90)
MOD_INT64 = $(addsuffix .o,$(basename $(SRC_INT64)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INT64DIR = $(INT64DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_INT64)' >> $(MAKENAME)


mod_list:
	@echo  MOD_INT64=  \\ >> $(MAKENAME)
	@echo  $(MOD_INT64) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(INT64DIR)' $(SRC_INT64)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
