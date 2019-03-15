#
#
#

INT32DIR = $$(CORE_SRCDIR)/32bit_Integer
SRC_INT32 = $(shell ls *.f90)
MOD_INT32 = $(addsuffix .o,$(basename $(SRC_INT32)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INT32DIR = $(INT32DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_INT32)' >> $(MAKENAME)


mod_list:
	@echo  MOD_INT32=  \\ >> $(MAKENAME)
	@echo  $(MOD_INT32) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(INT32DIR)' $(SRC_INT32)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
