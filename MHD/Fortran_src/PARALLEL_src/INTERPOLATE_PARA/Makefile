#
#
#

ITP_PARA_DIR = $$(PARA_SRCDIR)/INTERPOLATE_PARA
SRC_ITP_PARA = $(shell ls *.f90)
MOD_ITP_PARA = $(addsuffix .o,$(basename $(SRC_ITP_PARA)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_PARA_DIR = $(ITP_PARA_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ITP_PARA)' >> $(MAKENAME)

mod_list:
	@echo  MOD_ITP_PARA=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_PARA) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(ITP_PARA_DIR)' $(SRC_ITP_PARA)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
