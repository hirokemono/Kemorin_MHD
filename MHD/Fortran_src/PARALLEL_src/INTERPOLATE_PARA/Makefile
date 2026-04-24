#
#
#

ITP_PARA_DIR = $$(PARA_SRCDIR)/INTERPOLATE_PARA
SOURCES = $(shell ls *.f90)
MOD_ITP_PARA = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_PARA_DIR = $(ITP_PARA_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ITP_PARA)' >> $(MAKENAME)

mod_list:
	@echo '$(MOD_PARALLEL)+= $$(MOD_ITP_PARA)' >> $(MAKENAME)
	@echo 'MOD_ITP_PARA=  ' \\ >> $(MAKENAME)
	@echo '$(MOD_ITP_PARA)'    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ITP_PARA_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
