#
#
#

MHD_COMMON_DIR = $$(MHD_SRCDIR)/common
SOURCES = $(shell ls *.f90)
MOD_MHD_COMMON = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_COMMON_DIR = $(MHD_COMMON_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_MHD_COMMON)' \
	>> $(MAKENAME)

mod_list:
	@echo '$(MOD_MHD)+= $$(MOD_MHD_COMMON)' >> $(MAKENAME)
	@echo  'MOD_MHD_COMMON=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MHD_COMMON)'    >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_COMMON_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
