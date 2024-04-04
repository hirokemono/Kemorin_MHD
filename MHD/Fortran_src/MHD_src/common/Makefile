#
#
#

MHD_COMMON_DIR = $$(MHD_SRCDIR)/common
SOURCES = $(shell ls *.f90)
MOD_COMMON = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_COMMON_DIR = $(MHD_COMMON_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_COMMON)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_COMMON=  \\ >> $(MAKENAME)
	@echo  $(MOD_COMMON) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_COMMON_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
