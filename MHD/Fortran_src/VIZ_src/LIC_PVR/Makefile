#
#
#

LIC_DIR = $$(VIZ_SRCDIR)/LIC_PVR
SOURCES = $(shell ls *.f90 *.F90)
MOD_LIC = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'LIC_DIR = $(LIC_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_LIC)' \
	>> $(MAKENAME)



mod_list:
	@echo  MOD_LIC=  \\ >> $(MAKENAME)
	@echo  $(MOD_LIC) >> $(MAKENAME)
	@echo  >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(LIC_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
