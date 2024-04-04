#
#
#

VIZ_F2003_DIR = $$(VIZ_SRCDIR)/Fortran2003

SOURCES = $(shell ls *.f90)
MOD_VIZ_F2003 = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'VIZ_F2003_DIR = $(VIZ_F2003_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_VIZ_F2003)' \
	>> $(MAKENAME)



mod_list:
	@echo  MOD_VIZ_F2003=  \\ >> $(MAKENAME)
	@echo  $(MOD_VIZ_F2003) >> $(MAKENAME)
	@echo  >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(VIZ_F2003_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
