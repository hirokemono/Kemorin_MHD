#
#
#

PLANE_IO_DIR = $$(PLANE_SRCDIR)/IO
SOURCES = $(shell ls *.f90)
MOD_PLANE_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'PLANE_IO_DIR = $(PLANE_IO_DIR)' >> $(MAKENAME)

dir_list:

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_PLANE_IO)' >> $(MAKENAME)

mod_list:
	@echo  MOD_PLANE_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_PLANE_IO) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PLANE_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
