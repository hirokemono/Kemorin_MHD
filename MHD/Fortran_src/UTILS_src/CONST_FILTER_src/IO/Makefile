#
#
#

FILTER_3D_IODIR =  $$(FILTER_SRCDIR)/IO
SOURCES = $(shell ls *.f90)
MOD_GEN_FILTER_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_3D_IODIR =  $(FILTER_3D_IODIR)'          >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_GEN_FILTER_IO)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_GEN_FILTER_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_FILTER_IO) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(FILTER_3D_IODIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
