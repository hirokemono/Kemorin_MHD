#
#
#

FILTER_3D_MATDIR = $$(FILTER_SRCDIR)/filter_3d
SOURCES = $(shell ls *.f90)
MOD_3D_FILTER_MAT = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_3D_MATDIR = $(FILTER_3D_MATDIR)'   >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_3D_FILTER_MAT)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_3D_FILTER_MAT=  \\ >> $(MAKENAME)
	@echo  $(MOD_3D_FILTER_MAT) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(FILTER_3D_MATDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
