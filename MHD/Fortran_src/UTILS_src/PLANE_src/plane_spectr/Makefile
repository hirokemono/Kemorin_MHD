#
#
#

FFT_PLANE_SRCDIR = $$(PLANE_SRCDIR)/plane_spectr
SOURCES = $(shell ls *.f90)
MOD_FFT_PLANE_COMMON = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_PLANE_SRCDIR = $(FFT_PLANE_SRCDIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FFT_PLANE_COMMON)' >> $(MAKENAME)

mod_list:
	@echo  MOD_FFT_PLANE_COMMON=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_COMMON) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_PLANE_SRCDIR)' $(SOURCES)
