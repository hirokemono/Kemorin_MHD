#
#
#

FFT_PLANE_SRCDIR = $$(PLANE_SRCDIR)/plane_spectr
SOURCES = $(shell ls *.f90)

MOD_FFT_PLANE_COMMON = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_FFT_PLANE_COMMON'
UPPER_MODULES=

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
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\    >> $(MAKENAME)
	@echo  '$(MOD_FFT_PLANE_COMMON)' >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_PLANE_SRCDIR)' $(SOURCES)
