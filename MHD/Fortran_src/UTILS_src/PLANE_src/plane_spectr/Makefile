#
#
#

FFT_PLANE_SRCDIR = $$(PLANE_SRCDIR)/plane_spectr
SRC_FFT_PLANE_COMMON = $(shell ls *.f90)
MOD_FFT_PLANE_COMMON = $(addsuffix .o,$(basename $(SRC_FFT_PLANE_COMMON)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_PLANE_SRCDIR = $(FFT_PLANE_SRCDIR)' >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FFT_PLANE_COMMON)' >> $(MAKENAME)

mod_list:
	@echo  MOD_FFT_PLANE_COMMON=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_COMMON) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FFT_PLANE_SRCDIR)' $(SRC_FFT_PLANE_COMMON)
