#
#
#

CORRELATE_PLANE_DIR = $$(PLANE_SRCDIR)/correlation
SRC_CORRELATE_PLANE = $(shell ls *.f90)
MOD_CORRELATE_PLANE = $(addsuffix .o,$(basename $(SRC_CORRELATE_PLANE)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CORRELATE_PLANE_DIR = $(CORRELATE_PLANE_DIR)'  >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_CORRELATE_PLANE)' >> $(MAKENAME)

mod_list:
	@echo  MOD_CORRELATE_PLANE=  \\ >> $(MAKENAME)
	@echo  $(MOD_CORRELATE_PLANE) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CORRELATE_PLANE_DIR)' $(SRC_CORRELATE_PLANE)
