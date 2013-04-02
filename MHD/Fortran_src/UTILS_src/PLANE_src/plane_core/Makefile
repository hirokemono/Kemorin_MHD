#
#
#

PLANE_COMMON_DIR = $$(PLANE_SRCDIR)/plane_core
SRC_PLANE_COMMON = $(shell ls *.f90)
MOD_PLANE_COMMON = $(addsuffix .o,$(basename $(SRC_PLANE_COMMON)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PLANE_COMMON_DIR = $(PLANE_COMMON_DIR)'   >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_PLANE_COMMON)' >> $(MAKENAME)

mod_list:
	@echo  MOD_PLANE_COMMON=  \\ >> $(MAKENAME)
	@echo  $(MOD_PLANE_COMMON) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PLANE_COMMON_DIR)' $(SRC_PLANE_COMMON)

clean:
	rm -f *.o *~ *.mod; \
	cd $(ISPACK_DIR); \
	rm *.a; \
	make clean
