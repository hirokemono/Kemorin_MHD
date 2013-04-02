#
#
#

CUBMESH_DIR = $$(PLANE_SRCDIR)/cubmesh

LIB_CUBMESH = cubmesh

SRC_CUBMESH = $(shell ls *.f90)
MOD_CUBMESH = $(addsuffix .o,$(basename $(SRC_CUBMESH)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CUBMESH_DIR = $(CUBMESH_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_CUBMESH)' >> $(MAKENAME)

mod_list:
	@echo  MOD_CUBMESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUBMESH) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CUBMESH_DIR)' $(SRC_CUBMESH)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
