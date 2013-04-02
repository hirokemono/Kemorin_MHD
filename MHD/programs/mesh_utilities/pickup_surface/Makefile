#
#
#
#
#

PICK_SURF_SRCDIR = $$(MESHER_DIR)/pickup_surface

TARGET_PICK_SURF =    pick_surface
SRC_PICK_SURF_MAIN = $(shell ls *.f90)
MOD_PICK_SURF_MAIN = $(addsuffix .o,$(basename $(SRC_PICK_SURF_MAIN)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PICK_SURF_SRCDIR = $(PICK_SURF_SRCDIR)'  >> $(MAKENAME)

target_list:
	@echo 'TARGET_PICK_SURF =   $$(BUILDDIR)/$(TARGET_PICK_SURF)'   >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_PICK_SURF)': '$$(MOD_PICK_SURF_MAIN)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PICK_SURF)' \
	'$$(MOD_PICK_SURF_MAIN)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_PICK_SURF_MAIN =  \\ >> $(MAKENAME)
	@echo  $(MOD_PICK_SURF_MAIN) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PICK_SURF_SRCDIR)' $(SRC_PICK_SURF_MAIN)

clean:
	rm -f *.o *~ *.mod *.diag *.L
