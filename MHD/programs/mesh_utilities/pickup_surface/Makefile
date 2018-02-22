#
#
#
#
#

PICK_SURF_SRCDIR = $$(MESHER_DIR)/pickup_surface

TARGET_PICK_SURF =      pick_surface
TARGET_PARA_PICK_SURF = parallel_pick_surf

SOURCES = $(shell ls *.f90)

MOD_PICK_SURF_MAIN = \
pick_surf_single.o

MOD_PARA_PICK_SURF_MAIN = \
main_pick_surface.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PICK_SURF_SRCDIR = $(PICK_SURF_SRCDIR)'  >> $(MAKENAME)

target_list:
	@echo 'TARGET_PICK_SURF =      $$(BUILDDIR)/$(TARGET_PICK_SURF)'   >> $(MAKENAME)
	@echo 'TARGET_PARA_PICK_SURF = $$(BUILDDIR)/$(TARGET_PARA_PICK_SURF)'   >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo pickup_sf: \
	'$$(TARGET_PICK_SURF) $$(TARGET_PARA_PICK_SURF)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_PICK_SURF)': '$$(MOD_PICK_SURF_MAIN)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PICK_SURF)' \
	'$$(MOD_PICK_SURF_MAIN)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_PARA_PICK_SURF)': '$$(MOD_PARA_PICK_SURF_MAIN)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PARA_PICK_SURF)' \
	'$$(MOD_PARA_PICK_SURF_MAIN)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_PICK_SURF_MAIN =       \\ >> $(MAKENAME)
	@echo  $(MOD_PICK_SURF_MAIN)      >> $(MAKENAME)
	@echo  MOD_PARA_PICK_SURF_MAIN =  \\ >> $(MAKENAME)
	@echo  $(MOD_PARA_PICK_SURF_MAIN) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PICK_SURF_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
