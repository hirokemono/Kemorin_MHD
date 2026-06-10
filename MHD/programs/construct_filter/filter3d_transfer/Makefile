#
#
#

FILTER3D_NEWMESH_DIR =    $$(G_FILTER_PLUG_DIR)/filter3d_transfer
TARGET_NEWMESH_FILTER3D = $$(FEM_UTIL_BINDIR)/filter3d_transfer

SOURCES = $(shell ls *.f90)
MOD_NEWMESH_FILTER3D = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER3D_NEWMESH_DIR = $(FILTER3D_NEWMESH_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_NEWMESH_FILTER3D = $(TARGET_NEWMESH_FILTER3D)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_NEWMESH_FILTER3D)': '$$(MOD_NEWMESH_FILTER3D) $$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_NEWMESH_FILTER3D)' \
	'$$(MOD_NEWMESH_FILTER3D) -L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_NEWMESH_FILTER3D=  \\       >> $(MAKENAME)
	@echo  $(MOD_NEWMESH_FILTER3D)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FILTER3D_NEWMESH_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
