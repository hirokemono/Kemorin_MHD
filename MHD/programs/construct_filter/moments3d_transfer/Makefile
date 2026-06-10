#
#
#

MOMENTS_NEWMESH_DIR =    $$(G_FILTER_PLUG_DIR)/moments3d_transfer
TARGET_NEWMESH_MOMENTS = $$(FEM_UTIL_BINDIR)/moments3d_transfer

SOURCES = $(shell ls *.f90)
MOD_NEWMESH_MOMENTS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MOMENTS_NEWMESH_DIR = $(MOMENTS_NEWMESH_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_NEWMESH_MOMENTS = $(TARGET_NEWMESH_MOMENTS)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_NEWMESH_MOMENTS)': '$$(MOD_NEWMESH_MOMENTS) $$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_NEWMESH_MOMENTS)' \
	'$$(MOD_NEWMESH_MOMENTS) -L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_NEWMESH_MOMENTS=  \\       >> $(MAKENAME)
	@echo  $(MOD_NEWMESH_MOMENTS)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MOMENTS_NEWMESH_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
