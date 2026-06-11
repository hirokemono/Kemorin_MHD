#
#
#

ITP_MESH_DIR =    $$(ITP_PLUG_DIR)/interpolate_node
TARGET_ITP_MESH = $$(FEM_UTIL_BINDIR)/interpolate_node

SOURCES = $(shell ls *.f90)
MOD_ITP_MESH = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_MESH_DIR = $(ITP_MESH_DIR)'   >> $(MAKENAME)

target_list:
	@echo 'TARGET_ITP_MESH = $(TARGET_ITP_MESH)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ITP_MESH)': \
	'$$(MOD_ITP_MESH)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ITP_MESH)' \
	'$$(MOD_ITP_MESH)' '-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_ITP_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_MESH)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ITP_MESH_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
