#
#
#
PERI_CUBE_MESHDIR = $$(MESHER_DIR)/periodic_cube

TARGET_PERI_CUBE = peri_cube_kemo
SOURCES = $(shell ls *.f90)
MOD_PERI_CUBE_MESH = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PERI_CUBE_MESHDIR =  $(PERI_CUBE_MESHDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_PERI_CUBE =   $$(BUILDDIR)/$(TARGET_PERI_CUBE)'    >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_PERI_CUBE)': '$$(MOD_PERI_CUBE_MESH)' \
	 '$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PERI_CUBE)' \
	'$$(MOD_PERI_CUBE_MESH)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_PERI_CUBE_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_PERI_CUBE_MESH)   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PERI_CUBE_MESHDIR)' $(SOURCES)
