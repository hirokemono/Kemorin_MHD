#
#
#

PART_MAINDIR =     $$(MESHER_DIR)/partitioner

TARGET_PART_KEMO =   part_kemo
TARGET_ITP_PART =    part_interpolate_table
TARGET_SLEEVE_EXTEND =  extend_sleeves
TARGET_MAKE_SURF_MESH = make_surface_mesh

SOURCES = $(shell ls *.f90)

MOD_PARTITIONER = \
partitioner.o \
set_partition_by_fine_mesh.o \
grouping_for_partition.o \
intelligent_partition.o

MOD_ITP_PART = \
distribute_para_itp_table.o

MOD_SLEEVE_EXTEND = \
main_sleeve_extend.o \
analyzer_sleeve_extend.o

MOD_MAKE_SURF_MESH = \
main_make_surface_mesh.o \
analyzer_make_surface_mesh.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PART_MAINDIR =     $$(MESHER_DIR)/partitioner'     >> $(MAKENAME)

target_list:
	@echo 'TARGET_PART_KEMO =      $$(BUILDDIR)/$(TARGET_PART_KEMO)'     >> $(MAKENAME)
	@echo 'TARGET_ITP_PART =       $$(BUILDDIR)/$(TARGET_ITP_PART)'      >> $(MAKENAME)
	@echo 'TARGET_SLEEVE_EXTEND =  $$(BUILDDIR)/$(TARGET_SLEEVE_EXTEND)' >> $(MAKENAME)
	@echo 'TARGET_MAKE_SURF_MESH = $$(BUILDDIR)/$(TARGET_MAKE_SURF_MESH)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo partitioners: \
	'$$(TARGET_PART_KEMO) $$(TARGET_ITP_PART)' \
	'$$(TARGET_SLEEVE_EXTEND) $$(TARGET_MAKE_SURF_MESH) ' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PART_KEMO)': '$$(MOD_PARTITIONER)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PART_KEMO)' \
	'$$(MOD_PARTITIONER)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ITP_PART)': '$$(MOD_ITP_PART)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_ITP_PART)' \
	'$$(MOD_ITP_PART)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SLEEVE_EXTEND)': '$$(MOD_SLEEVE_EXTEND)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SLEEVE_EXTEND)' \
	'$$(MOD_SLEEVE_EXTEND)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MAKE_SURF_MESH)': '$$(MOD_MAKE_SURF_MESH)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MAKE_SURF_MESH)' \
	'$$(MOD_MAKE_SURF_MESH)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_PARTITIONER =  \\    >> $(MAKENAME)
	@echo  $(MOD_PARTITIONER)       >> $(MAKENAME)
	@echo  MOD_ITP_PART =  \\       >> $(MAKENAME)
	@echo  $(MOD_ITP_PART)          >> $(MAKENAME)
	@echo  MOD_SLEEVE_EXTEND =   \\ >> $(MAKENAME)
	@echo  $(MOD_SLEEVE_EXTEND)     >> $(MAKENAME)
	@echo  MOD_MAKE_SURF_MESH =  \\ >> $(MAKENAME)
	@echo  $(MOD_MAKE_SURF_MESH)    >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PART_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
