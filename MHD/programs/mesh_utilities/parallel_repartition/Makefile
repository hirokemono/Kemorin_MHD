#
#
#

REPARTITION_DIR = $$(MESHER_DIR)/parallel_repartition

TARGET_VOLUME_REPARTITION = repartition_by_volume
TARGET_CHECK_REPARTITION =  chech_repartition
TARGET_FIELD_TO_NEWPART =   field_to_new_partition

MOD_VOLUME_REPARTITION = \
main_repart_by_volume.o \
analyzer_repart_by_volume.o \
\
t_ctl_file_volume_grouping.o \
t_control_param_repartition.o

MOD_CHECK_REPARTITION = \
main_check_repartition.o \
analyzer_check_repart.o \
\
t_ctl_file_volume_grouping.o \
t_control_param_repartition.o

MOD_FIELD_TO_NEWPART = \
main_field_to_repartition.o \
analyzer_field_to_repart.o \
\
t_ctl_file_volume_grouping.o \
t_control_param_repartition.o \
udt_to_new_partition.o

SOURCES = $(shell ls *.f90)

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'REPARTITION_DIR = $(REPARTITION_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_VOLUME_REPARTITION = $$(BUILDDIR)/$(TARGET_VOLUME_REPARTITION)' >> $(MAKENAME)
	@echo 'TARGET_CHECK_REPARTITION =  $$(BUILDDIR)/$(TARGET_CHECK_REPARTITION)' >> $(MAKENAME)
	@echo 'TARGET_FIELD_TO_NEWPART =   $$(BUILDDIR)/$(TARGET_FIELD_TO_NEWPART)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo repartition: \
	'$$(TARGET_VOLUME_REPARTITION) $$(TARGET_CHECK_REPARTITION)' \
	'$$(TARGET_FIELD_TO_NEWPART)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_VOLUME_REPARTITION): $$(MOD_VOLUME_REPARTITION) $$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_VOLUME_REPARTITION)' \
	'$$(MOD_VOLUME_REPARTITION)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CHECK_REPARTITION): $$(MOD_CHECK_REPARTITION) $$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CHECK_REPARTITION)' \
	'$$(MOD_CHECK_REPARTITION)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FIELD_TO_NEWPART): $$(MOD_FIELD_TO_NEWPART) $$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FIELD_TO_NEWPART)' \
	'$$(MOD_FIELD_TO_NEWPART)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:

mod_list:
	@echo  MOD_VOLUME_REPARTITION=  \\ >> $(MAKENAME)
	@echo  $(MOD_VOLUME_REPARTITION)   >> $(MAKENAME)
	@echo  MOD_CHECK_REPARTITION=  \\  >> $(MAKENAME)
	@echo  $(MOD_CHECK_REPARTITION)    >> $(MAKENAME)
	@echo  MOD_FIELD_TO_NEWPART=  \\   >> $(MAKENAME)
	@echo  $(MOD_FIELD_TO_NEWPART)     >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(REPARTITION_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
