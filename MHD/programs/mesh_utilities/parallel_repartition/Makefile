#
#
#

REPARTITION_DIR = $$(MESHER_DIR)/parallel_repartition

TARGET_VOLUME_REPARTITION = repartition_by_volume

MOD_VOLUME_REPARTITION = \
main_repart_by_volume.o \
analyzer_repart_by_volume.o


SOURCES = $(shell ls *.f90)

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'REPARTITION_DIR = $(REPARTITION_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_VOLUME_REPARTITION = $$(BUILDDIR)/$(TARGET_VOLUME_REPARTITION)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo repartition: \
	'$$(TARGET_VOLUME_REPARTITION) '  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_VOLUME_REPARTITION): $$(MOD_VOLUME_REPARTITION) $$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_VOLUME_REPARTITION)' \
	'$$(MOD_VOLUME_REPARTITION)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:

mod_list:
	@echo  MOD_VOLUME_REPARTITION=  \\ >> $(MAKENAME)
	@echo  $(MOD_VOLUME_REPARTITION) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(REPARTITION_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
