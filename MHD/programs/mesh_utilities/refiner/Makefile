#
#
#

REFINE_MAINDIR =   $$(MESHER_DIR)/refiner

TARGET_REFINE =       refiner_kemo
TARGET_REF_PARA_TBL = refine_interpolate_table

SOURCES = $(shell ls *.f90)

MOD_REFINER = \
refiner.o \
analyzer_refine.o

MOD_REF_PARA_TBL = \
refine_interpolate_para_tbl.o \
analyzer_refine_itp_para.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'REFINE_MAINDIR = $(REFINE_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_REFINE =       $$(BUILDDIR)/$(TARGET_REFINE)' >> $(MAKENAME)
	@echo 'TARGET_REF_PARA_TBL = $$(BUILDDIR)/$(TARGET_REF_PARA_TBL)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo refineres: \
	'$$(TARGET_REFINE)    $$(TARGET_REF_PARA_TBL)' \
	>> $(MAKENAME)
	@echo '$$(TARGET_REFINE)': '$$(MOD_REFINER)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_REFINE)' \
	'$$(MOD_REFINER)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_REF_PARA_TBL)': '$$(MOD_REF_PARA_TBL)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_REF_PARA_TBL)' \
	'$$(MOD_REF_PARA_TBL)' '-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

mod_list:
	@echo  MOD_REFINER =  \\ >> $(MAKENAME)
	@echo  $(MOD_REFINER)    >> $(MAKENAME)
	@echo  MOD_REF_PARA_TBL =  \\ >> $(MAKENAME)
	@echo  $(MOD_REF_PARA_TBL)    >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(REFINE_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
