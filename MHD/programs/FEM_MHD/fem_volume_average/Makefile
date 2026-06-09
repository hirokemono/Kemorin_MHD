#
#
#

FEM_VOL_AVE_DIR =        $$(MHD_OUT_DIR)/fem_volume_average
TARGET_FEM_SNAP_VOL_AVE= $$(FEM_BUILDDIR)/fem_volume_average

SOURCES = $(shell ls *.f90)
MOD_FEM_SNAP_VOL_AVE = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_VOL_AVE_DIR = $(FEM_VOL_AVE_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FEM_SNAP_VOL_AVE = $(TARGET_FEM_SNAP_VOL_AVE)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_FEM_SNAP_VOL_AVE)': '$$(MOD_FEM_SNAP_VOL_AVE) $$(LIB_FILES_FEM_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_SNAP_VOL_AVE)' \
	'$$(MOD_FEM_SNAP_VOL_AVE) -L. $$(LIBS_FEM_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_FEM_SNAP_VOL_AVE=  \\       >> $(MAKENAME)
	@echo  $(MOD_FEM_SNAP_VOL_AVE)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FEM_VOL_AVE_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
