#
#
#

FEM_SNAP_TMP_DIR =    $$(MHD_OUT_DIR)/fem_snapshot_tmp
TARGET_FEM_SNAP_TMP = $$(FEM_BUILDDIR)/fem_snapshot_tmp

SOURCES = $(shell ls *.f90)
MOD_FEM_SNAP_TMP = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_SNAP_TMP_DIR = $(FEM_SNAP_TMP_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FEM_SNAP_TMP = $(TARGET_FEM_SNAP_TMP)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_FEM_SNAP_TMP)': '$$(MOD_FEM_SNAP_TMP) $$(LIB_FILES_FEM_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_SNAP_TMP)' \
	'$$(MOD_FEM_SNAP_TMP) -L. $$(LIBS_FEM_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_FEM_SNAP_TMP=  \\       >> $(MAKENAME)
	@echo  $(MOD_FEM_SNAP_TMP)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FEM_SNAP_TMP_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
