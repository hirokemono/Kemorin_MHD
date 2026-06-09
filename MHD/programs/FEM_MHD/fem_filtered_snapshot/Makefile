#
#
#

FEM_SNAP_FILTER_DIR =    $$(MHD_OUT_DIR)/fem_filtered_snapshot
TARGET_FEM_SNAP_FILTER = $$(FEM_BUILDDIR)/fem_filtered_snapshot

SOURCES = $(shell ls *.f90)
MOD_FEM_SNAP_FILTER = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_SNAP_FILTER_DIR = $(FEM_SNAP_FILTER_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FEM_SNAP_FILTER = $(TARGET_FEM_SNAP_FILTER)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_FEM_SNAP_FILTER)': '$$(MOD_FEM_SNAP_FILTER) $$(LIB_FILES_FEM_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_SNAP_FILTER)' \
	'$$(MOD_FEM_SNAP_FILTER) -L. $$(LIBS_FEM_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_FEM_SNAP_FILTER=  \\       >> $(MAKENAME)
	@echo  $(MOD_FEM_SNAP_FILTER)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FEM_SNAP_FILTER_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
