#
#
#

FEM_SNAPSHOT_DIR =    $$(MHD_OUT_DIR)/fem_snapshot
TARGET_FEM_SNAPSHOT = $$(FEM_BUILDDIR)/fem_snapshot

SOURCES = $(shell ls *.f90)
MOD_FEM_SNAPSHOT = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_SNAPSHOT_DIR = $(FEM_SNAPSHOT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FEM_SNAPSHOT = $(TARGET_FEM_SNAPSHOT)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_FEM_SNAPSHOT)': '$$(MOD_FEM_SNAPSHOT) $$(LIB_FILES_FEM_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_SNAPSHOT)' \
	'$$(MOD_FEM_SNAPSHOT) -L. $$(LIBS_FEM_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_FEM_SNAPSHOT=  \\       >> $(MAKENAME)
	@echo  $(MOD_FEM_SNAPSHOT)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FEM_SNAPSHOT_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
