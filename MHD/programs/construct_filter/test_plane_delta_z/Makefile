#
#
#

TEST_PLANE_DZ_DIR =    $$(G_FILTER_PLUG_DIR)/test_plane_delta_z
TARGET_TEST_PLANE_DZ = $$(FEM_UTIL_BINDIR)/test_plane_delta_z

SOURCES = $(shell ls *.f90)
MOD_TEST_PLANE_DZ = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_PLANE_DZ_DIR = $(TEST_PLANE_DZ_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_TEST_PLANE_DZ = $(TARGET_TEST_PLANE_DZ)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_TEST_PLANE_DZ)': '$$(MOD_TEST_PLANE_DZ) $$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_TEST_PLANE_DZ)' \
	'$$(MOD_TEST_PLANE_DZ) -L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_TEST_PLANE_DZ=  \\       >> $(MAKENAME)
	@echo  $(MOD_TEST_PLANE_DZ)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_PLANE_DZ_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
