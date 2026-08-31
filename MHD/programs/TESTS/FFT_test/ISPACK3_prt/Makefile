#
#
#
TEST_PRT_ISPACK3_DIR =    $$(FFT_TEST_DIR)/ISPACK3_prt
TARGET_PRT_ISPACK3_TEST = $$(TEST_BUILD_DIR)/test_prt_ISPACK3
TARGETS =

SOURCES = $(shell ls *.f90)
MOD_PRT_ISPACK3_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_PRT_ISPACK3_DIR = $(TEST_PRT_ISPACK3_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_PRT_ISPACK3_TEST = $(TARGET_PRT_ISPACK3_TEST)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_PRT_ISPACK3_TEST)'         >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_PRT_ISPACK3_TEST)': \
	'$$(MOD_PRT_ISPACK3_TEST)' \
	'$$(LIB_MESH_FILE) $$(LIB_ISPACK3_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PRT_ISPACK3_TEST)' \
	'$$(MOD_PRT_ISPACK3_TEST)' \
	'-L. $$(LIB_MESH) $$(LIB_ISPACK3) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_PRT_ISPACK3_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_PRT_ISPACK3_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_PRT_ISPACK3_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean
