#
#
#
TEST_OMP_ROCFFT_ISPACK3_PRT_DIR = $$(ROCM_TEST_DIR)/OpenMP_rocFFT_ISPACK3_prt
TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST = $$(TEST_BUILD_DIR)/test_OMP_rocFFT_ISPACK3_prt
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_OMP_ROCFFT_ISPACK3_PRT_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_OMP_ROCFFT_ISPACK3_PRT_DIR = $(TEST_OMP_ROCFFT_ISPACK3_PRT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST' \
	'= $(TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST)': \
	'$$(MOD_OMP_ROCFFT_ISPACK3_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST)' \
	'$$(MOD_OMP_ROCFFT_ISPACK3_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_OMP_ROCFFT_ISPACK3_PRT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_OMP_ROCFFT_ISPACK3_PRT_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_OMP_ROCFFT_ISPACK3_PRT_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean
