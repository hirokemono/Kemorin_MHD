#
#
#

ROCM_TEST_DIR = $$(TESTS_DIR)/ROCm_test

TARGET_ROCM_DGEMM_TEST = test_ROCmBLAS_DGEMM
TARGET_HIPM_DGEMM_TEST = test_HIPmBLAS_DGEMM

TARGET_ROCMFFT_CPLX_PRT_TEST = test_ROCmfft_cplx_prt
TARGET_ROCMFFT_CPLX_RTP_TEST = test_ROCmfft_cplx_rtp
TARGET_ROCMFFT_REAL_PRT_TEST = test_ROCmfft_real_prt
TARGET_ROCMFFT_REAL_RTP_TEST = test_ROCmfft_real_rtp
TARGET_ROCMFFT_OMP_PRT_TEST = test_OMP_ROCmfft_prt
TARGET_ROCMFFT_OMP_RTP_TEST = test_OMP_ROCmfft_rtp
TARGET_SGL_ROCMFFT_CPLX_PRT_TEST = test_ROCmfft_cplx_sgl_prt
TARGET_SGL_ROCMFFT_CPLX_RTP_TEST = test_ROCmfft_cplx_sgl_rtp
TARGET_SGL_ROCMFFT_REAL_PRT_TEST = test_ROCmfft_real_sgl_prt
TARGET_SGL_ROCMFFT_REAL_RTP_TEST = test_ROCmfft_real_sgl_rtp

LIBS_ROCM_TEST = \
'$$(LIB_MESH)'

LIB_FILES_ROCM_TEST = \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_ROCM_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_ROCM_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90 *.F90)
MOD_ROCM_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ROCM_TEST_DIR = $(ROCM_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_MESH_TEST = $$(TEST_BUILD_DIR)/$(TARGET_MESH_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo fem_mesh_test: \
	'$$(TARGET_MESH_TEST)  '\
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MESH_TEST): $$(MOD_ROCM_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_MESH_TEST)' \
	'$$(MOD_ROCM_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:
	@echo  LIBS_ROCM_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_ROCM_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_ROCM_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_ROCM_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_ROCM_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_ROCM_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ROCM_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
