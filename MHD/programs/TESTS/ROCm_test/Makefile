#
#
#

ROCM_TEST_DIR = $$(TESTS_DIR)/ROCm_test

SUBDIRS = \
rocBLAS \
rocFFT \

EACH_TEST_DIR = \ 
complex_rocFFT_prt \
complex_rocFFT_rtp \
real_rocFFT_prt \
real_rocFFT_rtp \
OpenMP_rocFFT_prt \
OpenMP_rocFFT_rtp \
\
rocFFT_FFTPACK_prt \
rocFFT_FFTPACK_rtp \
\
complex_rocFFT_FFTPACK_prt \
complex_rocFFT_FFTPACK_rtp \
real_rocFFT_FFTPACK_prt \
real_rocFFT_FFTPACK_rtp \
OpenMP_rocFFT_FFTPACK_prt \
OpenMP_rocFFT_FFTPACK_rtp \
\
OpenMP_rocFFT_ISPACK1_prt \
OpenMP_rocFFT_ISPACK1_rtp \
OpenMP_rocFFT_ISPACK3_prt \
OpenMP_rocFFT_ISPACK3_rtp \
OpenMP_rocFFT_FFTW3_prt \
OpenMP_rocFFT_FFTW3_rtp \
OpenMP_rocFFT_OpenMP_FFTW3_rtp \
\
single_complex_rocFFT_prt \
single_complex_rocFFT_rtp \
single_real_rocFFT_prt \
single_real_rocFFT_rtp

TARGETS_EACH_ROCFFT_TEST = 

LIBS_ROCM_TEST =    \
'$$(LIB_MESH)'      \
'$$(LIB_ISPACK)'    \
'$$(LIB_ISPACK3)'   \
'$$(LIB_FFTPACK)'

LIB_FILES_ROCM_TEST =    \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_ISPACK_FILE)'    \
'$$(LIB_ISPACK3_FILE)'   \
'$$(LIB_FFTPACK_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_ROCM_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_ROCM_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ROCM_TEST_DIR = $(ROCM_TEST_DIR)' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		TARGETS=TARGETS_EACH_ROCFFT_TEST \
		target_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_task:
	@echo ROCm_tests: rocBLAS_tests rocFFT_tests            >> $(MAKENAME)
	 @echo each_rocFFT_tests: $$(TARGETS_EACH_ROCFFT_TEST)  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done

lib_name:
	@echo  LIBS_ROCM_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_ROCM_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_ROCM_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_ROCM_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done
	@echo  '#' >> $(MAKENAME)

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct compile list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		depends; \
		) \
	done


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
