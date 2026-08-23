#
#
#
#
#

FFT_TEST_DIR =       $$(TESTS_DIR)/FFT_test

TARGET_FFT_TEST =      test_fft

TARGET_FFTPACK_TEST =  $$(TEST_BUILD_DIR)/test_FFTPACK
TARGET_ISPACK1_TEST =  $$(TEST_BUILD_DIR)/test_ISPACK1
TARGET_ISPACK3_TEST =  $$(TEST_BUILD_DIR)/test_ISPACK3
TARGET_FFW3_F_TEST =   $$(TEST_BUILD_DIR)/test_FFTW3_f
TARGET_SGL_FFW3_TEST = $$(TEST_BUILD_DIR)/test_single_FFTW3
TARGET_OMP_FFW3_TEST = $$(TEST_BUILD_DIR)/test_omp_FFTW3

TARGET_PRT_FFTPACK_TEST =  $$(TEST_BUILD_DIR)/test_prt_FFTPACK
TARGET_PRT_ISPACK1_TEST =  $$(TEST_BUILD_DIR)/test_prt_ISPACK1
TARGET_PRT_ISPACK3_TEST =  $$(TEST_BUILD_DIR)/test_prt_ISPACK3
TARGET_PRT_FFW3_F_TEST =   $$(TEST_BUILD_DIR)/test_prt_FFTW3_f

TARGET_FFW3_F_TEST =     $$(TEST_BUILD_DIR)/test_FFTW3_f

TARGET_CMPR_FFT_TEST =   $$(TEST_BUILD_DIR)/compare_fft_test

TARGET_FFT_LABEL_CHECK = $$(TEST_BUILD_DIR)/check_FFT_labels

TARGETS_FFT_TEST = \
'$$(TARGET_FFT_TEST)' \
'$$(TARGET_FFTPACK_TEST)' \
'$$(TARGET_ISPACK1_TEST)' \
'$$(TARGET_ISPACK3_TEST)' \
'$$(TARGET_PRT_FFTPACK_TEST)' \
'$$(TARGET_PRT_ISPACK1_TEST)' \
'$$(TARGET_PRT_ISPACK3_TEST)' \
'$$(TARGET_PRT_FFW3_F_TEST)' \
'$$(TARGET_CMPR_FFT_TEST)' \
'$$(TARGET_FFT_LABEL_CHECK)'


ifdef FFTW3_LIBS
  TARGETS_FFT_TEST+= \
   '$$(TARGET_FFW3_F_TEST)' \
   '$$(TARGET_SGL_FFW3_TEST)'
endif

ifdef FFTW3_OMP_LIBS
  TARGETS_FFT_TEST+= \
   '$$(TARGET_OMP_FFW3_TEST)'
endif


LIBS_FFT_TEST = \
'$$(LIB_MESH)'      \
'$$(LIB_FFTPACK)'   \
'$$(LIB_ISPACK)'    \
'$$(LIB_ISPACK3)'   \
'$$(LIB_NETLIB)'

LIB_FILES_FFT_TEST = \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_FFTPACK_FILE)'   \
'$$(LIB_ISPACK_FILE)'    \
'$$(LIB_ISPACK3_FILE)'   \
'$$(LIB_NETLIB_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_FFT_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_FFT_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90)

MOD_FFT_TEST = \
m_FFT_size.o \
t_fft_test_data.o \
FFT_test_loop.o \
test_FFT.o

LIBS_FFTPACK_TEST = \
'$$(LIB_MESH)'      \
'$$(LIB_FFTPACK)'

LIB_FILES_FFTPACK_TEST = \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_FFTPACK_FILE)'

LIBS_ISPACK_TEST = \
'$$(LIB_MESH)'      \
'$$(LIB_ISPACK)'

LIB_FILES_ISPACK_TEST = \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_ISPACK_FILE)'

LIBS_ISPACK3_TEST = \
'$$(LIB_MESH)'      \
'$$(LIB_ISPACK3)'

LIB_FILES_ISPACK3_TEST = \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_ISPACK3_FILE)'

LIBS_FFTW_TEST = \
'$$(LIB_MESH)'      \
'$$(LIB_ISPACK3)'

LIB_FILES_FFTW_TEST = \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_ISPACK3_FILE)'

MOD_FFTPACK_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_FFTPACK5.o

MOD_PRT_FFTPACK_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_prt_FFTPACK5.o


MOD_ISPACK1_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_ISPACK1_FFT.o

MOD_PRT_ISPACK1_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_prt_ISPACK1_FFT.o

MOD_ISPACK3_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_ISPACK3_FFT.o

MOD_PRT_ISPACK3_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_prt_ISPACK3_FFT.o

MOD_FFW3_F_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_FFTW3.o

MOD_PRT_FFW3_F_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_prt_FFTW3.o

MOD_OMP_FFW3_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_OMP_FFTW3.o

MOD_SGL_FFW3_TEST = \
t_fft_test_data.o \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
test_single_FFTW3.o

MOD_CMPR_FFT_TEST = \
m_precision.o \
m_constants.o \
m_machine_parameter.o \
\
compare_FFT_test.o \
t_fft_test_data.o

MOD_FFT_LABEL_CHECK = \
FFT_flag_check.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_TEST_DIR =  $(FFT_TEST_DIR)'          >> $(MAKENAME)

target_list:
	@echo 'TARGET_FFT_TEST =         $(TARGET_FFT_TEST)'          >> $(MAKENAME)
	@echo 'TARGET_FFTPACK_TEST =     $(TARGET_FFTPACK_TEST)'      >> $(MAKENAME)
	@echo 'TARGET_ISPACK1_TEST =     $(TARGET_ISPACK1_TEST)'      >> $(MAKENAME)
	@echo 'TARGET_ISPACK3_TEST =     $(TARGET_ISPACK3_TEST)'      >> $(MAKENAME)
	@echo 'TARGET_FFW3_F_TEST =      $(TARGET_FFW3_F_TEST)'       >> $(MAKENAME)
	@echo 'TARGET_SGL_FFW3_TEST =    $(TARGET_SGL_FFW3_TEST)'     >> $(MAKENAME)
	@echo 'TARGET_OMP_FFW3_TEST =    $(TARGET_OMP_FFW3_TEST)'     >> $(MAKENAME)
	@echo 'TARGET_PRT_FFTPACK_TEST = $(TARGET_PRT_FFTPACK_TEST)'  >> $(MAKENAME)
	@echo 'TARGET_PRT_ISPACK1_TEST = $(TARGET_PRT_ISPACK1_TEST)'  >> $(MAKENAME)
	@echo 'TARGET_PRT_ISPACK3_TEST = $(TARGET_PRT_ISPACK3_TEST)'  >> $(MAKENAME)
	@echo 'TARGET_PRT_FFW3_F_TEST =  $(TARGET_PRT_FFW3_F_TEST)'   >> $(MAKENAME)
	@echo 'TARGET_CMPR_FFT_TEST =    $(TARGET_CMPR_FFT_TEST)'     >> $(MAKENAME)
	@echo 'TARGET_FFT_LABEL_CHECK =  $(TARGET_FFT_LABEL_CHECK)'   >> $(MAKENAME)

target_task:
	@echo fft_test: $(TARGETS_FFT_TEST) >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FFT_TEST)': '$$(MOD_FFT_TEST)' \
	'$$(LIB_FILES_FFT_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FFT_TEST)' \
	'$$(MOD_FFT_TEST)  -L. $$(LIBS_FFT_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FFTPACK_TEST)': '$$(MOD_FFTPACK_TEST)' \
	'$$(LIB_FILES_FFTPACK_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FFTPACK_TEST)' \
	'$$(MOD_FFTPACK_TEST)  -L. $$(LIBS_FFTPACK_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PRT_FFTPACK_TEST)': '$$(MOD_PRT_FFTPACK_TEST)' \
	'$$(LIB_FILES_FFTPACK_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PRT_FFTPACK_TEST)' \
	'$$(MOD_PRT_FFTPACK_TEST)  -L. $$(LIBS_FFTPACK_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ISPACK1_TEST)': '$$(MOD_ISPACK1_TEST)' \
	'$$(LIB_FILES_ISPACK_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ISPACK1_TEST)' \
	'$$(MOD_ISPACK1_TEST)  -L. $$(LIBS_ISPACK_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PRT_ISPACK1_TEST)': '$$(MOD_PRT_ISPACK1_TEST)' \
	'$$(LIB_FILES_ISPACK_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PRT_ISPACK1_TEST)' \
	'$$(MOD_PRT_ISPACK1_TEST)  -L. $$(LIBS_ISPACK_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ISPACK3_TEST)': '$$(MOD_ISPACK3_TEST)' \
	'$$(LIB_FILES_ISPACK3_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ISPACK3_TEST)' \
	'$$(MOD_ISPACK3_TEST)  -L. $$(LIBS_ISPACK3_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PRT_ISPACK3_TEST)': '$$(MOD_PRT_ISPACK3_TEST)' \
	'$$(LIB_FILES_ISPACK3_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PRT_ISPACK3_TEST)' \
	'$$(MOD_PRT_ISPACK3_TEST)  -L. $$(LIBS_ISPACK3_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FFW3_F_TEST)': '$$(MOD_FFW3_F_TEST)' \
	'$$(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FFW3_F_TEST)' \
	'$$(MOD_FFW3_F_TEST) -L. $$(LIB_MESH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PRT_FFW3_F_TEST)': '$$(MOD_PRT_FFW3_F_TEST)' \
	'$$(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PRT_FFW3_F_TEST)' \
	'$$(MOD_PRT_FFW3_F_TEST) -L. $$(LIB_MESH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SGL_FFW3_TEST)': '$$(MOD_SGL_FFW3_TEST)' \
	'$$(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SGL_FFW3_TEST)' \
	'$$(MOD_SGL_FFW3_TEST) -L. $$(LIB_MESH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_OMP_FFW3_TEST)': '$$(MOD_OMP_FFW3_TEST)' \
	'$$(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_OMP_FFW3_TEST)' \
	'$$(MOD_OMP_FFW3_TEST) -L. $$(LIB_MESH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CMPR_FFT_TEST)': '$$(MOD_CMPR_FFT_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CMPR_FFT_TEST)' \
	'$$(MOD_CMPR_FFT_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FFT_LABEL_CHECK)': '$$(MOD_FFT_LABEL_CHECK)' \
	'$$(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FFT_LABEL_CHECK)' \
	'$$(MOD_FFT_LABEL_CHECK)' -L. '$$(LIB_MESH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_FFT_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FFT_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_FFT_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FFT_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_FFTPACK_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FFTPACK_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_FFTPACK_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FFTPACK_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_ISPACK_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_ISPACK_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_ISPACK_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_ISPACK_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_ISPACK3_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_ISPACK3_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_ISPACK3_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_ISPACK3_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_FFTW_TEST=  \\             >> $(MAKENAME)
	@echo  $(LIBS_FFTW_TEST)               >> $(MAKENAME)
	@echo  LIB_FILES_FFTW_TEST=  \\        >> $(MAKENAME)
	@echo  $(LIB_FILES_FFTW_TEST)          >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_FFT_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_FFT_TEST)        >> $(MAKENAME)
	@echo  MOD_FFTPACK_TEST=       \\  >> $(MAKENAME)
	@echo  $(MOD_FFTPACK_TEST)         >> $(MAKENAME)
	@echo  MOD_ISPACK1_TEST=       \\  >> $(MAKENAME)
	@echo  $(MOD_ISPACK1_TEST)         >> $(MAKENAME)
	@echo  MOD_ISPACK3_TEST=       \\  >> $(MAKENAME)
	@echo  $(MOD_ISPACK3_TEST)         >> $(MAKENAME)
	@echo  MOD_FFW3_F_TEST=        \\  >> $(MAKENAME)
	@echo  $(MOD_FFW3_F_TEST)          >> $(MAKENAME)
	@echo  MOD_PRT_FFTPACK_TEST=   \\  >> $(MAKENAME)
	@echo  $(MOD_PRT_FFTPACK_TEST)     >> $(MAKENAME)
	@echo  MOD_PRT_ISPACK1_TEST=   \\  >> $(MAKENAME)
	@echo  $(MOD_PRT_ISPACK1_TEST)     >> $(MAKENAME)
	@echo  MOD_PRT_ISPACK3_TEST=   \\  >> $(MAKENAME)
	@echo  $(MOD_PRT_ISPACK3_TEST)     >> $(MAKENAME)
	@echo  MOD_PRT_ISPACK3_TEST=   \\  >> $(MAKENAME)
	@echo  $(MOD_PRT_ISPACK3_TEST)     >> $(MAKENAME)
	@echo  MOD_PRT_FFW3_F_TEST=    \\  >> $(MAKENAME)
	@echo  $(MOD_PRT_FFW3_F_TEST)      >> $(MAKENAME)
	@echo  MOD_SGL_FFW3_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_SGL_FFW3_TEST)        >> $(MAKENAME)
	@echo  MOD_OMP_FFW3_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_OMP_FFW3_TEST)        >> $(MAKENAME)
	@echo  MOD_CMPR_FFT_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_CMPR_FFT_TEST)        >> $(MAKENAME)
	@echo  MOD_FFT_LABEL_CHECK=    \\  >> $(MAKENAME)
	@echo  $(MOD_FFT_LABEL_CHECK)      >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
