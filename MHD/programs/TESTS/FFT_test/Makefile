#
#
#
#
#

FFT_TEST_DIR =       $$(TESTS_DIR)/FFT_test

SUBDIRS = \
original_FFT_test \
\
FFTPACK_rtp \
ISPACK1_rtp \
ISPACK3_rtp \
FFTPACK_prt \
ISPACK1_prt \
ISPACK3_prt \
\
compare_FFT_test


ifdef FFTW3_LIBS
  SUBDIRS+= \
   FFTW3_rtp \
   FFTW3_prt \
   single_FFTW3_rtp
endif

ifdef FFTW3_OMP_LIBS
  SUBDIRS+= OpenMP_FFTW3_rtp
endif


TARGET_SELECT_PRT_FFT_TEST = $$(TEST_BUILD_DIR)/test_prt_select_FFT
TARGET_SELECT_RTP_FFT_TEST = $$(TEST_BUILD_DIR)/test_rtp_select_FFT

TARGETS_FFT_TEST = \
'$$(TARGET_SELECT_PRT_FFT_TEST)' \
'$$(TARGET_SELECT_RTP_FFT_TEST)'

TARGETS_EACH_FFT_TEST = 

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
SOURCES = $(shell ls *.f90 *.F90)

MOD_SELECT_PRT_FFT_TEST = \
test_prt_select_FFT.o \
multi_FFT_select.o

MOD_SELECT_RTP_FFT_TEST = \
test_rtp_select_FFT.o \
multi_FFT_select.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_TEST_DIR =  $(FFT_TEST_DIR)'          >> $(MAKENAME)
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
	@echo 'TARGET_SELECT_PRT_FFT_TEST = $(TARGET_SELECT_PRT_FFT_TEST)' >> $(MAKENAME)
	@echo 'TARGET_SELECT_RTP_FFT_TEST = $(TARGET_SELECT_RTP_FFT_TEST)' >> $(MAKENAME)
	@echo 'TARGET_SGL_FFW3_TEST =    $(TARGET_SGL_FFW3_TEST)'     >> $(MAKENAME)
	@echo 'TARGET_FFT_LABEL_CHECK =  $(TARGET_FFT_LABEL_CHECK)'   >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		TARGETS=TARGETS_EACH_FFT_TEST \
		target_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_task:
	@echo fft_test:      $$(TARGETS_FFT_TEST) >> $(MAKENAME)
	@echo each_fft_test: $$TARGETS_EACH_FFT_TEST) >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SELECT_PRT_FFT_TEST):' \
	'$$(MOD_SELECT_PRT_FFT_TEST)'  \
	'$$(LIB_FILES_FFT_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SELECT_PRT_FFT_TEST)' \
	'$$(MOD_SELECT_PRT_FFT_TEST)'  \
	'-L. $$(LIBS_FFT_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SELECT_RTP_FFT_TEST):' \
	'$$(MOD_SELECT_RTP_FFT_TEST)'  \
	'$$(LIB_FILES_FFT_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SELECT_RTP_FFT_TEST)' \
	'$$(MOD_SELECT_RTP_FFT_TEST)'  \
	'-L. $$(LIBS_FFT_TEST) $$(F90LIBS)' >> $(MAKENAME)
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
	@echo  LIBS_FFT_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FFT_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_FFT_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FFT_TEST)       >> $(MAKENAME)

mod_list:
	@echo  MOD_SELECT_PRT_FFT_TEST= \\ >> $(MAKENAME)
	@echo  $(MOD_SELECT_PRT_FFT_TEST)  >> $(MAKENAME)
	@echo  MOD_SELECT_RTP_FFT_TEST= \\ >> $(MAKENAME)
	@echo  $(MOD_SELECT_RTP_FFT_TEST)  >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
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
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_TEST_DIR)' $(SOURCES)
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
