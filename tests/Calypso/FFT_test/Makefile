#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
GMAKE = make
SRCDIR = /Users/matsui/src_kemo
MAKEDIR = $(SRCDIR)/work
BUILDDIR= $(SRCDIR)/bin

CURRENTDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

TESTS_DIR =      $(SRCDIR)/MHD/programs/TESTS
FFT_TEST_DIR = $(TESTS_DIR)/FFT_test

include $(MAKEDIR)/Makefile.header


#
TARGET_SELECT_PRT_FFT_TEST = $(BUILDDIR)/tests/test_prt_select_FFT
TARGET_SELECT_RTP_FFT_TEST = $(BUILDDIR)/tests/test_rtp_select_FFT
#
TARGET_FFTPACK_TEST =  $(CURRENTDIR)/test_rtp_FFTPACK
TARGET_ISPACK1_TEST =  $(CURRENTDIR)/test_rtp_ISPACK1
TARGET_ISPACK3_TEST =  $(CURRENTDIR)/test_rtp_ISPACK3
TARGET_FFW3_F_TEST =   $(CURRENTDIR)/test_rtp_FFTW3_f
TARGET_SGL_FFTW3_TEST = $(CURRENTDIR)/test_single_FFTW3
TARGET_RTP_OMP_FFTW_TEST = $(CURRENTDIR)/test_rtp_OMP_FFTW
TARGET_PRT_FFTPACK_TEST =  $(CURRENTDIR)/test_prt_FFTPACK
TARGET_PRT_ISPACK1_TEST =  $(CURRENTDIR)/test_prt_ISPACK1
TARGET_PRT_ISPACK3_TEST =  $(CURRENTDIR)/test_prt_ISPACK3
TARGET_PRT_FFTW3_F_TEST =   $(CURRENTDIR)/test_prt_FFTW3_f

TARGET_CMPR_FFT_TEST = $(BUILDDIR)/tests/compare_FFT_test
SRC_CMPR_FFT_TEST = $(FFT_TEST_DIR)/compare_FFT_test/compare_FFT_test.f90



all: tako

tako:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_FFTPACK_TEST='$(TARGET_FFTPACK_TEST)' \
		TARGET_ISPACK1_TEST='$(TARGET_ISPACK1_TEST)' \
		TARGET_ISPACK3_TEST='$(TARGET_ISPACK3_TEST)' \
		TARGET_FFW3_F_TEST='$(TARGET_FFW3_F_TEST)' \
		TARGET_SGL_FFW3_TEST='$(TARGET_SGL_FFW3_TEST)' \
		TARGET_RTP_OMP_FFTW_TEST='$(TARGET_RTP_OMP_FFTW_TEST)' \
		TARGET_RTP_OMP_FFTW_TEST='$(TARGET_RTP_OMP_FFTW_TEST)' \
		$(TARGET_RTP_OMP_FFTW_TEST)

test: 
	make clean
	
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_OpenMP_FFTW
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'OMP_FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed OpenMP FFTW3 check'; \
		exit 1;\
	fi
	rm OMP_FFTW_test.dat
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_FFTW
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed FFTW3 check'; \
		exit 1;\
	fi
	rm FFTW_test.dat
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_ISPACK3
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed ISPACK3 check'; \
		exit 1;\
	fi
	rm ISPACK3_test.dat
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_ISPACK1
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed ISPACK1 check'; \
		exit 1;\
	fi
	rm ISPACK1_test.dat
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_FFTPACK
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed FFTPACK check'; \
		exit 1;\
	fi
	rm FFTPACK_test.dat
	
	
	$(TARGET_SELECT_PRT_FFT_TEST) control_FFTW
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside FFTW3 check'; \
		exit 1;\
	fi
	rm FFTW_test.dat
	
	$(TARGET_SELECT_PRT_FFT_TEST) control_ISPACK3
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside ISPACK3 check'; \
		exit 1;\
	fi
	rm ISPACK3_test.dat
	
	$(TARGET_SELECT_PRT_FFT_TEST) control_ISPACK1
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside ISPACK1 check'; \
		exit 1;\
	fi
	rm ISPACK1_test.dat
	
	$(TARGET_SELECT_PRT_FFT_TEST) control_FFTPACK
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside FFTPACK check'; \
		exit 1;\
	fi
	rm FFTPACK_test.dat
	
	echo 'FFT check finished'


clean:
	rm -f *.dat log.txt
	rm -fr $(TARGET_CMPR_FFT_TEST)* $(TARGET_CMPR_FFT_TEST)*

distclean: clean
