#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR= $(SRCDIR)/bin
#
TARGET_FFTPACK_TEST =  $(BUILDDIR)/tests/test_FFTPACK
TARGET_ISPACK1_TEST =  $(BUILDDIR)/tests/test_ISPACK1
TARGET_ISPACK3_TEST =  $(BUILDDIR)/tests/test_ISPACK3
TARGET_FFW3_F_TEST =   $(BUILDDIR)/tests/test_FFTW3_f
TARGET_SGL_FFW3_TEST = $(BUILDDIR)/tests/test_single_FFTW3
TARGET_OMP_FFW3_TEST = $(BUILDDIR)/tests/test_omp_FFTW3
TARGET_PRT_FFTPACK_TEST =  $(BUILDDIR)/tests/test_prt_FFTPACK
TARGET_PRT_ISPACK1_TEST =  $(BUILDDIR)/tests/test_prt_ISPACK1
TARGET_PRT_ISPACK3_TEST =  $(BUILDDIR)/tests/test_prt_ISPACK3
TARGET_PRT_FFW3_F_TEST =   $(BUILDDIR)/tests/test_prt_FFTW3_f

TARGET_CMPR_FFT_TEST = $(BUILDDIR)/tests/compare_fft_test

all: test

test: 
	make clean
	
	$(TARGET_FFTPACK_TEST)
	$(TARGET_CMPR_FFT_TEST) 'reference/rtp_fftpack_test.dat' 'rtp_fftpack_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Gauss points check'; \
		exit 1;\
	fi
	
	$(TARGET_FFW3_F_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'mul_fftw_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed FFTW3 check'; \
		exit 1;\
	fi
	
	$(TARGET_SGL_FFW3_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'sgl_fftw_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed single FFTW3 check'; \
		exit 1;\
	fi
	
	$(TARGET_PRT_FFTPACK_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'prt_fftpack_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside FFTPACK check'; \
		exit 1;\
	fi
	
	$(TARGET_ISPACK1_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'rtp_ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed ISPACK1 check'; \
		exit 1;\
	fi
	
	$(TARGET_ISPACK3_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'rtp_ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed ISPACK3 check'; \
		exit 1;\
	fi
	
	$(TARGET_PRT_ISPACK1_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'prt_ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside ISPACK1 check'; \
		exit 1;\
	fi
	
	$(TARGET_PRT_ISPACK3_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'prt_ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside ISPACK3 check'; \
		exit 1;\
	fi
	
	$(TARGET_PRT_FFW3_F_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'prt_fftw_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside FFTW3 check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_FFW3_TEST)
	$(TARGET_CMPR_FFT_TEST) 'rtp_fftpack_test.dat' 'omp_fftw_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed OpenMP FFTW3 check'; \
		exit 1;\
	fi
	
	echo 'FFT check finished'


clean:
	rm -f *.dat log.txt

distclean: clean
