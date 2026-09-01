#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
CURRENTDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

SRCDIR = /Users/matsui/src_kemo
MAKEDIR = $(SRCDIR)/work

GMAKE = make

include $(MAKEDIR)/Makefile.header


TARGET_CMPR_FFT_TEST =     $(CURRENTDIR)/compare_FFT_test
#
TARGET_SELECT_PRT_FFT_TEST = $(CURRENTDIR)/test_prt_select_FFT
TARGET_SELECT_RTP_FFT_TEST = $(CURRENTDIR)/test_rtp_select_FFT
#
TARGET_RTP_ISPACK1_TEST =  $(CURRENTDIR)/test_rtp_ISPACK1
TARGET_RTP_ISPACK3_TEST =  $(CURRENTDIR)/test_rtp_ISPACK3
TARGET_PRT_ISPACK1_TEST =  $(CURRENTDIR)/test_prt_ISPACK1
TARGET_PRT_ISPACK3_TEST =  $(CURRENTDIR)/test_prt_ISPACK3
#
TARGET_RTP_OMP_FFTW_TEST = $(CURRENTDIR)/test_rtp_OMP_FFTW


TARGETS_FFT_TEST = \
		$(TARGET_SELECT_PRT_FFT_TEST) \
		$(TARGET_SELECT_RTP_FFT_TEST) \
		$(TARGET_RTP_ISPACK1_TEST) \
		$(TARGET_PRT_ISPACK1_TEST) \
		$(TARGET_RTP_ISPACK3_TEST) \
		$(TARGET_PRT_ISPACK3_TEST) \
		$(TARGET_CMPR_FFT_TEST)

TEST_LIST= test_fft

ifdef FFTW3_OMP_LIBS
  TEST_LIST+= test_omp_fftw
endif



all: test

test: $(TEST_LIST)
	@echo 'FFT check finished'


test_fft:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SELECT_PRT_FFT_TEST='$(TARGET_SELECT_PRT_FFT_TEST)' \
		TARGET_SELECT_RTP_FFT_TEST='$(TARGET_SELECT_RTP_FFT_TEST)' \
		TARGET_RTP_ISPACK1_TEST='$(TARGET_RTP_ISPACK1_TEST)' \
		TARGET_PRT_ISPACK1_TEST='$(TARGET_PRT_ISPACK1_TEST)' \
		TARGET_RTP_ISPACK3_TEST='$(TARGET_RTP_ISPACK3_TEST)' \
		TARGET_PRT_ISPACK3_TEST='$(TARGET_PRT_ISPACK3_TEST)' \
		TARGET_CMPR_FFT_TEST='$(TARGET_CMPR_FFT_TEST)' \
		$(TARGETS_FFT_TEST)
	
	$(TARGET_RTP_ISPACK3_TEST) control_ISPACK3
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside ISPACK3 check'; \
		exit 1;\
	fi
	rm ISPACK3_test.dat
	
	$(TARGET_PRT_ISPACK3_TEST) control_ISPACK3
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside ISPACK3 check'; \
		exit 1;\
	fi
	rm ISPACK3_test.dat
	
	$(TARGET_RTP_ISPACK1_TEST) control_ISPACK1
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside ISPACK1 check'; \
		exit 1;\
	fi
	rm ISPACK1_test.dat
	
	$(TARGET_PRT_ISPACK1_TEST) control_ISPACK1
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside ISPACK1 check'; \
		exit 1;\
	fi
	rm ISPACK1_test.dat
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_FFTW
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside FFTW3 check'; \
		exit 1;\
	fi
	rm FFTW_test.dat
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_ISPACK3
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside ISPACK3 check'; \
		exit 1;\
	fi
	rm ISPACK3_test.dat
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_ISPACK1
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside ISPACK1 check'; \
		exit 1;\
	fi
	rm ISPACK1_test.dat
	
	$(TARGET_SELECT_RTP_FFT_TEST) control_FFTPACK
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside FFTPACK check'; \
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

test_omp_fftw:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_RTP_OMP_FFTW_TEST='$(TARGET_RTP_OMP_FFTW_TEST)' \
		$(TARGET_RTP_OMP_FFTW_TEST)
	
	$(TARGET_RTP_OMP_FFTW_TEST) control_OpenMP_FFTW
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'OMP_FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed OpenMP FFTW3 check'; \
		exit 1;\
	fi
	rm OMP_FFTW_test.dat
	

clean:
	rm -f *.dat log.txt
	rm -fr $(TARGETS_FFT_TEST) $(TARGET_RTP_OMP_FFTW_TEST)

distclean: clean
