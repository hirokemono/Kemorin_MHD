#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
CURRENTDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

SRCDIR = /home/hrmatsui/src_kemo
MAKEDIR = $(SRCDIR)/work

GMAKE = make

include $(MAKEDIR)/Makefile.header

TARGET_CMPR_FFT_TEST =     $(CURRENTDIR)/compare_FFT_test
TARGET_PRT_FFTPACK_TEST =  $(CURRENTDIR)/test_FFTPACK

TARGET_CPLX_ROCFFT_FFTPACK_PRT_TEST = $(CURRENTDIR)/test_cplx_rocFFT_FFTPACK_prt
TARGET_CPLX_ROCFFT_FFTPACK_RTP_TEST = $(CURRENTDIR)/test_cplx_rocFFT_FFTPACK_rtp
TARGET_REAL_ROCFFT_FFTPACK_PRT_TEST = $(CURRENTDIR)/test_real_rocFFT_FFTPACK_prt
TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST = $(CURRENTDIR)/test_real_rocFFT_FFTPACK_rtp
TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_FFTPACK_prt
TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_FFTPACK_rtp


TARGETS= \
$(TARGET_CMPR_FFT_TEST) \
$(TARGET_PRT_FFTPACK_TEST) \
\
$(TARGET_CPLX_ROCFFT_FFTPACK_PRT_TEST) \
$(TARGET_CPLX_ROCFFT_FFTPACK_RTP_TEST) \
$(TARGET_REAL_ROCFFT_FFTPACK_PRT_TEST) \
$(TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST) \
$(TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST) \
$(TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST)

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_CMPR_FFT_TEST='$(TARGET_CMPR_FFT_TEST)' \
		TARGET_PRT_FFTPACK_TEST='$(TARGET_PRT_FFTPACK_TEST)' \
		TARGET_CPLX_ROCFFT_FFTPACK_PRT_TEST='$(TARGET_CPLX_ROCFFT_FFTPACK_PRT_TEST)' \
		TARGET_CPLX_ROCFFT_FFTPACK_RTP_TEST='$(TARGET_CPLX_ROCFFT_FFTPACK_RTP_TEST)' \
		TARGET_REAL_ROCFFT_FFTPACK_PRT_TEST='$(TARGET_REAL_ROCFFT_FFTPACK_PRT_TEST)' \
		TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST='$(TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST)' \
		TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST='$(TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST)' \
		TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST='$(TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST)' \
		$(TARGETS)

test: dataclean targets
	$(TARGET_PRT_FFTPACK_TEST)
	
	$(TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_OMP_rocFFT_FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside OpenMP rocFFT and FFTPACK check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_OMP_rocFFT_FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside OpenMP rocFFT and FFTPACK check'; \
		exit 1;\
	fi
	
	$(TARGET_CPLX_ROCFFT_FFTPACK_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_complex_rocFFT_FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside OpenMP rocFFT and FFTPACK check'; \
		exit 1;\
	fi
	
	$(TARGET_CPLX_ROCFFT_FFTPACK_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_cplx_rocFFT_FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside OpenMP rocFFT and FFTPACK check'; \
		exit 1;\
	fi
	
	$(TARGET_REAL_ROCFFT_FFTPACK_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_real_rocFFT_FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside OpenMP rocFFT and FFTPACK check'; \
		exit 1;\
	fi
	
	$(TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_real_rocFFT_FFTPACK.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside OpenMP rocFFT and FFTPACK check'; \
		exit 1;\
	fi
	
	@echo 'Finish rocFFT and FFTPACK sharing'


dataclean:
	rm -f *.dat

clean: dataclean
	@for target in $(TARGETS); do \
		(echo $${target}; rm -fr $${target}*) \
	done

destclean: clean