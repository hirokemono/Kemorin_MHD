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

TARGET_ROCFFT_PRT_TEST =   $(CURRENTDIR)/test_rocFFT_prt
TARGET_ROCFFT_RTP_TEST =   $(CURRENTDIR)/test_rocFFT_rtp


TARGETS= \
$(TARGET_PRT_FFTPACK_TEST) \
\
$(TARGET_ROCFFT_PRT_TEST) \
$(TARGET_ROCFFT_RTP_TEST)

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_PRT_FFTPACK_TEST='$(TARGET_PRT_FFTPACK_TEST)' \
		TARGET_ROCFFT_PRT_TEST='$(TARGET_ROCFFT_PRT_TEST)' \
		TARGET_ROCFFT_RTP_TEST='$(TARGET_ROCFFT_RTP_TEST)' \
		$(TARGETS)

test: dataclean targets
	$(TARGET_PRT_FFTPACK_TEST) control_rocFFT_base
	
	sed -e s/OpenMP_rocFFT/complex_rocFFT/g control_rocFFT_base > control_rocFFT
	$(TARGET_ROCFFT_PRT_TEST) control_rocFFT
	$(TARGET_ROCFFT_RTP_TEST) control_rocFFT
	
	sed -e s/OpenMP_rocFFT/real_rocFFT/g control_rocFFT_base > control_rocFFT
	$(TARGET_ROCFFT_PRT_TEST) control_rocFFT
	$(TARGET_ROCFFT_RTP_TEST) control_rocFFT
	
	$(TARGET_ROCFFT_PRT_TEST) control_rocFFT_base
	$(TARGET_ROCFFT_RTP_TEST) control_rocFFT_base
	
	@echo 'Finish rocFFT test'


dataclean:
	rm -f *.dat

clean: dataclean
	rm -f control_rocFFT
	@for target in $(TARGETS); do \
		(echo $${target}; rm -fr $${target}*) \
	done

destclean: clean