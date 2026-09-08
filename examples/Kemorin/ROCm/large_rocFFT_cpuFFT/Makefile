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

TARGET_PRT_FFTPACK_TEST =  $(CURRENTDIR)/test_FFTPACK

TARGET_ROCFFT_CPU_FFT_PRT_TEST = $(CURRENTDIR)/test_rocFFT_cpu_FFTs_prt
TARGET_ROCFFT_CPU_FFT_RTP_TEST = $(CURRENTDIR)/test_rocFFT_cpu_FFTs_rtp


TARGETS= \
$(TARGET_PRT_FFTPACK_TEST) \
\
$(TARGET_ROCFFT_CPU_FFT_PRT_TEST) \
$(TARGET_ROCFFT_CPU_FFT_RTP_TEST)

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_PRT_FFTPACK_TEST='$(TARGET_PRT_FFTPACK_TEST)' \
		TARGET_ROCFFT_CPU_FFT_PRT_TEST='$(TARGET_ROCFFT_CPU_FFT_PRT_TEST)' \
		TARGET_ROCFFT_CPU_FFT_RTP_TEST='$(TARGET_ROCFFT_CPU_FFT_RTP_TEST)' \
		$(TARGETS)

test: dataclean targets
	$(TARGET_PRT_FFTPACK_TEST)
	
	sed -e s/OpenMP_rocFFT/OpenMP_rocFFT/g control_rocFFT_base > control_tmp
	sed -e s/FFTPACK/ISPACK/g control_tmp > control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_PRT_TEST) control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_RTP_TEST) control_rocFFT
	
	sed -e s/FFTPACK/ISPACK3/g control_tmp > control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_PRT_TEST) control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_RTP_TEST) control_rocFFT
	
	sed -e s/FFTPACK/FFTW3/g control_tmp > control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_PRT_TEST) control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_RTP_TEST) control_rocFFT
	
	sed -e s/FFTPACK/OpenMP_FFTW/g control_tmp > control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_RTP_TEST) control_rocFFT
	
	cp control_tmp control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_PRT_TEST) control_rocFFT
	$(TARGET_ROCFFT_CPU_FFT_RTP_TEST) control_rocFFT
	
	@echo 'Finish rocFFT and FFTPACK sharing'


dataclean:
	rm -f *.dat

clean: dataclean
	rm control_tmp control_rocFFT
	@for target in $(TARGETS); do \
		(echo $${target}; rm -fr $${target}*) \
	done

destclean: clean