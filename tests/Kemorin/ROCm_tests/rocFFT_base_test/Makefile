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

TARGET_CPLX_ROCFFT_PRT_TEST = $(CURRENTDIR)/test_cplx_rocFFT_prt
TARGET_CPLX_ROCFFT_RTP_TEST = $(CURRENTDIR)/test_cplx_rocFFT_rtp
TARGET_REAL_ROCFFT_PRT_TEST = $(CURRENTDIR)/test_real_rocFFT_prt
TARGET_REAL_ROCFFT_RTP_TEST = $(CURRENTDIR)/test_real_rocFFT_rtp
TARGET_OMP_ROCFFT_PRT_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_prt
TARGET_OMP_ROCFFT_RTP_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_rtp

TARGET_SGL_CPLX_ROCFFT_PRT_TEST = $(CURRENTDIR)/test_sgl_cplx_rocFFT_prt
TARGET_SGL_CPLX_ROCFFT_RTP_TEST = $(CURRENTDIR)/test_sgl_cplx_rocFFT_rtp
TARGET_SGL_REAL_ROCFFT_PRT_TEST = $(CURRENTDIR)/test_sgl_real_rocFFT_prt
TARGET_SGL_REAL_ROCFFT_RTP_TEST = $(CURRENTDIR)/test_sgl_real_rocFFT_rtp


TARGETS= \
$(TARGET_CMPR_FFT_TEST) \
$(TARGET_PRT_FFTPACK_TEST) \
\
$(TARGET_CPLX_ROCFFT_PRT_TEST) \
$(TARGET_CPLX_ROCFFT_RTP_TEST) \
$(TARGET_REAL_ROCFFT_PRT_TEST) \
$(TARGET_REAL_ROCFFT_RTP_TEST) \
$(TARGET_OMP_ROCFFT_PRT_TEST) \
$(TARGET_OMP_ROCFFT_RTP_TEST) \
\
$(TARGET_SGL_CPLX_ROCFFT_PRT_TEST) \
$(TARGET_SGL_CPLX_ROCFFT_RTP_TEST) \
$(TARGET_SGL_REAL_ROCFFT_PRT_TEST) \
$(TARGET_SGL_REAL_ROCFFT_RTP_TEST)


all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_CMPR_FFT_TEST='$(TARGET_CMPR_FFT_TEST)' \
		TARGET_PRT_FFTPACK_TEST='$(TARGET_PRT_FFTPACK_TEST)' \
		TARGET_CPLX_ROCFFT_PRT_TEST='$(TARGET_CPLX_ROCFFT_PRT_TEST)' \
		TARGET_CPLX_ROCFFT_RTP_TEST='$(TARGET_CPLX_ROCFFT_RTP_TEST)' \
		TARGET_REAL_ROCFFT_PRT_TEST='$(TARGET_REAL_ROCFFT_PRT_TEST)' \
		TARGET_REAL_ROCFFT_RTP_TEST='$(TARGET_REAL_ROCFFT_RTP_TEST)' \
		TARGET_OMP_ROCFFT_PRT_TEST='$(TARGET_OMP_ROCFFT_PRT_TEST)' \
		TARGET_OMP_ROCFFT_RTP_TEST='$(TARGET_OMP_ROCFFT_RTP_TEST)' \
		TARGET_SGL_CPLX_ROCFFT_PRT_TEST='$(TARGET_SGL_CPLX_ROCFFT_PRT_TEST)' \
		TARGET_SGL_CPLX_ROCFFT_RTP_TEST='$(TARGET_SGL_CPLX_ROCFFT_RTP_TEST)' \
		TARGET_SGL_REAL_ROCFFT_PRT_TEST='$(TARGET_SGL_REAL_ROCFFT_PRT_TEST)' \
		TARGET_SGL_REAL_ROCFFT_RTP_TEST='$(TARGET_SGL_REAL_ROCFFT_RTP_TEST)' \
		$(TARGETS)

test: dataclean targets
	$(TARGET_PRT_FFTPACK_TEST)
	
	$(TARGET_CPLX_ROCFFT_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside complex rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_CPLX_ROCFFT_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside complex rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_REAL_ROCFFT_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_real_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside real rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_REAL_ROCFFT_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_real_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside real rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_OMP_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside OpenMP rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_OMP_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside OpenMP rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_SGL_CPLX_ROCFFT_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'sgl_prt_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside single rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_SGL_CPLX_ROCFFT_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'sgl_rtp_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside single rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_SGL_REAL_ROCFFT_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'sgl_prt_real_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside single rocFFT check'; \
		exit 1;\
	fi
	
	$(TARGET_SGL_REAL_ROCFFT_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'sgl_rtp_real_rocFFT_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside single rocFFT check'; \
		exit 1;\
	fi
	
	@echo 'Base rocFFT check finished'

dataclean:
	rm -f *.dat

clean: dataclean
	@for target in $(TARGETS); do \
		(rm -fr $${target}*) \
	done

destclean: clean
