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

TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_FFTPACK_prt
TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_FFTPACK_rtp
TARGET_OMP_ROCFFT_ISPACK1_PRT_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_ISPACK1_prt
TARGET_OMP_ROCFFT_ISPACK1_RTP_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_ISPACK1_rtp
TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_ISPACK3_prt
TARGET_OMP_ROCFFT_ISPACK3_RTP_TEST =  $(CURRENTDIR)/test_OMP_rocFFT_ISPACK3_rtp
TARGET_OMP_ROCFFT_FFTW_PRT_TEST =     $(CURRENTDIR)/test_OMP_rocFFT_FFTW_prt
TARGET_OMP_ROCFFT_FFTW_RTP_TEST =     $(CURRENTDIR)/test_OMP_rocFFT_FFTW_rtp
TARGET_OMP_ROCFFT_OMP_FFTW_RTP_TEST = $(CURRENTDIR)/test_OMP_rocFFT_OMP_FFTW_rtp


TARGETS= \
$(TARGET_CMPR_FFT_TEST) \
$(TARGET_PRT_FFTPACK_TEST) \
\
$(TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST) \
$(TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST) \
$(TARGET_OMP_ROCFFT_ISPACK1_PRT_TEST) \
$(TARGET_OMP_ROCFFT_ISPACK1_RTP_TEST) \
$(TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST) \
$(TARGET_OMP_ROCFFT_ISPACK3_RTP_TEST) \
$(TARGET_OMP_ROCFFT_FFTW_PRT_TEST) \
$(TARGET_OMP_ROCFFT_FFTW_RTP_TEST) \
$(TARGET_OMP_ROCFFT_OMP_FFTW_RTP_TEST)

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_CMPR_FFT_TEST='$(TARGET_CMPR_FFT_TEST)' \
		TARGET_PRT_FFTPACK_TEST='$(TARGET_PRT_FFTPACK_TEST)' \
		TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST='$(TARGET_OMP_ROCFFT_FFTPACK_PRT_TEST)' \
		TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST='$(TARGET_OMP_ROCFFT_FFTPACK_RTP_TEST)' \
		TARGET_OMP_ROCFFT_ISPACK1_PRT_TEST='$(TARGET_OMP_ROCFFT_ISPACK1_PRT_TEST)' \
		TARGET_OMP_ROCFFT_ISPACK1_RTP_TEST='$(TARGET_OMP_ROCFFT_ISPACK1_RTP_TEST)' \
		TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST='$(TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST)' \
		TARGET_OMP_ROCFFT_ISPACK3_RTP_TEST='$(TARGET_OMP_ROCFFT_ISPACK3_RTP_TEST)' \
		TARGET_OMP_ROCFFT_FFTW_PRT_TEST='$(TARGET_OMP_ROCFFT_FFTW_PRT_TEST)' \
		TARGET_OMP_ROCFFT_FFTW_RTP_TEST='$(TARGET_OMP_ROCFFT_FFTW_RTP_TEST)' \
		TARGET_OMP_ROCFFT_OMP_FFTW_RTP_TEST='$(TARGET_OMP_ROCFFT_OMP_FFTW_RTP_TEST)' \
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
	
	$(TARGET_OMP_ROCFFT_ISPACK1_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_OpenMP_rocFFT_ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside OpenMP rocFFT and ISPACK1 check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_ISPACK1_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_OpenMP_rocFFT_ISPACK1_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside OpenMP rocFFT and ISPACK1 check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_ISPACK3_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_OpenMP_rocFFT_ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside OpenMP rocFFT and ISPACK3 check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_ISPACK3_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_OpenMP_rocFFT_ISPACK3_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside OpenMP rocFFT and ISPACK3 check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_FFTW_PRT_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'prt_OpenMP_rocFFT_FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside OpenMP rocFFT and FFTW check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_FFTW_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_OpenMP_rocFFT_FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside OpenMP rocFFT and FFTW check'; \
		exit 1;\
	fi
	
	$(TARGET_OMP_ROCFFT_OMP_FFTW_RTP_TEST)
	@$(TARGET_CMPR_FFT_TEST) 'prt_fftpack_test.dat' 'rtp_OpenMP_rocFFT_OFFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside OpenMP rocFFT and OpenMP FFTW check'; \
		exit 1;\
	fi


dataclean:
	rm -f *.dat

clean: dataclean
	@for target in $(TARGETS); do \
		(echo $${target}; rm -fr $${target}*) \
	done

destclean: clean
