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
BUILDDIR= $(SRCDIR)/bin

TESTS_DIR =      $(SRCDIR)/MHD/programs/TESTS

#
TARGET_SELECT_PRT_FFT_TEST = $(CURRENTDIR)/test_prt_select_FFT
TARGET_SELECT_RTP_FFT_TEST = $(CURRENTDIR)/test_rtp_select_FFT
TARGET_CMPR_FFT_TEST =       $(CURRENTDIR)/compare_fft_test

TARGETS = \
$(TARGET_SELECT_PRT_FFT_TEST) \
$(TARGET_SELECT_RTP_FFT_TEST) \
$(TARGET_CMPR_FFT_TEST)

$(TARGET_CMPR_FFT_TEST): $(SRC_CMPR_FFT_TEST) $(MAKEDIR)/$(LIB_MESH_FILE)
	$(F90) $(F90OPTFLAGS) -o $@ $(SRC_CMPR_FFT_TEST) \
	-L$(MAKEDIR) $(LIB_MESH) $(F90LIBS)




all:

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SELECT_PRT_FFT_TEST='$(TARGET_SELECT_PRT_FFT_TEST)' \
		TARGET_SELECT_RTP_FFT_TEST='$(TARGET_SELECT_RTP_FFT_TEST)' \
		TARGET_CMPR_FFT_TEST='$(TARGET_CMPR_FFT_TEST)' \
		$(TARGETS)

test: dataclean targets
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

dataclean:
	rm -f *.dat log.txt

clean: dataclean
	rm -fr $(TARGET_SELECT_PRT_FFT_TEST)*
	rm -fr $(TARGET_SELECT_RTP_FFT_TEST)*
	rm -fr $(TARGET_CMPR_FFT_TEST)*

distclean: clean
