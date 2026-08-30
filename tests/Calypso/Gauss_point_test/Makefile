#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = ~/src_kemo
MAKEDIR = $(SRCDIR)/work

TESTS_DIR =      $(SRCDIR)/MHD/programs/TESTS
GAUSS_TEST_DIR = $(TESTS_DIR)/Gauss_point_test

include $(MAKEDIR)/Makefile.header

TARGET_GAUSS_TEST =     ./gauss_points
TARGET_CMP_GAUSS_TEST = ./compare_gauss_points

SRC_GAUSS_TEST =  \
$(GAUSS_TEST_DIR)/test_gauss_points.f90 \
$(GAUSS_TEST_DIR)/gauss_points_IO.f90

SRC_CMP_GAUSS_TEST = \
$(GAUSS_TEST_DIR)/compare_gauss_points.f90 \
$(GAUSS_TEST_DIR)/gauss_points_IO.f90

all: test

$(TARGET_GAUSS_TEST): $(SRC_GAUSS_TEST) $(MAKEDIR)/$(LIB_MESH_FILE)
	$(F90) $(F90OPTFLAGS) -o $@ $(SRC_GAUSS_TEST) \
	-L$(MAKEDIR) $(LIB_MESH) $(F90LIBS)
$(TARGET_CMP_GAUSS_TEST): $(SRC_CMP_GAUSS_TEST) $(MAKEDIR)/$(LIB_MESH_FILE)
	$(F90) $(F90OPTFLAGS) -o $@ $(SRC_CMP_GAUSS_TEST) \
	-L$(MAKEDIR) $(LIB_MESH) $(F90LIBS)

test: $(TARGET_GAUSS_TEST) $(TARGET_CMP_GAUSS_TEST)
	make dataclean
	$(TARGET_GAUSS_TEST) 32
	$(TARGET_CMP_GAUSS_TEST)  reference/gauss_point32.dat gauss_point32.dat > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Gauss points check'; \
		exit 1;\
	fi
	echo 'Gauss points check finished'

dataclean:
	rm -f *.dat log.txt

clean: dataclean
	rm -fr $(TARGET_GAUSS_TEST)* $(TARGET_CMP_GAUSS_TEST)*

distclean: clean
