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

TARGET_ROCM_DGEMM_TEST = $(CURRENTDIR)/test_rocBLAS_DGEMM

TARGETS= \
$(TARGET_ROCM_DGEMM_TEST)

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_ROCM_DGEMM_TEST='$(TARGET_ROCM_DGEMM_TEST)' \
		$(TARGETS)

test: dataclean targets
	$(TARGET_ROCM_DGEMM_TEST)

dataclean:
	rm -f *.dat

clean: dataclean
	@for target in $(TARGETS); do \
		(rm -fr $${target}*) \
	done

destclean: clean