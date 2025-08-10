#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
#
MHDDIR =  $(SRCDIR)/MHD
BUILDDIR= $(SRCDIR)/bin
TESTDIR=  $(SRCDIR)/tests
#
# MPI settings
#
MPICHDIR =    
MPICHLIBDIR = $(MPICHDIR)/lib
MPICHBINDIR = $(MPICHDIR)/bin
MPICHINCDIR = 
MPILIBS =      
#

all: test

test: 
	rm -f *.dat log.txt
	$(BUILDDIR)/tests/gauss_points < input.txt
	echo 'Compare section and isosurface data'
	$(BUILDDIR)/tests/compare_gauss_points < compare.txt > log.txt
	tail -n 1 log.txt
	if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Gauss points check'; \
		exit 1;\
	fi

clean:
	rm -f *.dat

distclean: clean
