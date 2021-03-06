#
#
#
TESTS_DIR = $$(PROG_DIR)/TESTS

SUBDIRS =      \
mesh_test      \
comm_test      \
node_clustering_test \
Gauss_point_test  \
test_solver    \
\
Legendre_test \
zlib_test     \
graphic_test  \
FFT_test      \
\
BC_file_test

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'TESTS_DIR =  $(TESTS_DIR)'            >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done

target_task:
	@echo tests_with_f: fem_mesh_test comm_tests gauss_tests solver_tests \
	f_graphic_tests fft_test leg_tests getarg_tests node_clustring_test \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo tests_with_c: zlib_tests c_graphic_tests \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FFTW3_LIBS='$(FFTW3_LIBS)' \
		FFTW3_OMP_LIBS='$(FFTW3_OMP_LIBS)' \
		target_task \
		) \
	done


lib_name:
	@for dir in  $(SUBDIRS) ; do \
		( \
		echo 'generate library list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		lib_name; \
		 ) \
	done


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done

module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct dependencies for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		CC='$(CC)'   \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MHDDIR='$(MHDDIR)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		DEPFLAGS='$(DEPFLAGS)' \
		OPTFLAGS='$(OPTFLAGS)' \
		C_INCLUDE='$(C_INCLUDE)' \
		depends; \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
