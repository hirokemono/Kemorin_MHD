#
#
#
TESTS_DIR = $$(PROG_DIR)/TESTS

SUBDIRS =      \
mesh_test      \
comm_test      \
test_gaussian  \
test_solver    \
\
test_schmidt_poly \
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
		make \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done

target_task:
	@echo tests_with_f: fem_mesh_test comm_tests gauss_tests solver_tests \
	f_graphic_tests fft_test lag_tests getarg_tests \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo tests_with_c: zlib_tests c_graphic_tests \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done


lib_name:
	@for dir in  $(SUBDIRS) ; do \
		( \
		echo 'generate library list for ' $$dir ; \
		cd $${dir};  \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		make lib_name; \
		 ) \
	done


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done

module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
