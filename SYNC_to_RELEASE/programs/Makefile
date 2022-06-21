#
#
#

PROG_DIR = $$(MHDDIR)/programs

LIB_PYTHONS =      '-L$$(PYTHONDIR) -lcalypso_to_pythons'
LIB_FILE_PYTHONS = '$$(PYTHONDIR)/libcalypso_to_pythons.so'

SUBDIRS = \
SPH_MHD \
\
mesh_utilities \
data_utilities \
\
TESTS

DLIBDIRS = \
data_utilities

LIBS_SPH_MHD = \
'$$(LIB_CALYPSO)'  \
'$$(LIB_CALYPSO_C)'  \
'$$(LIB_FFTPACK)'    \
'$$(LIB_NETLIB)'

LIBS_MESH_UTILS = \
'$$(LIB_CALYPSO)'  \
'$$(LIB_CALYPSO_C)'

LIB_FILES_SPH_MHD = \
'$$(LIB_CALYPSO_FILE)'   \
'$$(LIB_CALYPSO_C_FILE)' \
'$$(LIB_FFTPACK_FILE)'   \
'$$(LIB_NETLIB_FILE)'

LIB_FILES_MESH_UTILS = \
'$$(LIB_CALYPSO_FILE)'   \
'$$(LIB_CALYPSO_C_FILE)'


LIB_PYTHON_UTILS =  \
'$$(LIB_CALYPSO)'

LIB_FILES_PYTHON_UTILS =  \
'$$(LIB_CALYPSO_FILE)'

MOD_PYTHONS = \
'$$(MOD_SPH_UTIL_DLL)'

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PROG_DIR = $(PROG_DIR)'     >> $(MAKENAME)
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
	@echo 'parallels: sph_mhd'      \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'utilities: data_utils mesh_utils sph_data_util' >> $(MAKENAME)
	@echo 'tests: tests_with_f' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FFTW3_LIBS='$(FFTW3_LIBS)' \
		target_task \
		) \
	done


lib_name:
	@echo  LIB_PYTHONS=  \\           >> $(MAKENAME)
	@echo  $(LIB_PYTHONS)             >> $(MAKENAME)
	@echo  LIB_FILE_PYTHONS=  \\           >> $(MAKENAME)
	@echo  $(LIB_FILE_PYTHONS)             >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIB_PYTHON_UTILS=  \\           >> $(MAKENAME)
	@echo  $(LIB_PYTHON_UTILS)             >> $(MAKENAME)
	@echo  LIB_FILES_PYTHON_UTILS=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_PYTHON_UTILS)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_SPH_MHD=  \\          >> $(MAKENAME)
	@echo  $(LIBS_SPH_MHD)            >> $(MAKENAME)
	@echo  LIB_FILES_SPH_MHD=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_SPH_MHD)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_MESH_UTILS=  \\          >> $(MAKENAME)
	@echo  $(LIBS_MESH_UTILS)            >> $(MAKENAME)
	@echo  LIB_FILES_MESH_UTILS=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_MESH_UTILS)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate library list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_name \
		) \
	done

lib_tasks:
	@echo '#' >> $(MAKENAME)
	@echo '$$(LIB_FILE_PYTHONS)': '$$(MOD_PYTHONS)' \
	'$$(LIB_FILES_PYTHON_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -shared -fPIC '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(LIB_FILE_PYTHONS)' \
	'$$(MOD_PYTHONS)' \
	'-L. $$(LIB_PYTHON_UTILS)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


mod_list:
	@echo  MOD_PYTHONS=  \\     >> $(MAKENAME)
	@echo  $(MOD_PYTHONS)       >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
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
		echo 'generate compile list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate dependencies for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
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
