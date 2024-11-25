#
#
#

PROG_DIR = $$(MHDDIR)/programs

LIB_PYTHONS =      '-L$$(PYTHONDIR) -lcalypso_to_pythons'
LIB_FILE_PYTHONS = '$$(PYTHONDIR)/libcalypso_to_pythons.so'

MOD_PYTHONS = \
'$$(MOD_MHD_F2003)' \
'$$(MOD_VIZ_F2003)' \
'$$(MOD_SPH_UTIL_DLL)' \
'$$(MOD_MHD_CTL_DLL)' \
'$$(MOD_VIZ_CTL_DLL)'

SUBDIRS = \
FEM_MHD \
SPH_MHD \
VIZ_only \
interpolate \
construct_filter \
\
mesh_utilities \
data_utilities \
\
Rayleigh_link \
\
TESTS

DLIBDIRS = \
data_utilities

ifdef OPENGL_LIBS
  SUBDIRS += VIEWER
endif

LIB_PYTHON_UTILS =  \
'$$(LIB_MESH)'

LIB_FILES_PYTHON_UTILS =  \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIB_PYTHON_UTILS+=        '$$(LIB_KEMO_C)'
  LIB_FILES_PYTHON_UTILS+=  '$$(LIB_KEMO_C_FILE)'
endif

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
	@echo 'parallels: '            \
	'fem_mhd  sph_mhd visualizer'  \
	'const_filteres itp_tools'     >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'utilities: '            \
	'data_utils mesh_utils'        \
	'rayleigh_link'                >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo 'FEM_minimum: '          \
	'fem_mhd cubed_sph_utils '     \
	'partitioners refineres'       >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		FFTW3_LIBS='$(FFTW3_LIBS)' \
		FFTW3_OMP_LIBS='$(FFTW3_OMP_LIBS)' \
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
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate library list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
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
	@for dir in $(DLIBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done
	@echo '#' >> $(MAKENAME)
	@for dir in Rayleigh_link; do \
		( cd $${dir};  \
		echo 'generate library for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		64BIT_INT='$(64BIT_INT)'   \
		lib_tasks \
		) \
	done


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
		FFMPEG_CFLAGS='$(FFMPEG_CFLAGS)' \
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
