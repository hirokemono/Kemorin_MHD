#
#
#

CORE_SRCDIR = $$(F_SRCDIR)/CORE_src

LIB_MESH =       -lkemo_core
LIB_MESH_FILE = libkemo_core.a

SUBDIRS = \
BASE                 \
MESH                 \
MESH_1st             \
MESH_type            \
IO                   \
IO_BINARY            \
IO_ZLIB              \
IO_MPI               \
CONTROLS             \
\
INTERPOLATE          \
spherical_harmonics  \
gauss_integration    \
jacobian             \
FFT_wrapper          \
SPH_SPECTR_src


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CORE_SRCDIR = $(CORE_SRCDIR)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list; \
		) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_MESH =      $(LIB_MESH)'      >> $(MAKENAME)
	@echo  'LIB_MESH_FILE = $(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_MESH_FILE): $$(MOD_CORE)'>> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve; \
		) \
	done

mod_list:
	@echo  MOD_CORE= \\ >> $(MAKENAME)
	@echo  \
	'$$(MOD_BASE)'       \
	'$$(MOD_CTL)'        \
	'$$(MOD_IO)'         \
	'$$(MOD_BINARY_IO)'  \
	'$$(MOD_ZLIB_IO)'    \
	'$$(MOD_MPI_IO)'     \
	'$$(MOD_MESH)'       \
	'$$(MOD_MESH1)'      \
	'$$(MOD_MESH2)'      \
	'$$(MOD_MESH_T)'     \
	'$$(MOD_GAUSS)'      \
	'$$(MOD_JACOBI)'     \
	'$$(MOD_SPH)'        \
	'$$(MOD_FFT_WRAP)'   \
	'$$(MOD_SPH_SPECTR)' \
	'$$(MOD_ITP)'     >> $(MAKENAME)
	@echo   >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
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
