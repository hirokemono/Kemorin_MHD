#
#
#

MHD_OUT_DIR  = $$(PROG_DIR)/FEM_MHD
FEM_BUILDDIR = $$(BUILDDIR)/FEM

SUBDIRS = \
fem_mhd                 \
fem_snapshot            \
fem_filtered_snapshot   \
fem_snapshot_tmp        \
fem_volume_average      \
check_FEM_MHD_matrices

LIBS_FEM_MHD = \
'$$(LIB_MHD)'    \
'$$(LIB_VIZ)'    \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'   \
'$$(LIB_MT_STREAM)'


LIB_FILES_FEM_MHD = \
'$$(LIB_MHD_FILE)'    \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'   \
'$$(LIB_MT_STREAM_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_FEM_MHD+=        '$$(LIB_KEMO_C)'
  LIB_FILES_FEM_MHD+=   '$$(LIB_KEMO_C_FILE)'
endif

#
#  -------------------------------------------------------------------------
#

dir_list:
	@mkdir -p $(BUILDDIR)/FEM
	@echo 'FEM_BUILDDIR = $(FEM_BUILDDIR)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'MHD_OUT_DIR = $(MHD_OUT_DIR)' >>   $(MAKENAME)
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
	@echo '#' >> $(MAKENAME)
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
	@echo fem_mhd_targets:  \
	'$$(TARGET_FEM_MHD)' \
	'$$(TARGET_FEM_SNAPSHOT)'    \
	'$$(TARGET_FEM_SNAP_FILTER)' \
	'$$(TARGET_FEM_SNAP_TMP)'    \
	'$$(TARGET_FEM_MHD_MAT)'     \
	'$$(TARGET_FEM_SNAP_VOL_AVE)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		FORTRAN_C_CONNECT='$(FORTRAN_C_CONNECT)' \
		FFTW3_LIBS='$(FFTW3_LIBS)' \
		FFTW3_OMP_LIBS='$(FFTW3_OMP_LIBS)' \
		ROCM_CFLAGS='$(ROCM_CFLAGS)' \
		target_task \
		) \
	done


lib_name:
	@echo  LIBS_FEM_MHD=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FEM_MHD)            >> $(MAKENAME)
	@echo  LIB_FILES_FEM_MHD=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FEM_MHD)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct compile list for ' $$dir ; \
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
		ROCM_CFLAGS='$(ROCM_CFLAGS)' \
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
	
	rm -f *.o *~ *.mod *.diag *.L
