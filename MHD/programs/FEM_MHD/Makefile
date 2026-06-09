#
#
#

MHD_OUT_DIR  = $$(PROG_DIR)/FEM_MHD
FEM_BUILDDIR = $$(BUILDDIR)/FEM

TARGET_FEM_MHD = fem_mhd

TARGET_SNAP=    fem_snapshot

SUBDIRS = \
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

SOURCES = $(shell ls *.f90)

MOD_MHD_PLUG =  \
main_MHD.o \
analyzer_MHD.o \
FEM_analyzer_MHD.o

MOD_SNAP_PLUG = \
main_snapshot.o \
analyzer_snap.o \
FEM_analyzer_snapshot.o

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
	@echo 'TARGET_FEM_MHD = $$(BUILDDIR)/$(TARGET_FEM_MHD)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'TARGET_SNAP =    $$(BUILDDIR)/$(TARGET_SNAP)'    >> $(MAKENAME)
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
	'$$(TARGET_SNAP)'    \
	'$$(TARGET_FEM_SNAP_FILTER)' \
	'$$(TARGET_FEM_SNAP_TMP)'    \
	'$$(TARGET_FEM_MHD_MAT)'     \
	'$$(TARGET_FEM_SNAP_VOL_AVE)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FEM_MHD):  $$(MOD_MHD_PLUG)  $$(LIB_FILES_FEM_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_MHD)' '$$(MOD_MHD_PLUG)' \
	'-L. $$(LIBS_FEM_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SNAP)': '$$(MOD_SNAP_PLUG)' '$$(LIB_FILES_FEM_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SNAP)' \
	'$$(MOD_SNAP_PLUG) -L.  $$(LIBS_FEM_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
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

mod_list:
	@echo  MOD_MHD_PLUG=  \\ >> $(MAKENAME)
	@echo  $(MOD_MHD_PLUG) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  MOD_SNAP_PLUG=  \\     >> $(MAKENAME)
	@echo  $(MOD_SNAP_PLUG)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_OUT_DIR)' $(SOURCES)
	@echo  '#' >> $(MAKENAME)
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
