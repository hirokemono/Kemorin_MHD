#
#
#

MHD_OUT_DIR =       $$(PROG_DIR)/FEM_MHD

TARGET = fem_mhd

TARGET_SNAP=    fem_snapshot
TARGET_FILTER=  filtered_snap
TARGET_VOL_AVE= volume_average
TARGET_MHD_MAT= check_mhd_matrix

TARGET_SNAP_TMP = snap_works

LIBS_MHD = \
'$$(LIB_MHD)'    \
'$$(LIB_VIZ)'    \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_MHD = \
'$$(LIB_MHD_FILE)'    \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_MHD+=        '$$(LIB_KEMO_C)'
  LIB_FILES_MHD+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_MHD_PLUG =  \
main_MHD.o \
analyzer_MHD.o \
FEM_analyzer_MHD.o

MOD_MHD_MAT =  \
main_MHD_matrices.o \
analyzer_check_mat_MHD.o \
FEM_check_MHD_matrices.o


MOD_SNAP_PLUG = \
main_snapshot.o \
analyzer_snap.o \
FEM_analyzer_snapshot.o


MOD_FILTERING_ALL = \
main_filtering.o \
analyzer_filtering.o \
FEM_analyzer_snapshot.o \
FEM_analyzer_filtered.o \
\
filter_all_fields.o

MOD_SNAP_VOL_AVE = \
main_FEM_volume_ave.o \
analyzer_volume_ave.o \
FEM_analyzer_vol_average.o

MOD_SNAP_TMP = \
main_snap_tmp.o \
analyzer_snap_tmp.o \
FEM_analyzer_snap_tmp.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_OUT_DIR = $(MHD_OUT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET = $$(BUILDDIR)/$(TARGET)' >> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo 'TARGET_SNAP =    $$(BUILDDIR)/$(TARGET_SNAP)'    >> $(MAKENAME)
	@echo 'TARGET_FILTER =  $$(BUILDDIR)/$(TARGET_FILTER)'  >> $(MAKENAME)
	@echo 'TARGET_VOL_AVE = $$(BUILDDIR)/$(TARGET_VOL_AVE)' >> $(MAKENAME)
	@echo 'TARGET_MHD_MAT = $$(BUILDDIR)/$(TARGET_MHD_MAT)' >> $(MAKENAME)
	@echo 'TARGET_SNAP_TMP =  $$(BUILDDIR)/$(TARGET_SNAP_TMP)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo fem_mhd:  \
	'$$(TARGET)'         \
	'$$(TARGET_SNAP)'    \
	'$$(TARGET_FILTER)'  \
	'$$(TARGET_MHD_MAT)' \
	'$$(TARGET_SNAP_TMP)' \
	'$$(TARGET_VOL_AVE)'     >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET):  $$(MOD_MHD_PLUG)  $$(LIB_FILES_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET)' '$$(MOD_MHD_PLUG)' \
	'-L. $$(LIBS_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SNAP)': '$$(MOD_SNAP_PLUG)' '$$(LIB_FILES_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SNAP)' \
	'$$(MOD_SNAP_PLUG) -L.  $$(LIBS_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SNAP_TMP)': '$$(MOD_SNAP_TMP)' '$$(LIB_FILES_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SNAP_TMP)' \
	'$$(MOD_SNAP_TMP) -L.  $$(LIBS_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FILTER)': '$$(MOD_FILTERING_ALL) $$(LIB_FILES_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_FILTER)' \
	'$$(MOD_FILTERING_ALL) -L. $$(LIBS_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_VOL_AVE)': '$$(MOD_SNAP_VOL_AVE) $$(LIB_FILES_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_VOL_AVE)'  \
	'$$(MOD_SNAP_VOL_AVE) -L. $$(LIBS_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MHD_MAT)': '$$(MOD_MHD_MAT) $$(LIB_FILES_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MHD_MAT)' \
	'$$(MOD_MHD_MAT) -L. $$(LIBS_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_MHD=  \\          >> $(MAKENAME)
	@echo  $(LIBS_MHD)            >> $(MAKENAME)
	@echo  LIB_FILES_MHD=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_MHD)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_MHD_PLUG=  \\ >> $(MAKENAME)
	@echo  $(MOD_MHD_PLUG) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  MOD_SNAP_PLUG=  \\     >> $(MAKENAME)
	@echo  $(MOD_SNAP_PLUG)       >> $(MAKENAME)
	@echo  MOD_FILTERING_ALL=  \\ >> $(MAKENAME)
	@echo  $(MOD_FILTERING_ALL)   >> $(MAKENAME)
	@echo  MOD_SNAP_VOL_AVE=  \\  >> $(MAKENAME)
	@echo  $(MOD_SNAP_VOL_AVE)    >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  MOD_MHD_MAT=  \\       >> $(MAKENAME)
	@echo  $(MOD_MHD_MAT)         >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  MOD_SNAP_TMP=  \\      >> $(MAKENAME)
	@echo  $(MOD_SNAP_TMP)        >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_OUT_DIR)' $(SOURCES)


clean:
	rm -f *.o *~ *.mod *.diag *.L
