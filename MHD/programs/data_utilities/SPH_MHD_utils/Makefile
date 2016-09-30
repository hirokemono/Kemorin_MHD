#
#
#

SPH_SNAPSHOT_DIR =   $$(DATA_UTILS_DIR)/SPH_MHD_utils

TARGET_SPH_SNAP =         sph_snapshot
TARGET_SPH_PSF_SNAP =     sph_snapshot_psf
TARGET_SPH_ZM_SNAP =      sph_zm_snapshot
TARGET_SPH_ZRMS_SNAP =    sph_zonal_rms_snapshot
TARGET_SPH_SNAP_BADBOY =  sph_snapshot_badboy

TARGET_SPH_SP_SNAP =    sph_special_snap
TARGET_MODIFY_SPH_INI = sph_modify_restart

TARGET_SPH_SNAP_NOVIZ =    sph_snapshot_noviz
TARGET_SPH_ZM_SNAP_NOVIZ = sph_zm_snapshot_noviz

TARGET_SPH_D_BENCH =         sph_dynamobench
TARGET_SPH_PICK_CIRC =       sph_pick_circle
TARGET_TAVE_PICK_CIRCLE =    t_ave_sph_picked_circle


LIBS_SPH_UTILS = \
'$$(LIB_MHD)'    \
'$$(LIB_VIZ)'     \
'$$(LIB_PARA)'    \
'$$(LIB_SOLVER)'  \
'$$(LIB_MESH)'    \
'$$(LIB_FFTPACK)' \
'$$(LIB_ISPACK)'


LIB_FILES_SPH_UTILS = \
'$$(LIB_MHD_FILE)'    \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'   \
'$$(LIB_FFTPACK_FILE)' \
'$$(LIB_ISPACK_FILE)'


ifdef FORTRAN_C_CONNECT
  LIBS_SPH_UTILS+=        '$$(LIB_KEMO_C)'
  LIB_FILES_SPH_UTILS+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_SPH_SNAP_MAIN =  \
main_sph_snapshot.o \
analyzer_sph_snap.o \
SPH_analyzer_snap.o

MOD_SPH_SNAP_PSF_MAIN =  \
main_sph_snapshot_w_psf.o \
analyzer_sph_snap_w_psf.o \
SPH_analyzer_snap.o

MOD_SPH_ZM_SNAP_MAIN =  \
main_sph_zm_snap.o \
analyzer_sph_zm_snap.o \
SPH_analyzer_snap.o \
SPH_analyzer_zm_snap.o

MOD_SPH_ZRMS_SNAP_MAIN =  \
main_sph_zonal_rms_snap.o \
analyzer_sph_zonal_rms_snap.o \
SPH_analyzer_snap.o \
SPH_analyzer_zrms_snap.o

MOD_SPH_SNAP_BADBOY =  \
main_sph_snap_badboy.o \
analyzer_sph_snap.o \
analyzer_sph_snap_badboy.o \
SPH_analyzer_snap.o


MOD_SPH_SP_SNAP_MAIN = \
main_sph_special_snap.o \
analyzer_sph_special_snap.o \
analyzer_sph_snap.o \
SPH_analyzer_snap.o


MOD_SPH_SNAP_NOVIZ_MAIN =  \
main_sph_snapshot_noviz.o \
analyzer_noviz_sph_snap.o \
SPH_analyzer_snap.o

MOD_SPH_ZM_SNAP_NOVIZ_MAIN =  \
main_sph_zm_snap_noviz.o \
analyzer_noviz_sph_zm_snap.o \
SPH_analyzer_snap.o \
SPH_analyzer_zm_snap.o

MOD_SPH_D_BENCH_MAIN = \
main_sph_dynamobench.o \
analyzer_sph_dynamobench.o \
SPH_analyzer_d_bench.o

MOD_SPH_PICK_CIRC = \
main_sph_pickup_cricle.o \
analyzer_sph_pickup_circle.o \
SPH_analyzer_sph_pick_circ.o

MOD_T_AVE_PICK_CIRCLE = \
t_ave_sph_picked_circle.o \
m_tave_field_on_circle.o

MOD_MODIFY_SPH_INI = \
main_sph_modify_restart.o \
analyzer_sph_modify_restart.o \
analyzer_sph_snap.o \
SPH_analyzer_snap.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_SNAPSHOT_DIR = $(SPH_SNAPSHOT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_SNAP =       $$(BUILDDIR)/$(TARGET_SPH_SNAP)'       >> $(MAKENAME)
	@echo 'TARGET_SPH_PSF_SNAP =   $$(BUILDDIR)/$(TARGET_SPH_PSF_SNAP)'   >> $(MAKENAME)
	@echo 'TARGET_SPH_SP_SNAP =    $$(BUILDDIR)/$(TARGET_SPH_SP_SNAP)'    >> $(MAKENAME)
	@echo 'TARGET_MODIFY_SPH_INI = $$(BUILDDIR)/$(TARGET_MODIFY_SPH_INI)' >> $(MAKENAME)
	@echo 'TARGET_SPH_ZM_SNAP =    $$(BUILDDIR)/$(TARGET_SPH_ZM_SNAP)'   >> $(MAKENAME)
	@echo 'TARGET_SPH_ZRMS_SNAP =  $$(BUILDDIR)/$(TARGET_SPH_ZRMS_SNAP)' >> $(MAKENAME)
	@echo 'TARGET_SPH_D_BENCH =    $$(BUILDDIR)/$(TARGET_SPH_D_BENCH)'   >> $(MAKENAME)
	@echo 'TARGET_SPH_PICK_CIRC =  $$(BUILDDIR)/$(TARGET_SPH_PICK_CIRC)' >> $(MAKENAME)
	@echo 'TARGET_SPH_SNAP_NOVIZ = $$(BUILDDIR)/$(TARGET_SPH_SNAP_NOVIZ)'>> $(MAKENAME)
	@echo 'TARGET_SPH_SNAP_BADBOY = $$(BUILDDIR)/$(TARGET_SPH_SNAP_BADBOY)' >> $(MAKENAME)
	@echo 'TARGET_SPH_ZM_SNAP_NOVIZ = $$(BUILDDIR)/$(TARGET_SPH_ZM_SNAP_NOVIZ)' >> $(MAKENAME)
	@echo 'TARGET_TAVE_PICK_CIRCLE =  $$(BUILDDIR)/$(TARGET_TAVE_PICK_CIRCLE)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_utils: \
	 '$$(TARGET_SPH_SNAP)'       \
	 '$$(TARGET_SPH_PSF_SNAP)'   \
	 '$$(TARGET_SPH_ZM_SNAP)'    \
	 '$$(TARGET_SPH_ZRMS_SNAP)'  \
	 '$$(TARGET_SPH_SP_SNAP)'    \
	 '$$(TARGET_MODIFY_SPH_INI)' \
	 '$$(TARGET_SPH_D_BENCH)'    \
	 '$$(TARGET_SPH_PICK_CIRC)'  \
	 '$$(TARGET_SPH_SNAP_NOVIZ)' \
	 '$$(TARGET_SPH_ZM_SNAP_NOVIZ)' \
	 '$$(TARGET_SPH_SNAP_BADBOY)'   \
	 '$$(TARGET_TAVE_PICK_CIRCLE)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_SNAP):  $$(MOD_SPH_SNAP_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SNAP)' '$$(MOD_SPH_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_PSF_SNAP):  $$(MOD_SPH_SNAP_PSF_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_PSF_SNAP)' '$$(MOD_SPH_SNAP_PSF_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SNAP_NOVIZ):  $$(MOD_SPH_SNAP_NOVIZ_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SNAP_NOVIZ)' '$$(MOD_SPH_SNAP_NOVIZ_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_ZM_SNAP):  $$(MOD_SPH_ZM_SNAP_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ZM_SNAP)' '$$(MOD_SPH_ZM_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_ZRMS_SNAP):  $$(MOD_SPH_ZRMS_SNAP_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ZRMS_SNAP)' '$$(MOD_SPH_ZRMS_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_ZM_SNAP_NOVIZ):  $$(MOD_SPH_ZM_SNAP_NOVIZ_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ZM_SNAP_NOVIZ)' '$$(MOD_SPH_ZM_SNAP_NOVIZ_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_D_BENCH):  $$(MOD_SPH_D_BENCH_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_D_BENCH)' '$$(MOD_SPH_D_BENCH_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_PICK_CIRC):  $$(MOD_SPH_PICK_CIRC)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_PICK_CIRC)' '$$(MOD_SPH_PICK_CIRC)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SP_SNAP):  $$(MOD_SPH_SP_SNAP_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SP_SNAP)' '$$(MOD_SPH_SP_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_MODIFY_SPH_INI):  $$(MOD_MODIFY_SPH_INI)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MODIFY_SPH_INI)' '$$(MOD_MODIFY_SPH_INI)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SNAP_BADBOY):  $$(MOD_SPH_SNAP_BADBOY)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SNAP_BADBOY)' '$$(MOD_SPH_SNAP_BADBOY)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_TAVE_PICK_CIRCLE):  $$(MOD_T_AVE_PICK_CIRCLE)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_TAVE_PICK_CIRCLE)' '$$(MOD_T_AVE_PICK_CIRCLE)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_SPH_UTILS=  \\          >> $(MAKENAME)
	@echo  $(LIBS_SPH_UTILS)            >> $(MAKENAME)
	@echo  LIB_FILES_SPH_UTILS=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_SPH_UTILS)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


mod_list:
	@echo  MOD_SPH_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_PSF_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_PSF_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SP_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SP_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_ZM_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ZM_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_ZRMS_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ZRMS_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_ZM_SNAP_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ZM_SNAP_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_BADBOY=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_BADBOY) >> $(MAKENAME)
	@echo  MOD_SPH_D_BENCH_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_D_BENCH_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_PICK_CIRC=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_PICK_CIRC) >> $(MAKENAME)
	@echo  MOD_T_AVE_PICK_CIRCLE=  \\ >> $(MAKENAME)
	@echo  $(MOD_T_AVE_PICK_CIRCLE) >> $(MAKENAME)
	@echo  MOD_MODIFY_SPH_INI=  \\ >> $(MAKENAME)
	@echo  $(MOD_MODIFY_SPH_INI) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_SNAPSHOT_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
