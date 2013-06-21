#
#
#

SPH_MHD_MAINDIR =   $$(PROG_DIR)/SPH_MHD

TARGET_SPH_MHD =        sph_mhd
TARGET_SPH_SNAP =       sph_snapshot
TARGET_SPH_ZM_SNAP =    sph_zm_snapshot

TARGET_SPH_SP_SNAP =    sph_special_snap

TARGET_SPH_MHD_NOVIZ =     sph_mhd_noviz
TARGET_SPH_SNAP_NOVIZ =    sph_snapshot_noviz
TARGET_SPH_ZM_SNAP_NOVIZ = sph_zm_snapshot_noviz

TARGET_SPH_LICV =       sph_licv
TARGET_SPH_MHD_MINI =   sph_mhd_nosnap

TARGET_SPH_D_BENCH =         sph_dynamobench
TARGET_SPH_PICK_CIRC =       sph_pick_circle
TARGET_TAVE_PICK_CIRCLE =   t_ave_sph_picked_circle

TARGET_SPH_INITIAL =   sph_initial_field


LIBS_SPH_MHD = \
'$$(LIB_MHD)'    \
'$$(LIB_VIZ)'     \
'$$(LIB_PARA)'    \
'$$(LIB_SOLVER)'  \
'$$(LIB_MESH)'    \
'$$(LIB_FFTPACK)' \
'$$(LIB_ISPACK)'


LIB_FILES_SPH_MHD = \
'$$(LIB_MHD_FILE)'    \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'   \
'$$(LIB_FFTPACK_FILE)' \
'$$(LIB_ISPACK_FILE)'


ifdef FORTRAN_C_CONNECT
  LIBS_SPH_MHD+=        '$$(LIB_KEMO_C)'
  LIB_FILES_SPH_MHD+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_SPH_MHD_MAIN =  \
main_sph_MHD.o \
analyzer_sph_MHD.o \
FEM_analyzer_sph_MHD.o \
SPH_analyzer_MHD.o

MOD_SPH_SNAP_MAIN =  \
main_sph_snapshot.o \
analyzer_sph_snap.o \
SPH_analyzer_snap.o \
FEM_analyzer_sph_MHD.o

MOD_SPH_ZM_SNAP_MAIN =  \
main_sph_zm_snap.o \
analyzer_sph_zm_snap.o \
SPH_analyzer_snap.o \
SPH_analyzer_zm_snap.o \
FEM_analyzer_sph_MHD.o

MOD_SPH_SP_SNAP_MAIN = \
main_sph_special_snap.o \
analyzer_sph_special_snap.o \
analyzer_sph_snap.o \
SPH_analyzer_snap.o \
FEM_analyzer_sph_MHD.o


MOD_SPH_MHD_NOVIZ_MAIN =  \
main_sph_MHD_noviz.o \
analyzer_noviz_sph_MHD.o \
FEM_analyzer_sph_MHD.o \
SPH_analyzer_MHD.o

MOD_SPH_MINI_MHD_MAIN =  \
main_sph_MHD_only.o \
analyzer_small_sph_MHD.o \
SPH_analyzer_MHD.o

MOD_SPH_SNAP_NOVIZ_MAIN =  \
main_sph_snapshot_noviz.o \
analyzer_noviz_sph_snap.o \
SPH_analyzer_snap.o \
FEM_analyzer_sph_MHD.o

MOD_SPH_LICV_MAIN =  \
main_sph_linear_cv.o \
analyzer_sph_licv.o \
SPH_analyzer_licv.o

MOD_SPH_ZM_SNAP_NOVIZ_MAIN =  \
main_sph_zm_snap_noviz.o \
analyzer_noviz_sph_zm_snap.o \
SPH_analyzer_snap.o \
SPH_analyzer_zm_snap.o \
FEM_analyzer_sph_MHD.o

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

MOD_SPH_INITIAL = \
main_sph_initial_fld.o \
analyzer_sph_initial_fld.o \
SPH_analyzer_const_initial.o \
const_sph_initial_spectr.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_MHD_MAINDIR = $(SPH_MHD_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_MHD =        $$(BUILDDIR)/$(TARGET_SPH_MHD)'       >> $(MAKENAME)
	@echo 'TARGET_SPH_LICV =       $$(BUILDDIR)/$(TARGET_SPH_LICV)'      >> $(MAKENAME)
	@echo 'TARGET_SPH_SNAP =       $$(BUILDDIR)/$(TARGET_SPH_SNAP)'      >> $(MAKENAME)
	@echo 'TARGET_SPH_SP_SNAP =    $$(BUILDDIR)/$(TARGET_SPH_SP_SNAP)'   >> $(MAKENAME)
	@echo 'TARGET_SPH_ZM_SNAP =    $$(BUILDDIR)/$(TARGET_SPH_ZM_SNAP)'   >> $(MAKENAME)
	@echo 'TARGET_SPH_D_BENCH =    $$(BUILDDIR)/$(TARGET_SPH_D_BENCH)'   >> $(MAKENAME)
	@echo 'TARGET_SPH_PICK_CIRC =  $$(BUILDDIR)/$(TARGET_SPH_PICK_CIRC)' >> $(MAKENAME)
	@echo 'TARGET_SPH_MHD_MINI =   $$(BUILDDIR)/$(TARGET_SPH_MHD_MINI)'  >> $(MAKENAME)
	@echo 'TARGET_SPH_MHD_NOVIZ =  $$(BUILDDIR)/$(TARGET_SPH_MHD_NOVIZ)' >> $(MAKENAME)
	@echo 'TARGET_SPH_SNAP_NOVIZ = $$(BUILDDIR)/$(TARGET_SPH_SNAP_NOVIZ)'>> $(MAKENAME)
	@echo 'TARGET_SPH_ZM_SNAP_NOVIZ = $$(BUILDDIR)/$(TARGET_SPH_ZM_SNAP_NOVIZ)' >> $(MAKENAME)
	@echo 'TARGET_TAVE_PICK_CIRCLE =  $$(BUILDDIR)/$(TARGET_TAVE_PICK_CIRCLE)' >> $(MAKENAME)
	@echo 'TARGET_SPH_INITIAL =       $$(BUILDDIR)/$(TARGET_SPH_INITIAL)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_mhd: \
	 '$$(TARGET_SPH_MHD)'        \
	 '$$(TARGET_SPH_SNAP)'       \
	 '$$(TARGET_SPH_ZM_SNAP)'    \
	 '$$(TARGET_SPH_SP_SNAP)'    \
	 '$$(TARGET_SPH_D_BENCH)'    \
	 '$$(TARGET_SPH_PICK_CIRC)'  \
	 '$$(TARGET_SPH_LICV)'       \
	 '$$(TARGET_SPH_MHD_NOVIZ)'  \
	 '$$(TARGET_SPH_SNAP_NOVIZ)' \
	 '$$(TARGET_SPH_ZM_SNAP_NOVIZ)' \
	 '$$(TARGET_SPH_INITIAL)'       \
	 '$$(TARGET_TAVE_PICK_CIRCLE)'  \
	 '$$(TARGET_SPH_MHD_MINI)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_MHD): $$(MOD_SPH_MHD_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_MHD)' '$$(MOD_SPH_MHD_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_MHD_NOVIZ): $$(MOD_SPH_MHD_NOVIZ_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_MHD_NOVIZ)' '$$(MOD_SPH_MHD_NOVIZ_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SNAP):  $$(MOD_SPH_SNAP_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SNAP)' '$$(MOD_SPH_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SNAP_NOVIZ):  $$(MOD_SPH_SNAP_NOVIZ_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SNAP_NOVIZ)' '$$(MOD_SPH_SNAP_NOVIZ_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_ZM_SNAP):  $$(MOD_SPH_ZM_SNAP_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ZM_SNAP)' '$$(MOD_SPH_ZM_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_ZM_SNAP_NOVIZ):  $$(MOD_SPH_ZM_SNAP_NOVIZ_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ZM_SNAP_NOVIZ)' '$$(MOD_SPH_ZM_SNAP_NOVIZ_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_D_BENCH):  $$(MOD_SPH_D_BENCH_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_D_BENCH)' '$$(MOD_SPH_D_BENCH_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_PICK_CIRC):  $$(MOD_SPH_PICK_CIRC)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_PICK_CIRC)' '$$(MOD_SPH_PICK_CIRC)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_LICV): $$(MOD_SPH_LICV_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_LICV) $$(MOD_SPH_LICV_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_MHD_MINI): $$(MOD_SPH_MINI_MHD_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_MHD_MINI)' '$$(MOD_SPH_MINI_MHD_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SP_SNAP):  $$(MOD_SPH_SP_SNAP_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SP_SNAP)' '$$(MOD_SPH_SP_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_TAVE_PICK_CIRCLE):  $$(MOD_T_AVE_PICK_CIRCLE)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_TAVE_PICK_CIRCLE)' '$$(MOD_T_AVE_PICK_CIRCLE)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_INITIAL):  $$(MOD_SPH_INITIAL)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_INITIAL)' '$$(MOD_SPH_INITIAL)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_SPH_MHD=  \\          >> $(MAKENAME)
	@echo  $(LIBS_SPH_MHD)            >> $(MAKENAME)
	@echo  LIB_FILES_SPH_MHD=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_SPH_MHD)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


mod_list:
	@echo  MOD_SPH_MHD_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MHD_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_MHD_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MHD_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SP_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SP_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_ZM_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ZM_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_ZM_SNAP_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ZM_SNAP_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_D_BENCH_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_D_BENCH_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_PICK_CIRC=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_PICK_CIRC) >> $(MAKENAME)
	@echo  MOD_T_AVE_PICK_CIRCLE=  \\ >> $(MAKENAME)
	@echo  $(MOD_T_AVE_PICK_CIRCLE) >> $(MAKENAME)
	@echo  MOD_SPH_LICV_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_LICV_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_MINI_MHD_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MINI_MHD_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_INITIAL=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_INITIAL) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_MHD_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
