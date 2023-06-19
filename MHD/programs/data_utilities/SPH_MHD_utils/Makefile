#
#
#

SPH_SNAPSHOT_DIR =   $$(DATA_UTILS_DIR)/SPH_MHD_utils

TARGET_SPH_SNAP =         sph_snapshot
TARGET_SPH_SNAP_BADBOY =  sph_snapshot_badboy

TARGET_SPH_SP_SNAP =    sph_special_snap
TARGET_MODIFY_SPH_INI = sph_modify_restart

TARGET_SPH_VIZS_SNAP =    sph_snapshot_vizs
TARGET_SPH_PSF_SNAP =     sph_snapshot_psf

TARGET_SPH_SNAP_NOVIZ =    sph_snapshot_noviz

TARGET_SPH_FORWARD_TRANS = sph_forward_transform
TARGET_SPH_BACK_TRANS =    sph_backward_transform
TARGET_SPH_ALL_CORRELATE = sph_all_correlate
TARGET_SPH_ALL_RMS_RATIO = sph_all_rms_ratio

TARGET_CONTROL_MHD_CHECK = check_control_mhd
TARGET_CTL_MHD_VIZ_CHECK = check_control_mhd_viz
TARGET_CTL_MHD_PSF_CHECK = check_control_mhd_psf

LIBS_SPH_UTILS =  \
'$$(LIB_MHD)'     \
'$$(LIB_VIZ)'     \
'$$(LIB_PARA)'    \
'$$(LIB_SOLVER)'  \
'$$(LIB_MESH)'    \
'$$(LIB_MT_STREAM)' \
'$$(LIB_FFTPACK)'   \
'$$(LIB_ISPACK)'    \
'$$(LIB_ISPACK3)'   \
'$$(LIB_NETLIB)'


LIB_FILES_SPH_UTILS = \
'$$(LIB_MHD_FILE)'    \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'   \
'$$(LIB_MT_STREAM_FILE)' \
'$$(LIB_FFTPACK_FILE)'   \
'$$(LIB_ISPACK_FILE)'    \
'$$(LIB_ISPACK3_FILE)'   \
'$$(LIB_NETLIB_FILE)'


ifdef FORTRAN_C_CONNECT
  LIBS_SPH_UTILS+=        '$$(LIB_KEMO_C)'
  LIB_FILES_SPH_UTILS+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_SPH_SNAP_MAIN =  \
main_sph_snapshot.o \
analyzer_sph_snap.o \
initialization_sph_SGS_snap.o \
SPH_analyzer_SGS_snap.o

MOD_SPH_SNAP_VIZS_MAIN =  \
main_sph_snapshot_w_vizs.o \
analyzer_sph_snap_w_vizs.o \
SPH_analyzer_snap_w_vizs.o

MOD_SPH_SNAP_PSF_MAIN =  \
main_sph_snapshot_w_psf.o \
analyzer_sph_snap_w_psf.o \
SPH_analyzer_snap_w_vizs.o

MOD_SPH_SNAP_BADBOY =  \
main_sph_snap_badboy.o \
analyzer_sph_snap_badboy.o \
initialization_sph_SGS_snap.o \
SPH_analyzer_SGS_snap.o


MOD_SPH_SP_SNAP_MAIN = \
main_sph_special_snap.o \
analyzer_sph_special_snap.o \
initialization_sph_SGS_snap.o \
SPH_analyzer_SGS_snap.o


MOD_SPH_SNAP_NOVIZ_MAIN =  \
main_sph_snapshot_noviz.o \
analyzer_noviz_sph_snap.o \
initialize_sph_snap_noviz.o \
SPH_analyzer_snap_w_vizs.o

MOD_MODIFY_SPH_INI = \
main_sph_modify_restart.o \
analyzer_sph_modify_restart.o \
analyzer_noviz_sph_snap.o \
initialize_sph_snap_noviz.o \
SPH_analyzer_snap_w_vizs.o

MOD_SPH_FORWARD_TRANS = \
main_sph_forward_transform.o \
analyzer_sph_forward_trans.o \
SPH_analyzer_SGS_snap.o

MOD_SPH_BACK_TRANS = \
main_sph_back_transform.o \
analyzer_sph_back_trans.o \
SPH_analyzer_back_trans.o \
back_sph_trans_4_all_field.o

MOD_SPH_ALL_CORRELATE = \
main_sph_all_correlate.o \
analyzer_sph_all_correlate.o \
SPH_analyzer_correle_all.o \
SPH_analyzer_back_trans.o \
back_sph_trans_4_all_field.o \
cal_correlations_by_spectr.o

MOD_SPH_ALL_RMS_RATIO = \
main_sph_all_rms_ratio.o \
analyzer_sph_all_rms_ratio.o \
SPH_analyzer_rms_ratio_all.o \
SPH_analyzer_back_trans.o \
back_sph_trans_4_all_field.o \
cal_correlations_by_spectr.o

MOD_CONTROL_MHD_CHECK = \
main_control_MHD_check.o

MOD_CTL_MHD_VIZ_CHECK = \
main_control_MHD_viz_check.o

MOD_CTL_MHD_PSF_CHECK = \
main_control_MHD_psf_check.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_SNAPSHOT_DIR = $(SPH_SNAPSHOT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_SNAP =       $$(BUILDDIR)/$(TARGET_SPH_SNAP)'       >> $(MAKENAME)
	@echo 'TARGET_SPH_VIZS_SNAP =  $$(BUILDDIR)/$(TARGET_SPH_VIZS_SNAP)'  >> $(MAKENAME)
	@echo 'TARGET_SPH_PSF_SNAP =   $$(BUILDDIR)/$(TARGET_SPH_PSF_SNAP)'   >> $(MAKENAME)
	@echo 'TARGET_SPH_SP_SNAP =    $$(BUILDDIR)/$(TARGET_SPH_SP_SNAP)'    >> $(MAKENAME)
	@echo 'TARGET_MODIFY_SPH_INI = $$(BUILDDIR)/$(TARGET_MODIFY_SPH_INI)' >> $(MAKENAME)
	@echo 'TARGET_SPH_SNAP_NOVIZ = $$(BUILDDIR)/$(TARGET_SPH_SNAP_NOVIZ)'>> $(MAKENAME)
	@echo 'TARGET_SPH_SNAP_BADBOY = $$(BUILDDIR)/$(TARGET_SPH_SNAP_BADBOY)' >> $(MAKENAME)
	@echo 'TARGET_SPH_FORWARD_TRANS =  $$(BUILDDIR)/$(TARGET_SPH_FORWARD_TRANS)' >> $(MAKENAME)
	@echo 'TARGET_SPH_BACK_TRANS =     $$(BUILDDIR)/$(TARGET_SPH_BACK_TRANS)' >> $(MAKENAME)
	@echo 'TARGET_SPH_ALL_CORRELATE =  $$(BUILDDIR)/$(TARGET_SPH_ALL_CORRELATE)' >> $(MAKENAME)
	@echo 'TARGET_SPH_ALL_RMS_RATIO =  $$(BUILDDIR)/$(TARGET_SPH_ALL_RMS_RATIO)' >> $(MAKENAME)
	@echo 'TARGET_CONTROL_MHD_CHECK =  $$(BUILDDIR)/$(TARGET_CONTROL_MHD_CHECK)' >> $(MAKENAME)
	@echo 'TARGET_CTL_MHD_VIZ_CHECK =  $$(BUILDDIR)/$(TARGET_CTL_MHD_VIZ_CHECK)' >> $(MAKENAME)
	@echo 'TARGET_CTL_MHD_PSF_CHECK =  $$(BUILDDIR)/$(TARGET_CTL_MHD_PSF_CHECK)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_utils: \
	 '$$(TARGET_SPH_SNAP)'       \
	 '$$(TARGET_SPH_VIZS_SNAP)'  \
	 '$$(TARGET_SPH_PSF_SNAP)'   \
	 '$$(TARGET_SPH_SP_SNAP)'    \
	 '$$(TARGET_MODIFY_SPH_INI)' \
	 '$$(TARGET_SPH_SNAP_NOVIZ)' \
	 '$$(TARGET_SPH_SNAP_BADBOY)'   \
	 '$$(TARGET_SPH_FORWARD_TRANS)'  \
	 '$$(TARGET_SPH_BACK_TRANS)'  \
	 '$$(TARGET_SPH_ALL_CORRELATE)' \
	 '$$(TARGET_SPH_ALL_RMS_RATIO)' \
	 '$$(TARGET_CONTROL_MHD_CHECK)' \
	 '$$(TARGET_CTL_MHD_VIZ_CHECK)' \
	 '$$(TARGET_CTL_MHD_PSF_CHECK)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_SNAP):  $$(MOD_SPH_SNAP_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_SNAP)' '$$(MOD_SPH_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_VIZS_SNAP):  $$(MOD_SPH_SNAP_VIZS_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_VIZS_SNAP)' '$$(MOD_SPH_SNAP_VIZS_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_PSF_SNAP):  $$(MOD_SPH_SNAP_PSF_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_PSF_SNAP)' '$$(MOD_SPH_SNAP_PSF_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SNAP_NOVIZ):  $$(MOD_SPH_SNAP_NOVIZ_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_SNAP_NOVIZ)' '$$(MOD_SPH_SNAP_NOVIZ_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SP_SNAP):  $$(MOD_SPH_SP_SNAP_MAIN)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_SP_SNAP)' '$$(MOD_SPH_SP_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_MODIFY_SPH_INI):  $$(MOD_MODIFY_SPH_INI)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_MODIFY_SPH_INI)' '$$(MOD_MODIFY_SPH_INI)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_SNAP_BADBOY):  $$(MOD_SPH_SNAP_BADBOY)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_SNAP_BADBOY)' '$$(MOD_SPH_SNAP_BADBOY)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_FORWARD_TRANS):  $$(MOD_SPH_FORWARD_TRANS)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_FORWARD_TRANS)' '$$(MOD_SPH_FORWARD_TRANS)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_BACK_TRANS):  $$(MOD_SPH_BACK_TRANS)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_BACK_TRANS)' '$$(MOD_SPH_BACK_TRANS)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_ALL_CORRELATE):  $$(MOD_SPH_ALL_CORRELATE)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_ALL_CORRELATE)' '$$(MOD_SPH_ALL_CORRELATE)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_ALL_RMS_RATIO):  $$(MOD_SPH_ALL_RMS_RATIO)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_ALL_RMS_RATIO)' '$$(MOD_SPH_ALL_RMS_RATIO)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CONTROL_MHD_CHECK):  $$(MOD_CONTROL_MHD_CHECK)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CONTROL_MHD_CHECK)' '$$(MOD_CONTROL_MHD_CHECK)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CTL_MHD_VIZ_CHECK):  $$(MOD_CTL_MHD_VIZ_CHECK)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CTL_MHD_VIZ_CHECK)' '$$(MOD_CTL_MHD_VIZ_CHECK)' \
	'-L. $$(LIBS_SPH_UTILS)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CTL_MHD_PSF_CHECK):  $$(MOD_CTL_MHD_PSF_CHECK)  $$(LIB_FILES_SPH_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CTL_MHD_PSF_CHECK)' '$$(MOD_CTL_MHD_PSF_CHECK)' \
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
	@echo  MOD_SPH_SNAP_VIZS_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_VIZS_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_PSF_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_PSF_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SP_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SP_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_BADBOY=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_BADBOY) >> $(MAKENAME)
	@echo  MOD_MODIFY_SPH_INI=  \\ >> $(MAKENAME)
	@echo  $(MOD_MODIFY_SPH_INI) >> $(MAKENAME)
	@echo  MOD_SPH_FORWARD_TRANS=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_FORWARD_TRANS) >> $(MAKENAME)
	@echo  MOD_SPH_BACK_TRANS=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_BACK_TRANS) >> $(MAKENAME)
	@echo  MOD_SPH_ALL_CORRELATE=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ALL_CORRELATE) >> $(MAKENAME)
	@echo  MOD_SPH_ALL_RMS_RATIO=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ALL_RMS_RATIO) >> $(MAKENAME)
	@echo  MOD_CONTROL_MHD_CHECK=  \\ >> $(MAKENAME)
	@echo  $(MOD_CONTROL_MHD_CHECK) >> $(MAKENAME)
	@echo  MOD_CTL_MHD_VIZ_CHECK=  \\ >> $(MAKENAME)
	@echo  $(MOD_CTL_MHD_VIZ_CHECK) >> $(MAKENAME)
	@echo  MOD_CTL_MHD_PSF_CHECK=  \\ >> $(MAKENAME)
	@echo  $(MOD_CTL_MHD_PSF_CHECK) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_SNAPSHOT_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
