#
#
#

SPH_ZONAL_FFT_DIR = $$(DATA_UTILS_DIR)/zonal_spectr

TARGET_SPH_Z_FFT =      zonal_fft
TARGET_SPH_Z_ENE_SPEC = ene_zonal_spec


LIBS_ZONAL_SPH = \
'$$(LIB_VIZ)'    \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'    \
'$$(LIB_FFTPACK)' \
'$$(LIB_ISPACK)'

LIB_FILES_ZONAL_SPH = \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'   \
'$$(LIB_FFTPACK_FILE)' \
'$$(LIB_ISPACK_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_ZONAL_SPH+=        '$$(LIB_KEMO_C)'
  LIB_FILES_ZONAL_SPH+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_SPH_Z_CTL = \
m_ctl_data_4_zonal_fft.o \
m_ctl_params_zonal_fft.o


MOD_SPH_Z_FFT = \
main_zonal_fft.o \
analyzer_zonal_fft.o \
\
transfer_field_2_rtp_grid.o

MOD_SPH_Z_ENE_SPEC = \
main_ene_zonal_spec.o \
analyzer_ene_zonal_spec.o \
\
m_sph_zonal_ene_spectr.o \
cal_zonal_ene_spec.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_ZONAL_FFT_DIR = $(SPH_ZONAL_FFT_DIR)'   >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_Z_FFT = $$(BUILDDIR)/$(TARGET_SPH_Z_FFT)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_Z_ENE_SPEC = $$(BUILDDIR)/$(TARGET_SPH_Z_ENE_SPEC)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo zonal_trans: \
	'$$(TARGET_SPH_Z_FFT)       $$(TARGET_SPH_Z_ENE_SPEC)'  \
	  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_Z_FFT)': '$$(MOD_SPH_Z_FFT)' \
	'$$(LIB_FILES_ZONAL_SPH)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_Z_FFT)' \
	'$$(MOD_SPH_Z_FFT)' \
	'-L. $$(LIBS_ZONAL_SPH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_Z_ENE_SPEC)': '$$(MOD_SPH_Z_ENE_SPEC)' \
	'$$(LIB_FILES_ZONAL_SPH)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_Z_ENE_SPEC)' \
	'$$(MOD_SPH_Z_ENE_SPEC)' \
	'-L. $$(LIBS_ZONAL_SPH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_ZONAL_SPH=  \\          >> $(MAKENAME)
	@echo  $(LIBS_ZONAL_SPH)            >> $(MAKENAME)
	@echo  LIB_FILES_ZONAL_SPH=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_ZONAL_SPH)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


mod_list:
	@echo  MOD_SPH_Z_FFT=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_Z_FFT) $(MOD_SPH_Z_CTL) >> $(MAKENAME)
	@echo  MOD_SPH_Z_ENE_SPEC=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_Z_ENE_SPEC) $(MOD_SPH_Z_CTL) >> $(MAKENAME)
	@echo   >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_ZONAL_FFT_DIR)' $(SOURCES)
