#
#
#

PLANE_SPECTR_DIR =  $$(DATA_UTILS_DIR)/plane_model

TARGET_SPEC_INI = initial_by_spectr
TARGET_SPEC_UDT = data_from_spectr

TARGET_FFT_PLANE = fft_4_plane

TARGET_SPEC_ENE =        lead_energy
TARGET_SPEC_E_PLANE =    lead_ene_plane
TARGET_PICK_MODE_PLANE = pickup_mode_plane
TARGET_SPEC_MODE =       pickup_ene_mode_plane
TARGET_SPEC_TAVE_PLANE = t_ave_ene_plane

TARGET_COR_SNAP =  correlate_plane_snap
TARGET_COR_PLANE = correlate_plane_evo

LIBS_PLANE_DATA = \
'$$(LIB_UTILS)'   \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'    \
'$$(LIB_FFTPACK)' \
'$$(LIB_ISPACK)'

LIB_FILES_PLANE_DATA = \
'$$(LIB_UTILS_FILE)'   \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'    \
'$$(LIB_FFTPACK_FILE)' \
'$$(LIB_ISPACK_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_PLANE_DATA+=        '$$(LIB_KEMO_C)'
  LIB_FILES_PLANE_DATA+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_FFT_PLANE_RST = \
set_list_4_FFT.o \
write_restart_by_spectr.o \
make_initial_by_spectra.o

MOD_FFT_PLANE_UDT = \
set_list_4_FFT.o \
make_physical_data.o

MOD_FFT_PLANE_SPECTR = \
set_list_4_FFT.o \
fft_4_plane.o

MOD_FFT_PLANE_ENETOT = \
set_list_4_FFT.o \
lead_energy.o

MOD_FFT_PLANE_ENEPLANE = \
set_list_4_FFT.o \
lead_ene_plane.o

MOD_FFT_PLANE_MODES = \
set_list_4_FFT.o \
pickup_ene_mode_plane.o

MOD_FFT_PLANE_TAVE_ENE = \
set_list_4_FFT.o \
t_average_4_plane_spectr.o

MOD_PICK_MODE_PLANE = \
set_list_4_FFT.o \
pickup_mode_plane.o

MOD_COR_PLANE_SNAP = \
correlation_4_snap.o \
m_correlate_4_plane.o

MOD_COR_PLANE = \
correlation_4_plane.o \
m_correlate_4_plane.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PLANE_SPECTR_DIR = $(PLANE_SPECTR_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPEC_INI =     $$(BUILDDIR)/$(TARGET_SPEC_INI)'    \
	>> $(MAKENAME)
	@echo 'TARGET_SPEC_UDT =     $$(BUILDDIR)/$(TARGET_SPEC_UDT)'     \
	>> $(MAKENAME)
	@echo 'TARGET_FFT_PLANE =    $$(BUILDDIR)/$(TARGET_FFT_PLANE)'    \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo 'TARGET_SPEC_ENE =        $$(BUILDDIR)/$(TARGET_SPEC_ENE)'        \
	>> $(MAKENAME)
	@echo 'TARGET_SPEC_E_PLANE =    $$(BUILDDIR)/$(TARGET_SPEC_E_PLANE)'    \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo 'TARGET_SPEC_MODE =       $$(BUILDDIR)/$(TARGET_SPEC_MODE)'       \
	>> $(MAKENAME)
	@echo 'TARGET_SPEC_TAVE_PLANE = $$(BUILDDIR)/$(TARGET_SPEC_TAVE_PLANE)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo 'TARGET_PICK_MODE_PLANE = $$(BUILDDIR)/$(TARGET_PICK_MODE_PLANE)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo 'TARGET_COR_SNAP =  $$(BUILDDIR)/$(TARGET_COR_SNAP)'    \
	>> $(MAKENAME)
	@echo 'TARGET_COR_PLANE = $$(BUILDDIR)/$(TARGET_COR_PLANE)'   \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)


target_task:
	@echo plane_utils:    '$$(TARGET_FFT_PLANE)'       \
	'$$(TARGET_SPEC_INI)  $$(TARGET_SPEC_UDT)'        \
	'$$(TARGET_SPEC_ENE)  $$(TARGET_SPEC_E_PLANE)'    \
	'$$(TARGET_SPEC_MODE) $$(TARGET_SPEC_TAVE_PLANE)' \
	'$$(TARGET_PICK_MODE_PLANE)'                      \
	'$$(TARGET_COR_SNAP)    $$(TARGET_COR_PLANE)'     \
	'$$(TARGET_INI_T_CUBE)  $$(TARGET_INI_B_CUBE)'    \
	>> $(MAKENAME)
	
	@echo '$$(TARGET_SPEC_INI)': '$$(MOD_FFT_PLANE_RST)' \
	'$$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPEC_INI)' \
	'$$(MOD_FFT_PLANE_RST)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPEC_UDT)': '$$(MOD_FFT_PLANE_UDT)' \
	'$$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPEC_UDT)' \
	'$$(MOD_FFT_PLANE_UDT)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FFT_PLANE): $$(MOD_FFT_PLANE_SPECTR) $$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_FFT_PLANE)' \
	'$$(MOD_FFT_PLANE_SPECTR) -L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPEC_ENE)': '$$(MOD_FFT_PLANE_ENETOT)' \
	'$$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPEC_ENE)' \
	'$$(MOD_FFT_PLANE_ENETOT)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPEC_E_PLANE)': '$$(MOD_FFT_PLANE_ENEPLANE)' \
	'$$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPEC_E_PLANE)' \
	'$$(MOD_FFT_PLANE_ENEPLANE)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPEC_MODE)': '$$(MOD_FFT_PLANE_MODES)' \
	'$$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPEC_MODE)' \
	'$$(MOD_FFT_PLANE_MODES)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPEC_TAVE_PLANE)': '$$(MOD_FFT_PLANE_TAVE_ENE)' \
	'$$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPEC_TAVE_PLANE)' \
	'$$(MOD_FFT_PLANE_TAVE_ENE)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_PICK_MODE_PLANE)': '$$(MOD_PICK_MODE_PLANE)' \
	'$$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PICK_MODE_PLANE)' \
	'$$(MOD_PICK_MODE_PLANE)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_COR_SNAP): $$(MOD_COR_PLANE_SNAP) $$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_COR_SNAP)' \
	'$$(MOD_COR_PLANE_SNAP)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_COR_PLANE): $$(MOD_COR_PLANE) $$(LIB_FILES_PLANE_DATA)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_COR_PLANE)' \
	'$$(MOD_COR_PLANE)' \
	'-L. $$(LIBS_PLANE_DATA)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_PLANE_DATA=  \\          >> $(MAKENAME)
	@echo  $(LIBS_PLANE_DATA)            >> $(MAKENAME)
	@echo  LIB_FILES_PLANE_DATA=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_PLANE_DATA)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_FFT_PLANE_RST=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_RST) >> $(MAKENAME)
	@echo  MOD_FFT_PLANE_UDT=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_UDT) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)
	@echo  MOD_FFT_PLANE_SPECTR=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_SPECTR) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)
	@echo  MOD_FFT_PLANE_ENETOT=  \\   >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_ENETOT)     >> $(MAKENAME)
	@echo  MOD_FFT_PLANE_ENEPLANE=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_ENEPLANE)   >> $(MAKENAME)
	@echo  MOD_FFT_PLANE_MODES=  \\    >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_MODES)      >> $(MAKENAME)
	@echo  MOD_FFT_PLANE_TAVE_ENE=  \\ >> $(MAKENAME)
	@echo  $(MOD_FFT_PLANE_TAVE_ENE)   >> $(MAKENAME)
	@echo  MOD_PICK_MODE_PLANE=  \\    >> $(MAKENAME)
	@echo  $(MOD_PICK_MODE_PLANE)      >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)
	@echo  MOD_COR_PLANE_SNAP=  \\ >> $(MAKENAME)
	@echo  $(MOD_COR_PLANE_SNAP) >> $(MAKENAME)
	@echo  MOD_COR_PLANE=  \\ >> $(MAKENAME)
	@echo  $(MOD_COR_PLANE) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PLANE_SPECTR_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
