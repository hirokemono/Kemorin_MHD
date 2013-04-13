#
#
#

SPH_MHD_MAINDIR =   $$(PROG_DIR)/SPH_MHD

TARGET_SPH_MHD_MINI =   sph_mhd


LIBS_SPH_MHD = \
'$$(LIB_CALYPSO)'  \
'$$(LIB_FFTPACK)'

LIB_FILES_SPH_MHD = \
'$$(LIB_CALYPSO_FILE)' \
'$$(LIB_FFTPACK_FILE)'


SOURCES = $(shell ls *.f90)

MOD_SPH_MINI_MHD_MAIN =  \
main_sph_MHD_only.o \
analyzer_small_sph_MHD.o \
SPH_analyzer_MHD.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_MHD_MAINDIR = $(SPH_MHD_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_MHD_MINI =   $$(BUILDDIR)/$(TARGET_SPH_MHD_MINI)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_mhd: \
	 '$$(TARGET_SPH_MHD_MINI)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_MHD_MINI): $$(MOD_SPH_MINI_MHD_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_MHD_MINI)' '$$(MOD_SPH_MINI_MHD_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_SPH_MHD=  \\          >> $(MAKENAME)
	@echo  $(LIBS_SPH_MHD)            >> $(MAKENAME)
	@echo  LIB_FILES_SPH_MHD=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_SPH_MHD)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


mod_list:
	@echo  MOD_SPH_MINI_MHD_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MINI_MHD_MAIN) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_MHD_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
