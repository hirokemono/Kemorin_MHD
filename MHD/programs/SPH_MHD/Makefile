#
#
#

SPH_MHD_MAINDIR =   $$(PROG_DIR)/SPH_MHD

TARGET_SPH_MHD =        sph_mhd
TARGET_SPH_MHD_PSF =    sph_mhd_psf
TARGET_SPH_MHD_NOVIZ =  sph_mhd_noviz
TARGET_SPH_MHD_MINI =   sph_mhd_nosnap

TARGET_SPH_LICV =       sph_licv

LIBS_SPH_MHD =    \
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


LIB_FILES_SPH_MHD =   \
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
  LIBS_SPH_MHD+=        '$$(LIB_KEMO_C)'
  LIB_FILES_SPH_MHD+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_SPH_MHD_MAIN =  \
main_sph_MHD.o \
analyzer_sph_MHD.o \
SPH_analyzer_SGS_MHD.o

MOD_SPH_MHD_PSF_MAIN =  \
main_sph_MHD_w_psf.o \
analyzer_sph_MHD_w_psf.o \
SPH_analyzer_MHD.o


MOD_SPH_MHD_NOVIZ_MAIN =  \
main_sph_MHD_noviz.o \
analyzer_sph_MHD_noviz.o \
SPH_analyzer_MHD.o

MOD_SPH_MINI_MHD_MAIN =  \
main_sph_MHD_only.o \
analyzer_small_sph_MHD.o \
SPH_analyzer_MHD.o

MOD_SPH_LICV_MAIN =  \
main_sph_linear_cv.o \
analyzer_sph_licv.o \
SPH_analyzer_licv.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_MHD_MAINDIR = $(SPH_MHD_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_MHD =        $$(BUILDDIR)/$(TARGET_SPH_MHD)'  >> $(MAKENAME)
	@echo 'TARGET_SPH_MHD_PSF =    $$(BUILDDIR)/$(TARGET_SPH_MHD_PSF)'  >> $(MAKENAME)
	@echo 'TARGET_SPH_LICV =       $$(BUILDDIR)/$(TARGET_SPH_LICV)'      >> $(MAKENAME)
	@echo 'TARGET_SPH_MHD_MINI =   $$(BUILDDIR)/$(TARGET_SPH_MHD_MINI)'  >> $(MAKENAME)
	@echo 'TARGET_SPH_MHD_NOVIZ =  $$(BUILDDIR)/$(TARGET_SPH_MHD_NOVIZ)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_mhd: \
	 '$$(TARGET_SPH_LICV)'       \
	 '$$(TARGET_SPH_MHD)'        \
	 '$$(TARGET_SPH_MHD_PSF)'    \
	 '$$(TARGET_SPH_MHD_NOVIZ)'  \
	 '$$(TARGET_SPH_MHD_MINI)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_MHD): $$(MOD_SPH_MHD_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_MHD)' '$$(MOD_SPH_MHD_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_MHD_PSF): $$(MOD_SPH_MHD_PSF_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_MHD_PSF)' '$$(MOD_SPH_MHD_PSF_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_MHD_NOVIZ): $$(MOD_SPH_MHD_NOVIZ_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_MHD_NOVIZ)' '$$(MOD_SPH_MHD_NOVIZ_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_LICV): $$(MOD_SPH_LICV_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_LICV) $$(MOD_SPH_LICV_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_MHD_MINI): $$(MOD_SPH_MINI_MHD_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_MHD_MINI)' '$$(MOD_SPH_MINI_MHD_MAIN)' \
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
	@echo  MOD_SPH_MHD_PSF_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MHD_PSF_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_MHD_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MHD_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_LICV_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_LICV_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_MINI_MHD_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MINI_MHD_MAIN) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_MHD_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
