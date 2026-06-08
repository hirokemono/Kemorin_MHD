#
#
#
MHD_OUT_DIR =       $$(MHD_OUT_DIR)/check_FEM_MHD_matrices

TARGET_FEM_MHD_MAT= check_mhd_matrix

SOURCES = $(shell ls *.f90)
MOD_FEM_MHD_MAT = $(addsuffix .o,$(basename $(SOURCES)) )

LIBS_FEM_MHD_MAT = \
'$$(LIB_MHD)'    \
'$$(LIB_VIZ)'    \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'   \
'$$(LIB_MT_STREAM)'


LIB_FILES_FEM_MHD_MAT = \
'$$(LIB_MHD_FILE)'    \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'   \
'$$(LIB_MT_STREAM_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_FEM_MHD_MAT+=        '$$(LIB_KEMO_C)'
  LIB_FILES_FEM_MHD_MAT+=   '$$(LIB_KEMO_C_FILE)'
endif

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_OUT_DIR = $(MHD_OUT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FEM_MHD_MAT = $$(BUILDDIR)/$(TARGET_FEM_MHD_MAT)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo fem_mhd:  \
	'$$(TARGET_FEM_MHD_MAT)'   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FEM_MHD_MAT)': '$$(MOD_FEM_MHD_MAT) $$(LIB_FILES_FEM_MHD_MAT)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_MHD_MAT)' \
	'$$(MOD_FEM_MHD_MAT) -L. $$(LIBS_FEM_MHD_MAT)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_FEM_MHD_MAT=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FEM_MHD_MAT)            >> $(MAKENAME)
	@echo  LIB_FILES_FEM_MHD_MAT=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FEM_MHD_MAT)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_FEM_MHD_MAT=  \\       >> $(MAKENAME)
	@echo  $(MOD_FEM_MHD_MAT)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_OUT_DIR)' $(SOURCES)


clean:
	rm -f *.o *~ *.mod *.diag *.L
