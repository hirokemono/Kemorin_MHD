#
#
#
FEM_MHD_MAT_DIR = $$(MHD_OUT_DIR)/check_FEM_MHD_matrices

TARGET_FEM_MHD_MAT= check_FEM_MHD_matrices

SOURCES = $(shell ls *.f90)
MOD_FEM_MHD_MAT = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_MHD_MAT_DIR = $(FEM_MHD_MAT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FEM_MHD_MAT = $$(BUILDDIR)/$(TARGET_FEM_MHD_MAT)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_FEM_MHD_MAT)': '$$(MOD_FEM_MHD_MAT) $$(LIB_FILES_FEM_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_MHD_MAT)' \
	'$$(MOD_FEM_MHD_MAT) -L. $$(LIBS_FEM_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_FEM_MHD_MAT=  \\       >> $(MAKENAME)
	@echo  $(MOD_FEM_MHD_MAT)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FEM_MHD_MAT_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
