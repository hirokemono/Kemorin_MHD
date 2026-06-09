#
#
#

FEM_MHD_PROG_DIR = $$(MHD_OUT_DIR)/fem_mhd
TARGET_FEM_MHD =   $$(BUILDDIR)/fem_mhd

SOURCES = $(shell ls *.f90)
MOD_FEM_MHD_PLUG = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_MHD_PROG_DIR = $(FEM_MHD_PROG_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FEM_MHD = $(TARGET_FEM_MHD)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_FEM_MHD)': '$$(MOD_FEM_MHD_PLUG) $$(LIB_FILES_FEM_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_MHD)' \
	'$$(MOD_FEM_MHD_PLUG) -L. $$(LIBS_FEM_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_FEM_MHD_PLUG=  \\       >> $(MAKENAME)
	@echo  $(MOD_FEM_MHD_PLUG)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FEM_MHD_PROG_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
