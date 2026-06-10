#
#
#

CONST_3D_FILTER_DIR = $$(G_FILTER_PLUG_DIR)/commute_3d_filter
TARGET_3D_FILTER =    $$(FEM_UTIL_BINDIR)/commute_3d_filter

SOURCES = $(shell ls *.f90)
MOD_3D_FILTER = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CONST_3D_FILTER_DIR = $(CONST_3D_FILTER_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_3D_FILTER = $(TARGET_3D_FILTER)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_3D_FILTER)': '$$(MOD_3D_FILTER) $$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_3D_FILTER)' \
	'$$(MOD_3D_FILTER) -L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_3D_FILTER=  \\       >> $(MAKENAME)
	@echo  $(MOD_3D_FILTER)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(CONST_3D_FILTER_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
