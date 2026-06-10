#
#
#

SORT_3D_FILTER_DIR =    $$(G_FILTER_PLUG_DIR)/sort_commute3d_filter
TARGET_SORT_3D_FILTER = $$(FEM_UTIL_BINDIR)/sort_commute3d_filter

SOURCES = $(shell ls *.f90)
MOD_SORT_3D_FILTER = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SORT_3D_FILTER_DIR = $(SORT_3D_FILTER_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SORT_3D_FILTER = $(TARGET_SORT_3D_FILTER)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_SORT_3D_FILTER)': '$$(MOD_SORT_3D_FILTER) $$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SORT_3D_FILTER)' \
	'$$(MOD_SORT_3D_FILTER) -L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

lib_tasks:

mod_list:
	@echo  MOD_SORT_3D_FILTER=  \\       >> $(MAKENAME)
	@echo  $(MOD_SORT_3D_FILTER)         >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SORT_3D_FILTER_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L

distclean: clean
