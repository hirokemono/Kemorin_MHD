#
#
#
CONST_FILTER_Z_DIR =  $$(G_FILTER_PLUG_DIR)/commute_z_filter
TARGET_Z_FILTER =     $$(FEM_UTIL_BINDIR)/commute_z_filter

SOURCES = $(shell ls *.f90)
MOD_Z_FILTER_PLUG = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'CONST_FILTER_Z_DIR = $(CONST_FILTER_Z_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_Z_FILTER = $(TARGET_Z_FILTER)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_Z_FILTER)': '$$(MOD_Z_FILTER_PLUG) $$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_Z_FILTER)' \
	'$$(MOD_Z_FILTER_PLUG) -L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_Z_FILTER_PLUG=  \\ >> $(MAKENAME)
	@echo  $(MOD_Z_FILTER_PLUG) >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(CONST_FILTER_Z_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean
