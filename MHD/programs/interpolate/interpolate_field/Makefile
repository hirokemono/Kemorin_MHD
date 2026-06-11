#
#
#

ITP_FIELD_DIR =    $$(ITP_PLUG_DIR)/interpolate_field
TARGET_ITP_FIELD = $$(FEM_UTIL_BINDIR)/interpolate_field

SOURCES = $(shell ls *.f90)
MOD_ITP_FIELD = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_FIELD_DIR = $(ITP_FIELD_DIR)'   >> $(MAKENAME)

target_list:
	@echo 'TARGET_ITP_FIELD = $(TARGET_ITP_FIELD)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ITP_FIELD)': \
	'$$(MOD_ITP_FIELD)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ITP_FIELD)' \
	'$$(MOD_ITP_FIELD)' '-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_ITP_FIELD=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_FIELD)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ITP_FIELD_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
