#
#
#

GEN_ITP_TABLE_DIR =    $$(ITP_PLUG_DIR)/const_interpolate_table
TARGET_GEN_ITP_TABLE = $$(FEM_UTIL_BINDIR)/const_interpolate_table

SOURCES = $(shell ls *.f90)
MOD_GEN_ITP_TABLE = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GEN_ITP_TABLE_DIR = $(GEN_ITP_TABLE_DIR)'   >> $(MAKENAME)

target_list:
	@echo 'TARGET_GEN_ITP_TABLE = $(TARGET_GEN_ITP_TABLE)' >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_GEN_ITP_TABLE)': \
	'$$(MOD_GEN_ITP_TABLE)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GEN_ITP_TABLE)' \
	'$$(MOD_GEN_ITP_TABLE)' '-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_GEN_ITP_TABLE=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_ITP_TABLE)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(GEN_ITP_TABLE_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
