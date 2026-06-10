#
#
#

FILTER3D_TRANS_DIR =    $$(G_FILTER_PLUG_DIR)/filter_coef_transfer
TARGET_FILTER_TRANS =   $$(FEM_UTIL_BINDIR)/filter_coef_transfer

SOURCES = $(shell ls *.f90)
MOD_FILTER_COEF_TRANS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER3D_TRANS_DIR = $(FILTER3D_TRANS_DIR)'   >> $(MAKENAME)

target_list:
	@echo 'TARGET_FILTER_TRANS =    $(TARGET_FILTER_TRANS)'    >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_FILTER_TRANS)': \
	'$$(MOD_FILTER_COEF_TRANS)' '$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FILTER_TRANS)' \
	'$$(MOD_FILTER_COEF_TRANS)' '-L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_FILTER_COEF_TRANS=  \\ >> $(MAKENAME)
	@echo  $(MOD_FILTER_COEF_TRANS)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FILTER3D_TRANS_DIR)' $(SOURCES)
