#
#
#

CONST_FILTER_Z_DIR =  $$(G_FILTER_PLUG_DIR)/const_z_filter

TARGET_Z_FILTER = commute_z
TARGET_TEST_DZ =  test_dz

SOURCES = $(shell ls *.f90)

MOD_Z_FILTER_CTL = \
m_ctl_data_gen_z_filter.o \
read_ctl_gen_z_filter.o \
set_ctl_gen_z_filter.o \
\
input_control_gen_z_filter.o

MOD_Z_FILTER_PLUG = \
analyzer_gen_z_filter.o \
main_gen_z_filter.o


MOD_TEST_DZ_PLUG = \
analyzer_test_dz.o \
main_test_dz.o

MODULES = \
$(MOD_Z_FILTER_CTL) \
$(MOD_Z_FILTER_PLUG) \
$(MOD_TEST_DZ_PLUG)

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'CONST_FILTER_Z_DIR =  $$(G_FILTER_PLUG_DIR)/const_z_filter'    >> $(MAKENAME)

target_list:
	@echo 'TARGET_Z_FILTER = $$(BUILDDIR)/$(TARGET_Z_FILTER)' \
	>> $(MAKENAME)
	@echo 'TARGET_TEST_DZ =  $$(BUILDDIR)/$(TARGET_TEST_DZ)'  \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo const_z_filter:  \
	'$$(TARGET_Z_FILTER)' '$$(TARGET_TEST_DZ)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_Z_FILTER)': '$$(MOD_Z_FILTER_PLUG)' \
	'$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_Z_FILTER)' \
	'$$(MOD_Z_FILTER_PLUG)' '-L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_TEST_DZ)': '$$(MOD_TEST_DZ_PLUG)' \
	'$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_TEST_DZ)' \
	'$$(MOD_TEST_DZ_PLUG)' '-L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_Z_FILTER_PLUG=  \\ >> $(MAKENAME)
	@echo  $(MOD_Z_FILTER_PLUG)  $(MOD_Z_FILTER_CTL) >> $(MAKENAME)
	@echo   >> $(MAKENAME)
	@echo  MOD_TEST_DZ_PLUG=  \\ >> $(MAKENAME)
	@echo  $(MOD_TEST_DZ_PLUG)  $(MOD_Z_FILTER_CTL)  >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CONST_FILTER_Z_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
