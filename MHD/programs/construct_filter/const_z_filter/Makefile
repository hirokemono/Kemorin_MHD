#
#
#

CONST_FILTER_Z_DIR =  $$(G_FILTER_PLUG_DIR)/const_z_filter

TARGET_Z_FILTER = commute_z
TARGET_TEST_DZ =  test_dz

SOURCES = $(shell ls *.f90)

MOD_Z_FILTER_CTL = \
m_ctl_data_gen_z_filter.o \
set_ctl_gen_z_filter.o \
\
input_control_gen_z_filter.o \
\
const_geometry_z_commute.o \
\
m_int_edge_vart_width.o \
m_neibor_data_z.o \
m_work_4_integration.o \
m_matrix_4_z_commute.o \
m_commute_filter_z.o \
m_consist_mass_crs.o \
m_int_edge_data.o


MOD_Z_FILTER_PLUG = \
main_gen_z_filter.o \
analyzer_gen_z_filter.o \
\
m_int_commtative_filter.o \
m_z_filter_values.o \
\
const_crs_connect_commute_z.o \
const_delta_z_analytical.o \
const_z_commute_matrix.o \
const_connect_2_n_filter.o \
\
set_diff_position_z_filter.o \
set_vert_diff_z_filter.o \
int_edge_norm_nod_z_filter.o \
int_edge_moment_z_filter.o \
int_edge_horiz_filter_peri.o \
int_edge_commute_z_filter.o \
int_edge_mass_mat_z_filter.o \
\
cal_jacobian_linear_1d.o \
calcs_by_LUsolver.o \
copy_1darray_2_2darray.o \
\
set_neib_nod_z.o \
set_neib_ele_z.o \
set_neib_connect_z.o \
set_matrices_4_z_filter.o \
write_z_filter_4_nod.o

MOD_TEST_DZ_PLUG = \
main_test_dz.o \
analyzer_test_dz.o \
\
const_crs_connect_commute_z.o \
\
set_vert_diff_z_filter.o \
int_edge_norm_nod_z_filter.o \
int_edge_mass_mat_z_filter.o \
\
cal_jacobian_linear_1d.o \
calcs_by_LUsolver.o \
\
cal_delta_z_4_z_filter.o \
solve_by_mass_z.o \
set_matrices_4_z_filter.o

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
