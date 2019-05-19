#
#
#

UCD_UTILS_DIR = $$(DATA_UTILS_DIR)/FEM_utils

TARGET_AVE_FIELDS =  ave_udt
TARGET_DIFF_FIELDS = diff_udt
TARGET_PROD_FIELDS = products_udt
TARGET_RATIO_FIELDS = ratio_udt
TARGET_CORR_FIELDS =  correlate_udt
TARGET_CORR_ONECOMP = correlate_w_1_comp

TARGET_MED_PATCH =   med_ele_patch
TARGET_MED_GRP_UDT = ele_grp_udt
TARGET_TAVE_LAYER_EVO = t_ave_layerd_data

TARGET_PICK_NODE_MONITOR = pick_from_node_monitor
TARGET_MHD_LENGH_SCALE = fem_mhd_l_scale

LIBS_UDT_UTIL = \
'$$(LIB_UTILS)'   \
'$$(LIB_VIZ)'    \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_UDT_UTIL = \
'$$(LIB_UTILS_FILE)'   \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_UDT_UTIL+=        '$$(LIB_KEMO_C)'
  LIB_FILES_UDT_UTIL+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_UDT_UTILS = \
m_FEM_utils.o \
m_ctl_data_diff_udt.o \
m_ctl_params_4_diff_udt.o \
\
set_ctl_diff_udt.o \
input_control_udt_diff.o \
\
divide_phys_by_delta_t.o

MOD_DIFF_FIELDS = \
main_udt_diff.o \
analyzer_udt_diff.o

MOD_AVE_FIELDS = \
main_ave_udt.o \
analyzer_ave_udt.o

MOD_PROD_FIELDS = \
m_FEM_utils.o \
m_ctl_data_product_udt.o \
m_ctl_params_4_prod_udt.o \
product_udt_fields.o \
analyzer_udt_product.o \
main_prod_udt.o

MOD_RATIO_FIELDS = \
m_FEM_utils.o \
m_ctl_data_product_udt.o \
m_ctl_params_4_prod_udt.o \
product_udt_fields.o \
analyzer_udt_ratio.o \
main_ratio_udt.o

MOD_CORRELATE_FIELDS = \
main_udt_correlation.o \
analyzer_udt_correlation.o \
\
m_all_layerd_correlate.o \
read_layer_evo_file_header.o \
transfer_correlate_field.o \
correlation_all_layerd_data.o

MOD_CORRELATE_ONECOMP = \
main_udt_correlate_1comp.o \
analyzer_udt_corr_1comp.o \
\
m_all_layerd_correlate.o \
read_layer_evo_file_header.o \
transfer_correlate_field.o \
correlation_all_layerd_data.o

MOD_MED_PATCH = \
main_med_grp_patch.o \
analyzer_med_grp_patch.o

MOD_MED_GRP_UDT = \
element_grouping_udt.o \
\
m_ctl_data_ele_grp_udt.o \
m_ctl_params_ele_grp_udt.o \
m_merdional_grouping_patch.o \
m_tave_SGS_model_coefs.o \
set_model_coef_to_med_patch.o \
read_layer_evo_file_header.o

MOD_T_AVE_M_COEF =    \
m_ctl_data_ele_grp_udt.o \
m_ctl_params_ele_grp_udt.o \
m_merdional_grouping_patch.o \
m_tave_SGS_model_coefs.o \
set_model_coef_to_med_patch.o \
read_layer_evo_file_header.o

MOD_PICK_NODE_MONITOR = \
pick_point_node_monitor.o

MOD_MHD_LENGH_SCALE = \
m_ctl_data_product_udt.o \
m_ctl_params_4_prod_udt.o \
FEM_MHD_length_scale.o \
\
analyzer_MHD_lengthscale.o \
main_MHD_lenghscale.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'UCD_UTILS_DIR = $(UCD_UTILS_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_DIFF_FIELDS = ' \
	'$$(BUILDDIR)/$(TARGET_DIFF_FIELDS)'       >> $(MAKENAME)
	@echo 'TARGET_AVE_FIELDS = '  \
	'$$(BUILDDIR)/$(TARGET_AVE_FIELDS)'        >> $(MAKENAME)
	@echo 'TARGET_PROD_FIELDS = '  \
	'$$(BUILDDIR)/$(TARGET_PROD_FIELDS)'       >> $(MAKENAME)
	@echo 'TARGET_RATIO_FIELDS = '  \
	'$$(BUILDDIR)/$(TARGET_RATIO_FIELDS)'      >> $(MAKENAME)
	@echo 'TARGET_CORR_FIELDS = '  \
	'$$(BUILDDIR)/$(TARGET_CORR_FIELDS)'       >> $(MAKENAME)
	@echo 'TARGET_CORR_ONECOMP = '  \
	'$$(BUILDDIR)/$(TARGET_CORR_ONECOMP)'      >> $(MAKENAME)
	@echo 'TARGET_MED_PATCH = '  \
	'$$(BUILDDIR)/$(TARGET_MED_PATCH)'         >> $(MAKENAME)
	@echo 'TARGET_MED_GRP_UDT = '  \
	'$$(BUILDDIR)/$(TARGET_MED_GRP_UDT)'       >> $(MAKENAME)
	@echo 'TARGET_TAVE_LAYER_EVO = '  \
	'$$(BUILDDIR)/$(TARGET_TAVE_LAYER_EVO)'    >> $(MAKENAME)
	@echo 'TARGET_PICK_NODE_MONITOR = '  \
	'$$(BUILDDIR)/$(TARGET_PICK_NODE_MONITOR)' >> $(MAKENAME)
	@echo 'TARGET_MHD_LENGH_SCALE = '  \
	'$$(BUILDDIR)/$(TARGET_MHD_LENGH_SCALE)'   >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo fem_utils:    '$$(TARGET_DIFF_FIELDS)'      \
	                    '$$(TARGET_AVE_FIELDS)'       \
	                    '$$(TARGET_PROD_FIELDS)'      \
	                    '$$(TARGET_RATIO_FIELDS)'     \
	                    '$$(TARGET_CORR_FIELDS)'      \
	                    '$$(TARGET_CORR_ONECOMP)'     \
	                    '$$(TARGET_MED_PATCH)'        \
	                    '$$(TARGET_MED_GRP_UDT)'      \
	                    '$$(TARGET_TAVE_LAYER_EVO)'   \
	                    '$$(TARGET_PICK_NODE_MONITOR)'\
	                    '$$(TARGET_MHD_LENGH_SCALE)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DIFF_FIELDS)': \
	'$$(MOD_DIFF_FIELDS)' '$$(MOD_UDT_UTILS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_DIFF_FIELDS)' \
	'$$(MOD_DIFF_FIELDS)' '$$(MOD_UDT_UTILS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	
	@echo '$$(TARGET_AVE_FIELDS)': \
	'$$(MOD_AVE_FIELDS)' '$$(MOD_UDT_UTILS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_AVE_FIELDS)' \
	'$$(MOD_AVE_FIELDS)' '$$(MOD_UDT_UTILS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_PROD_FIELDS)': '$$(MOD_PROD_FIELDS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PROD_FIELDS)' '$$(MOD_PROD_FIELDS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_RATIO_FIELDS)': '$$(MOD_RATIO_FIELDS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_RATIO_FIELDS)' '$$(MOD_RATIO_FIELDS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_CORR_FIELDS)':  \
	'$$(MOD_CORRELATE_FIELDS)' '$$(MOD_UDT_UTILS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_CORR_FIELDS)' \
	'$$(MOD_CORRELATE_FIELDS)' '$$(MOD_UDT_UTILS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_CORR_ONECOMP)':  \
	'$$(MOD_CORRELATE_ONECOMP)' '$$(MOD_UDT_UTILS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_CORR_ONECOMP)' \
	'$$(MOD_CORRELATE_ONECOMP)' '$$(MOD_UDT_UTILS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_MED_PATCH)':  \
	'$$(MOD_MED_PATCH)' '$$(MOD_UDT_UTILS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MED_PATCH)' \
	'$$(MOD_MED_PATCH)' '$$(MOD_UDT_UTILS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_MED_GRP_UDT)':  \
	'$$(MOD_MED_GRP_UDT)' '$$(MOD_UDT_UTILS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MED_GRP_UDT)' \
	'$$(MOD_MED_GRP_UDT)' '$$(MOD_UDT_UTILS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_TAVE_LAYER_EVO)': '$$(MOD_T_AVE_M_COEF)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_TAVE_LAYER_EVO)' \
	'$$(MOD_T_AVE_M_COEF)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_PICK_NODE_MONITOR): $$(MOD_PICK_NODE_MONITOR)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PICK_NODE_MONITOR)' \
	'$$(MOD_PICK_NODE_MONITOR) -L. $$(LIBS_UDT_UTIL)' \
    '$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_MHD_LENGH_SCALE)':  \
	'$$(MOD_MHD_LENGH_SCALE)' '$$(MOD_UDT_UTILS)' \
	'$$(LIB_FILES_UDT_UTIL)'  \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MHD_LENGH_SCALE)' \
	'$$(MOD_MHD_LENGH_SCALE)' '$$(MOD_UDT_UTILS)' \
	'-L. $$(LIBS_UDT_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_UDT_UTIL=  \\          >> $(MAKENAME)
	@echo  $(LIBS_UDT_UTIL)            >> $(MAKENAME)
	@echo  LIB_FILES_UDT_UTIL=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_UDT_UTIL)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_UDT_UTILS=  \\         >> $(MAKENAME)
	@echo  $(MOD_UDT_UTILS)           >> $(MAKENAME)
	@echo  MOD_DIFF_FIELDS=  \\       >> $(MAKENAME)
	@echo  $(MOD_DIFF_FIELDS)         >> $(MAKENAME)
	@echo  MOD_AVE_FIELDS=  \\        >> $(MAKENAME)
	@echo  $(MOD_AVE_FIELDS)          >> $(MAKENAME)
	@echo  MOD_PROD_FIELDS=  \\       >> $(MAKENAME)
	@echo  $(MOD_PROD_FIELDS)         >> $(MAKENAME)
	@echo  MOD_RATIO_FIELDS=  \\      >> $(MAKENAME)
	@echo  $(MOD_RATIO_FIELDS)        >> $(MAKENAME)
	@echo  MOD_CORRELATE_FIELDS=  \\  >> $(MAKENAME)
	@echo  $(MOD_CORRELATE_FIELDS)    >> $(MAKENAME)
	@echo  MOD_CORRELATE_ONECOMP=  \\ >> $(MAKENAME)
	@echo  $(MOD_CORRELATE_ONECOMP)   >> $(MAKENAME)
	@echo  MOD_MED_PATCH= \\          >> $(MAKENAME)
	@echo  $(MOD_MED_PATCH)           >> $(MAKENAME)
	@echo  MOD_MED_GRP_UDT= \\        >> $(MAKENAME)
	@echo  $(MOD_MED_GRP_UDT)         >> $(MAKENAME)
	@echo  MOD_T_AVE_M_COEF= \\       >> $(MAKENAME)
	@echo  $(MOD_T_AVE_M_COEF)        >> $(MAKENAME)
	@echo  MOD_PICK_NODE_MONITOR=  \\ >> $(MAKENAME)
	@echo  $(MOD_PICK_NODE_MONITOR)   >> $(MAKENAME)
	@echo  MOD_MHD_LENGH_SCALE=  \\   >> $(MAKENAME)
	@echo  $(MOD_MHD_LENGH_SCALE)     >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(UCD_UTILS_DIR)' $(SOURCES)
