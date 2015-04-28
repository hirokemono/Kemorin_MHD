#
#
#

CONST_FILTER_3D_DIR = $$(G_FILTER_PLUG_DIR)/const_3d_filter

TARGET_3D_COMM_X =      commute3d
TARGET_SORT_3D_FILTER = sort_commute3d
TARGET_NEWMESH_FILTER = filter3d_transfer
TARGET_NEWMESH_MOMENT = moments3d_transfer
TARGET_FILTER_TRANS =   filter_coef_trans

TARGET_NEWMESH_FLT_SGL = filter3d_trans_single

SOURCES = $(shell ls *.f90)

MOD_SORT_FILTER3D = \
single_sorting_3dfilter.o \
analyzer_sorting_3dfilter.o \
m_ctl_data_gen_3d_filter.o \
\
set_ctl_gen_filter.o

MOD_GEN_FILTER3D_FX= \
main_gen_filter.o \
analyzer_gen_filter.o \
m_ctl_data_gen_3d_filter.o \
\
input_control_gen_filter.o \
\
set_ctl_gen_filter.o


MOD_FILTER_NMESH_SGL= \
single_filter_newdom.o \
analyzer_filter_newdom_sgl.o \
local_newdomain_filter.o \
bcast_nodes_for_trans.o \
\
m_ctl_data_newdomain_filter.o \
m_ctl_param_newdom_filter.o

MOD_FILTER_NEWMESH= \
main_filter_newdomains.o \
analyzer_filter_newdomains.o \
local_newdomain_filter.o \
bcast_nodes_for_trans.o \
\
m_ctl_data_newdomain_filter.o \
m_ctl_param_newdom_filter.o

MOD_FILTER_COEF_TRANS= \
main_trans_filter_coef.o \
analyzer_trans_filter_coef.o \
bcast_nodes_for_trans.o \
\
m_ctl_data_newdomain_filter.o \
m_ctl_param_newdom_filter.o


MOD_MOMENT_NEWMESH= \
main_moments_newdomains.o \
analyzer_moments_newdomains.o \
\
m_ctl_data_newdomain_filter.o \
m_ctl_param_newdom_filter.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'CONST_FILTER_3D_DIR = $(CONST_FILTER_3D_DIR)'   >> $(MAKENAME)

target_list:
	@echo 'TARGET_SORT_3D_FILTER =  $$(BUILDDIR)/$(TARGET_SORT_3D_FILTER)'  \
	>> $(MAKENAME)
	@echo 'TARGET_3D_COMM_X =       $$(BUILDDIR)/$(TARGET_3D_COMM_X)'       \
	>> $(MAKENAME)
	@echo 'TARGET_NEWMESH_FILTER =  $$(BUILDDIR)/$(TARGET_NEWMESH_FILTER)'  \
	>> $(MAKENAME)
	@echo 'TARGET_NEWMESH_FLT_SGL = $$(BUILDDIR)/$(TARGET_NEWMESH_FLT_SGL)' \
	>> $(MAKENAME)
	@echo 'TARGET_NEWMESH_MOMENT =  $$(BUILDDIR)/$(TARGET_NEWMESH_MOMENT)'  \
	>> $(MAKENAME)
	@echo 'TARGET_FILTER_TRANS =    $$(BUILDDIR)/$(TARGET_FILTER_TRANS)'    \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo const_3d_filter:   \
	'$$(TARGET_SORT_3D_FILTER)   $$(TARGET_3D_COMM_X)'      \
	'$$(TARGET_NEWMESH_FILTER)   $$(TARGET_NEWMESH_FLT_SGL)' \
	'$$(TARGET_NEWMESH_MOMENT)   $$(TARGET_FILTER_TRANS)   '  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_3D_COMM_X)': \
	'$$(MOD_GEN_FILTER3D_FX)' '$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_3D_COMM_X)' \
	'$$(MOD_GEN_FILTER3D_FX)' '-L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SORT_3D_FILTER)':  \
	'$$(MOD_SORT_FILTER3D)' '$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SORT_3D_FILTER)' \
	'$$(MOD_SORT_FILTER3D)' '-L.  $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_NEWMESH_MOMENT)': \
	'$$(MOD_MOMENT_NEWMESH)' '$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NEWMESH_MOMENT)' \
	'$$(MOD_MOMENT_NEWMESH)' '-L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_NEWMESH_FILTER)': \
	'$$(MOD_FILTER_NEWMESH)' '$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NEWMESH_FILTER)' \
	'$$(MOD_FILTER_NEWMESH)' '-L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_FILTER_TRANS)': \
	'$$(MOD_FILTER_COEF_TRANS)' '$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_FILTER_TRANS)' \
	'$$(MOD_FILTER_COEF_TRANS)' '-L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_NEWMESH_FLT_SGL)':  \
	'$$(MOD_FILTER_NMESH_SGL)' '$$(LIB_FILES_COMMUTE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NEWMESH_FLT_SGL)' \
	'$$(MOD_FILTER_NMESH_SGL)' '-L. $$(LIBS_COMMUTE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GEN_FILTER3D_FX=  \\   >> $(MAKENAME)
	@echo  $(MOD_GEN_FILTER3D_FX)     >> $(MAKENAME)
	@echo  MOD_SORT_FILTER3D=  \\     >> $(MAKENAME)
	@echo  $(MOD_SORT_FILTER3D)       >> $(MAKENAME)
	@echo  MOD_FILTER_COEF_TRANS=  \\ >> $(MAKENAME)
	@echo  $(MOD_FILTER_COEF_TRANS)   >> $(MAKENAME)
	@echo  MOD_FILTER_NEWMESH=  \\    >> $(MAKENAME)
	@echo  $(MOD_FILTER_NEWMESH)      >> $(MAKENAME)
	@echo  MOD_FILTER_NMESH_SGL=  \\  >> $(MAKENAME)
	@echo  $(MOD_FILTER_NMESH_SGL)    >> $(MAKENAME)
	@echo  MOD_MOMENT_NEWMESH=  \\    >> $(MAKENAME)
	@echo  $(MOD_MOMENT_NEWMESH)      >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CONST_FILTER_3D_DIR)' $(SOURCES)
