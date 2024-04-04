#
#
#

RAYLEIGH_LINK_DIR = $$(PROG_DIR)/Rayleigh_link

TARGET_FEM_MESH_RAYLEIGH =  make_rayleigh_mesh
TARGET_CVT_RAYLEIGH_SPH =   convert_rayleigh_sph
TARGET_CVT_RAYLEIGH_FLD =   convert_rayleigh_field
TARGET_RAYLEIGH_VIZ =       viz_rayleigh
TARGET_PICK_RAYLEIGH_SPH =  pickup_rayleigh_spectr

LIB_RAYLEIGH_LINK =       -lcalipso_rayleigh
LIB_RAYLEIGH_LINK_FILE =   libcalipso_rayleigh.a


LIBS_RAYLEIGH_LINK = \
'$$(LIB_RAYLEIGH_LINK)'    \
'$$(LIB_VIZ)'    \
'$$(LIB_UTILS)'     \
'$$(LIB_PARA)'      \
'$$(LIB_SOLVER)'    \
'$$(LIB_MESH)'      \
'$$(LIB_MT_STREAM)' \
'$$(LIB_FFTPACK)'   \
'$$(LIB_NETLIB)'

LIB_FILES_RAYLEIGH_LINK = \
'$$(LIB_RAYLEIGH_LINK_FILE)'    \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_UTILS_FILE)' \
'$$(LIB_PARA_FILE)'     \
'$$(LIB_SOLVER_FILE)'   \
'$$(LIB_MESH_FILE)'     \
'$$(LIB_MT_STREAM_FILE)' \
'$$(LIB_FFTPACK_FILE)'   \
'$$(LIB_NETLIB_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_RAYLEIGH_LINK+=        '$$(LIB_KEMO_C)'
  LIB_FILES_RAYLEIGH_LINK+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_RAYLEIGH_CONNECT = \
t_rayleigh_field_IO.o \
palallel_conf_4_rayleigh.o \
const_fem_nodes_4_rayleigh.o \
\
t_convert_from_rayleigh.o \
t_rayleigh_restart_IO.o \
t_viz_4_rayleigh.o \
t_ctl_data_rayleigh_vizs.o \
t_rayleigh_field_address.o \
\
t_ctl_pick_rayleigh_spectr.o \
t_picked_rayleigh_parameter.o \
t_picked_rayleigh_spectr.o \
\
rayleigh_restart_IO.o \
rayleigh99_rst_param_IO.o \
MPI_read_rayleigh_restart.o \
convert_from_rayleigh_rst.o \
FEM_analyzer_viz_rayleigh.o


MOD_FEM_MESH_RAYLEIGH = \
main_gen_FEM_4_rayleigh.o \
analyzer_gen_FEM_4_rayleigh.o

MOD_CVT_RAYLEIGH_SPH = \
main_cvt_from_rayleigh_sph.o \
analyzer_rayleigh_cvt_sph.o

MOD_CVT_RAYLEIGH_FLD = \
main_cvt_from_rayleigh_fld.o \
analyzer_rayleigh_cvt_fld.o


MOD_RAYLEIGH_VIZ = \
main_visualize_rayleigh.o \
analyzer_viz_rayleigh.o

MOD_PICK_RAYLEIGH_SPH = \
main_pickup_rayleigh_spectr.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'RAYLEIGH_LINK_DIR = $(RAYLEIGH_LINK_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FEM_MESH_RAYLEIGH = $$(BUILDDIR)/$(TARGET_FEM_MESH_RAYLEIGH)'     \
	>> $(MAKENAME)
	@echo 'TARGET_CVT_RAYLEIGH_SPH = $$(BUILDDIR)/$(TARGET_CVT_RAYLEIGH_SPH)' >> $(MAKENAME)
	@echo 'TARGET_CVT_RAYLEIGH_FLD = $$(BUILDDIR)/$(TARGET_CVT_RAYLEIGH_FLD)' >> $(MAKENAME)
	@echo 'TARGET_RAYLEIGH_VIZ = $$(BUILDDIR)/$(TARGET_RAYLEIGH_VIZ)' >> $(MAKENAME)
	@echo 'TARGET_PICK_RAYLEIGH_SPH = $$(BUILDDIR)/$(TARGET_PICK_RAYLEIGH_SPH)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

lib_name:
	@echo  LIB_RAYLEIGH_LINK= $(LIB_RAYLEIGH_LINK)       >> $(MAKENAME)
	@echo  LIB_RAYLEIGH_LINK_FILE= $(LIB_RAYLEIGH_LINK_FILE)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_RAYLEIGH_LINK=  \\     >> $(MAKENAME)
	@echo  $(LIBS_RAYLEIGH_LINK)       >> $(MAKENAME)
	@echo  LIB_FILES_RAYLEIGH_LINK=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_RAYLEIGH_LINK)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

target_task:
	@echo rayleigh_link: \
	'$$(TARGET_CVT_RAYLEIGH_SPH) $$(TARGET_CVT_RAYLEIGH_FLD)' \
	'$$(TARGET_FEM_MESH_RAYLEIGH)  $$(TARGET_RAYLEIGH_VIZ)' \
	'$$(TARGET_PICK_RAYLEIGH_SPH)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FEM_MESH_RAYLEIGH)': '$$(MOD_FEM_MESH_RAYLEIGH)' \
	'$$(LIB_FILES_RAYLEIGH_LINK)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FEM_MESH_RAYLEIGH)' \
	'$$(MOD_FEM_MESH_RAYLEIGH)' \
	'-L. $$(LIBS_RAYLEIGH_LINK)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CVT_RAYLEIGH_SPH)': '$$(MOD_CVT_RAYLEIGH_SPH)' \
	'$$(LIB_FILES_RAYLEIGH_LINK)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CVT_RAYLEIGH_SPH)' '$$(MOD_CVT_RAYLEIGH_SPH)' \
	'-L. $$(LIBS_RAYLEIGH_LINK)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CVT_RAYLEIGH_FLD)': '$$(MOD_CVT_RAYLEIGH_FLD)' \
	'$$(LIB_FILES_RAYLEIGH_LINK)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CVT_RAYLEIGH_FLD)' '$$(MOD_CVT_RAYLEIGH_FLD)' \
	'-L. $$(LIBS_RAYLEIGH_LINK)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_RAYLEIGH_VIZ)': '$$(MOD_RAYLEIGH_VIZ)' \
	'$$(LIB_FILES_RAYLEIGH_LINK)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_RAYLEIGH_VIZ)' '$$(MOD_RAYLEIGH_VIZ)' \
	'-L. $$(LIBS_RAYLEIGH_LINK)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PICK_RAYLEIGH_SPH)': '$$(MOD_PICK_RAYLEIGH_SPH)' \
	'$$(LIB_FILES_RAYLEIGH_LINK)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PICK_RAYLEIGH_SPH)' '$$(MOD_PICK_RAYLEIGH_SPH)' \
	'-L. $$(LIBS_RAYLEIGH_LINK)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_RAYLEIGH_LINK_FILE): $$(MOD_RAYLEIGH_CONNECT)'>> $(MAKENAME)

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_RAYLEIGH_CONNECT)' >> $(MAKENAME)


mod_list:
	@echo  MOD_RAYLEIGH_CONNECT=  \\ >> $(MAKENAME)
	@echo  $(MOD_RAYLEIGH_CONNECT)  >> $(MAKENAME)
	@echo  MOD_FEM_MESH_RAYLEIGH=  \\ >> $(MAKENAME)
	@echo  $(MOD_FEM_MESH_RAYLEIGH) >> $(MAKENAME)
	@echo  MOD_CVT_RAYLEIGH_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_CVT_RAYLEIGH_SPH)   >> $(MAKENAME)
	@echo  MOD_CVT_RAYLEIGH_FLD=  \\ >> $(MAKENAME)
	@echo  $(MOD_CVT_RAYLEIGH_FLD)   >> $(MAKENAME)
	@echo  MOD_RAYLEIGH_VIZ=  \\ >> $(MAKENAME)
	@echo  $(MOD_RAYLEIGH_VIZ)   >> $(MAKENAME)
	@echo  MOD_PICK_RAYLEIGH_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_PICK_RAYLEIGH_SPH)   >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(RAYLEIGH_LINK_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
