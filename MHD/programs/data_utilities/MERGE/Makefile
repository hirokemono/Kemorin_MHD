#
#
#

MERGE_SRCDIR =     $$(DATA_UTILS_DIR)/MERGE

TARGET_MERGE =  merge_field
TARGET_M_MESH = merge_mesh
TARGET_UPDATE_RST_SGL = sgl_update_restart

TARGET_NEWSPH = assemble_sph
TARGET_NEWUDT = assemble_udt
TARGET_NEWRST = assemble_restart


TARGET_UPDATE_RST =   update_restart
TARGET_ADD_TIME_SPH = add_time_4_fld

LIBS_MERGE = \

LIB_FILES_MERGE = \
'$$(LIB_UTILS_FILE)' \
'$$(LIB_PARA_FILE)'     \
'$$(LIB_SOLVER_FILE)'   \
'$$(LIB_MESH_FILE)'

LIBS_MERGE = \
'$$(LIB_UTILS)'     \
'$$(LIB_PARA)'      \
'$$(LIB_SOLVER)'    \
'$$(LIB_MESH)'

ifdef FORTRAN_C_CONNECT
  LIBS_MERGE+=        '$$(LIB_KEMO_C)'
  LIB_FILES_MERGE+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_MERGE_UCD = \
main_merge_udt.o \
analyzer_merge_udt.o

MOD_MERGE_MESH = \
assemble_mesh.o


MOD_NEWRST = \
main_assemble_rst.o \
analyzer_assemble_rst.o

MOD_UPDATE_RST = \
main_update_restart.o \
analyzer_update_rst.o

MOD_UPDATE_RST_SGL = \
update_restart.o

MOD_NEWSPH = \
main_assemble_sph.o \
analyzer_assemble_sph.o

MOD_NEWUDT = \
main_assemble_udt.o \
analyzer_assemble_udt.o

MOD_ADD_TIME_SPH = \
add_time_data_to_sph.o



#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MERGE_SRCDIR = $(MERGE_SRCDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_MERGE =  $$(BUILDDIR)/$(TARGET_MERGE)'  >> $(MAKENAME)
	@echo 'TARGET_M_MESH = $$(BUILDDIR)/$(TARGET_M_MESH)' >> $(MAKENAME)
	@echo 'TARGET_NEWUDT = $$(BUILDDIR)/$(TARGET_NEWUDT)' >> $(MAKENAME)
	@echo 'TARGET_NEWRST = $$(BUILDDIR)/$(TARGET_NEWRST)' >> $(MAKENAME)
	@echo 'TARGET_UPDATE_RST_SGL = $$(BUILDDIR)/$(TARGET_UPDATE_RST_SGL)' >> $(MAKENAME)
	@echo 'TARGET_NEWSPH = $$(BUILDDIR)/$(TARGET_NEWSPH)' >> $(MAKENAME)
	@echo 'TARGET_UPDATE_RST =   $$(BUILDDIR)/$(TARGET_UPDATE_RST)'   >> $(MAKENAME)
	@echo 'TARGET_ADD_TIME_SPH = $$(BUILDDIR)/$(TARGET_ADD_TIME_SPH)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo assembles: \
	'$$(TARGET_MERGE)        $$(TARGET_M_MESH)'            \
	'$$(TARGET_NEWUDT)       $$(TARGET_NEWRST)'            \
	'$$(TARGET_NEWSPH)       $$(TARGET_UPDATE_RST)'        \
	'$$(TARGET_ADD_TIME_SPH) $$(TARGET_UPDATE_RST_SGL)'    \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MERGE)': '$$(MOD_MERGE_UCD) $$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MERGE)' '$$(MOD_MERGE_UCD)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_M_MESH)': '$$(MOD_MERGE_MESH) $$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_M_MESH)' '$$(MOD_MERGE_MESH)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_NEWUDT)': '$$(MOD_NEWUDT) $$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NEWUDT)' '$$(MOD_NEWUDT)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_NEWRST)': '$$(MOD_NEWRST) $$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NEWRST)' '$$(MOD_NEWRST)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_UPDATE_RST)': '$$(MOD_UPDATE_RST) $$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_UPDATE_RST)' '$$(MOD_UPDATE_RST)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_UPDATE_RST_SGL)': '$$(MOD_UPDATE_RST_SGL) $$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_UPDATE_RST_SGL)' '$$(MOD_UPDATE_RST_SGL)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_ADD_TIME_SPH)': '$$(MOD_ADD_TIME_SPH)' \
	'$$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_ADD_TIME_SPH)' '$$(MOD_ADD_TIME_SPH)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_NEWSPH)': '$$(MOD_NEWSPH)' \
	'$$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NEWSPH)' '$$(MOD_NEWSPH)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:
	@echo  LIBS_MERGE=  \\     >> $(MAKENAME)
	@echo  $(LIBS_MERGE)       >> $(MAKENAME)
	@echo  LIB_FILES_MERGE=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_MERGE)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_MERGE_UCD=  \\    >> $(MAKENAME)
	@echo  $(MOD_MERGE_UCD)      >> $(MAKENAME)
	@echo  MOD_MERGE_MESH=  \\   >> $(MAKENAME)
	@echo  $(MOD_MERGE_MESH)     >> $(MAKENAME)
	@echo  MOD_NEWUDT=  \\       >> $(MAKENAME)
	@echo  $(MOD_NEWUDT)         >> $(MAKENAME)
	@echo  MOD_NEWRST=  \\       >> $(MAKENAME)
	@echo  $(MOD_NEWRST)         >> $(MAKENAME)
	@echo  MOD_UPDATE_RST=  \\   >> $(MAKENAME)
	@echo  $(MOD_UPDATE_RST)     >> $(MAKENAME)
	@echo  MOD_UPDATE_RST_SGL=  \\   >> $(MAKENAME)
	@echo  $(MOD_UPDATE_RST_SGL)     >> $(MAKENAME)
	@echo  MOD_NEWSPH=  \\       >> $(MAKENAME)
	@echo  $(MOD_NEWSPH)         >> $(MAKENAME)
	@echo  MOD_ADD_TIME_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_ADD_TIME_SPH)   >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MERGE_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
