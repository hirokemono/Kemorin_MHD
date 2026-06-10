#
#
#
G_FILTER_PLUG_DIR = $$(PROG_DIR)/construct_filter

LIBS_COMMUTE = \
'$$(LIB_UTILS)'  \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_COMMUTE = \
'$$(LIB_UTILS_FILE)'  \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_COMMUTE+=        '$$(LIB_KEMO_C)'
  LIB_FILES_COMMUTE+=   '$$(LIB_KEMO_C_FILE)'
endif

SUBDIRS = \
commute_3d_filter     \
sort_commute3d_filter \
moments3d_transfer    \
filter3d_transfer     \
filter3d_trans_single \
filter_coef_transfer  \
\
commute_z_filter  \
test_plane_delta_z

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'G_FILTER_PLUG_DIR = $(G_FILTER_PLUG_DIR)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_task:
	@echo const_filteres: const_3d_filter const_z_filter >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo const_3d_filter:   \
	'$$(TARGET_3D_FILTER)        $$(TARGET_SORT_3D_FILTER)'       \
	'$$(TARGET_NEWMESH_FILTER3D) $$(TARGET_NEWMESH_SGL_FILTER3D)' \
	'$$(TARGET_NEWMESH_MOMENTS)  $$(TARGET_FILTER_TRANS)   '  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo const_z_filter:  \
	'$$(TARGET_Z_FILTER)'  \
	'$$(TARGET_TEST_PLANE_DZ)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done


lib_name:
	@echo  LIBS_COMMUTE=  \\          >> $(MAKENAME)
	@echo  $(LIBS_COMMUTE)            >> $(MAKENAME)
	@echo  LIB_FILES_COMMUTE=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_COMMUTE)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

lib_tasks:

mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct compile list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		depends; \
		) \
	done


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
