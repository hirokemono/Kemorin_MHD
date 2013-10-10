#
#
#

MESHER_DIR = $$(PROG_DIR)/mesh_utilities

SUBDIRS = \
plane_layer \
cubed_sphere \
spherical_shell \
periodic_cube \
\
partitioner \
pickup_surface \
refiner

LIBS_MESH_UTIL = \
'$$(LIB_UTILS)'  \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIBS_MESH_CHECK = \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_MESH_UTIL = \
'$$(LIB_UTILS_FILE)'  \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

LIB_FILES_MESH_CHECK = \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_MESH_UTIL+=        '$$(LIB_KEMO_C)'
  LIBS_MESH_CHECK+=       '$$(LIB_KEMO_C)'
  LIB_FILES_MESH_UTIL+=   '$$(LIB_KEMO_C_FILE)'
  LIB_FILES_MESH_CHECK+=  '$$(LIB_KEMO_C_FILE)'
endif

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo '#' >> $(MAKENAME)
	@echo 'MESHER_DIR = $(MESHER_DIR)'   >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)

target_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		target_list \
		) \
	done

target_task:
	@echo mesh_utils: \
	'cubed_sph_utils sph_utils plane_mesh partitioners' \
	'$$(TARGET_REFINE)    $$(TARGET_REF_PARA_TBL)' \
	'$$(TARGET_PICK_SURF) $$(TARGET_PERI_CUBE)' \
	>> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done


lib_name:
	@echo  LIBS_MESH_UTIL=  \\          >> $(MAKENAME)
	@echo  $(LIBS_MESH_UTIL)            >> $(MAKENAME)
	@echo  LIBS_MESH_CHECK=  \\         >> $(MAKENAME)
	@echo  $(LIBS_MESH_CHECK)           >> $(MAKENAME)
	@echo  LIB_FILES_MESH_UTIL=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_MESH_UTIL)       >> $(MAKENAME)
	@echo  LIB_FILES_MESH_CHECK=  \\    >> $(MAKENAME)
	@echo  $(LIB_FILES_MESH_CHECK)      >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done



clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
