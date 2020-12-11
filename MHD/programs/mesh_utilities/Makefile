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
parallel_repartition \
partitioner \
pickup_surface \
refiner \
\
LIC_noise

LIBS_MESH_CHECK = \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIBS_MESH_UTIL = \
'$$(LIB_UTILS)'  \
'$$(LIB_VIZ)'    \
'$$(LIBS_MESH_CHECK)'

LIBS_CUBE_NOISE = \
'$$(LIBS_MESH_UTIL)'  \
'$$(LIB_MT_STREAM)'

LIB_FILES_MESH_CHECK = \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

LIB_FILES_MESH_UTIL = \
'$$(LIB_UTILS_FILE)'  \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_FILES_MESH_CHECK)'

LIB_FILES_CUBE_NOISE = \
'$$(LIB_FILES_MESH_UTIL)'  \
'$$(LIB_MT_STREAM_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_MESH_UTIL+=        '$$(LIB_KEMO_C)'
  LIBS_MESH_CHECK+=       '$$(LIB_KEMO_C)'
  LIBS_CUBE_NOISE+=       '$$(LIB_KEMO_C)'
  LIB_FILES_MESH_UTIL+=   '$$(LIB_KEMO_C_FILE)'
  LIB_FILES_MESH_CHECK+=  '$$(LIB_KEMO_C_FILE)'
  LIB_FILES_CUBE_NOISE+=  '$$(LIB_KEMO_C_FILE)'
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

target_task:
	@echo mesh_utils: \
	'noise_utils cubed_sph_utils sph_utils plane_mesh ' \
	'partitioners repartition refineres pickup_sf' \
	'$$(TARGET_PERI_CUBE)' \
	>> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done


lib_name:
	@echo  LIBS_MESH_CHECK=  \\         >> $(MAKENAME)
	@echo  $(LIBS_MESH_CHECK)           >> $(MAKENAME)
	@echo  LIB_FILES_MESH_CHECK=  \\    >> $(MAKENAME)
	@echo  $(LIB_FILES_MESH_CHECK)      >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_MESH_UTIL=  \\          >> $(MAKENAME)
	@echo  $(LIBS_MESH_UTIL)            >> $(MAKENAME)
	@echo  LIB_FILES_MESH_UTIL=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_MESH_UTIL)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIBS_CUBE_NOISE=  \\         >> $(MAKENAME)
	@echo  $(LIBS_CUBE_NOISE)           >> $(MAKENAME)
	@echo  LIB_FILES_CUBE_NOISE=  \\    >> $(MAKENAME)
	@echo  $(LIB_FILES_CUBE_NOISE)      >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done


module:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		module; \
		) \
	done

depends:
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct dependencies for ' $$dir ; \
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
