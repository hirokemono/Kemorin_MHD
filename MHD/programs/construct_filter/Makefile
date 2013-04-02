#
#
#

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
const_3d_filter \
const_z_filter


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'G_FILTER_PLUG_DIR = $$(PROG_DIR)/construct_filter' >> $(MAKENAME)
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
	@echo '#' >> $(MAKENAME)

target_task:
	@echo const_filteres: const_3d_filter const_z_filter >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
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
