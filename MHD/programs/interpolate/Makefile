#
#
#
ITP_PLUG_DIR = $$(PROG_DIR)/interpolate

SUBDIRS = \
const_interpolate_table \
const_ele_interpolate_tbl \
\
interpolate_field   \
interpolate_restart \
interpolate_node    \
interpolate_element

LIBS_INTERPOLATE = \
'$$(LIB_UTILS)'    \
'$$(LIB_PARA)'     \
'$$(LIB_SOLVER)'   \
'$$(LIB_MESH)'

LIB_FILES_INTERPOLATE = \
'$$(LIB_UTILS_FILE)'    \
'$$(LIB_PARA_FILE)'     \
'$$(LIB_SOLVER_FILE)'   \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_INTERPOLATE+=        '$$(LIB_KEMO_C)'
  LIB_FILES_INTERPOLATE+=   '$$(LIB_KEMO_C_FILE)'
endif

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_PLUG_DIR = $(ITP_PLUG_DIR)'      >> $(MAKENAME)
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
	@echo interpolate_tools: \
	 '$$(TARGET_GEN_ITP_TABLE)' \
	 '$$(TARGET_GEN_ELE_TABLE)' \
	 '$$(TARGET_ITP_FIELD)'     \
	 '$$(TARGET_ITP_RESTART)'   \
	 '$$(TARGET_ITP_MESH)'      \
	 '$$(TARGET_ITP_ELEMENT)'   >> $(MAKENAME)
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
	@echo  LIBS_INTERPOLATE=  \\          >> $(MAKENAME)
	@echo  $(LIBS_INTERPOLATE)            >> $(MAKENAME)
	@echo  LIB_FILES_INTERPOLATE=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_INTERPOLATE)       >> $(MAKENAME)
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
