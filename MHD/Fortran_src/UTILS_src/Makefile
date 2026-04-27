#
#
#

UTILS_SRCDIR =  $$(F_SRCDIR)/UTILS_src

LIB_UTILS =       -lkemo_utils
LIB_UTILS_FILE = libkemo_utils.a

MODULE_LIST=   'MOD_KEMO_UTILS'
UPPER_MODULES=

SUBDIR_NO_PG = \
CONST_FILTER_src \
INTERPOLATE_src  \
\
cubed_sph      \
PLANE_src      \
PARTITIONER    \
MERGE          \
REFINER

SUBDIRS = \
$(SUBDIR_NO_PG) \
draw_pg_f90


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'UTILS_SRCDIR =  $(UTILS_SRCDIR)'    >> $(MAKENAME)
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

lib_name:
	@echo  'LIB_UTILS =       $(LIB_UTILS)'      >> $(MAKENAME)
	@echo  'LIB_UTILS_FILE =  $(LIB_UTILS_FILE)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in  $(SUBDIRS); do \
		( cd $${dir};  \
		echo 'generate library list for ' $$dir ; \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		lib_name; \
		) \
	done

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)
	@for dir in draw_pg_f90; do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_tasks \
		) \
	done
	@echo '#' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_UTILS_FILE): $$('$(MODULE_LIST)')'>> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIR_NO_PG); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve; \
		) \
	done

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  $(MODULE_LIST)= >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(MODULE_LIST)' \
		mod_list; \
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
		MAKENAME='$(MAKENAME)' \
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
