#
#
#

UTILS_SRCDIR =  $$(F_SRCDIR)/UTILS_src

LIB_UTILS =       -lkemo_utils
LIB_UTILS_FILE = libkemo_utils.a

SUBDIRS = \
CONST_FILTER_src \
INTERPOLATE_src  \
\
cubed_sph      \
spherical_grid \
PLANE_src      \
PARTITIONER    \
MERGE          \
REFINER        \
pick_surface   \
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
		make \
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
		make lib_name \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)'; \
		 ) \
	done

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_UTILS_FILE): $$(MOD_KEMO_UTILS)'>> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		lib_archve; \
		) \
	done

mod_list:
	@echo '' >> $(MAKENAME)
	@echo MOD_KEMO_UTILS=  \\ >> $(MAKENAME)
	@echo  \
	'$$(MOD_GEN_FILTER)'   \
	'$$(MOD_CUBED_SPH)'    \
	'$$(MOD_ITP_TABLE)'    \
	'$$(MOD_MERGE)'        \
	'$$(MOD_PART_KEMO)'    \
	'$$(MOD_PICK_SURFACE)' \
	'$$(MOD_PLANE_MODEL)'  \
	'$$(MOD_REFINE)'       \
	'$$(MOD_SPH_GRID)'     \
	'$$(MOD_DRAW_PG)'      >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		make \
		MAKENAME='$(MAKENAME)' \
		mod_list; \
		) \
	done


module:
	@echo '' >> $(MAKENAME)
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
