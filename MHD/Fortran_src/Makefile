#! /bin/csh
#
#

F_SRCDIR =  $$(MHDDIR)/Fortran_src

LIB_CALYPSO =      -lcalipso_kemo
LIB_CALYPSO_FILE = libcalipso_kemo.a

MODULE_LIST=   'MOD_CALYPSO_F'
UPPER_MODULES=

SUBDIRS =    \
\
CORE_src     \
SOLVER_src   \
PARALLEL_src \
MHD_src      \
VIZ_src      \
UTILS_src

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'F_SRCDIR =  $(F_SRCDIR)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)'     \
		MAKENAME='$(MAKENAME)'   \
		64BIT_INT='$(64BIT_INT)'   \
		dir_list; \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_CALYPSO =      $(LIB_CALYPSO)'      >> $(MAKENAME)
	@echo  'LIB_CALYPSO_FILE = $(LIB_CALYPSO_FILE)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo 'create library file name list...'
	@for dir in  $(SUBDIRS) ; do \
		( \
		echo 'generate library list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)' \
		MAKENAME='$(MAKENAME)' \
		64BIT_INT='$(64BIT_INT)'   \
		lib_name; \
		 ) \
	done

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		64BIT_INT='$(64BIT_INT)'   \
		lib_tasks \
		) \
	done

libtarget:
	@echo  '$$(LIB_CALYPSO_FILE): $$('$(MODULE_LIST)')' >> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		64BIT_INT='$(64BIT_INT)'   \
		lib_archve \
		) \
	done

mod_list:
	@echo  $(MODULE_LIST)= >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		UPPER_MODULES='$(MODULE_LIST)' \
		64BIT_INT='$(64BIT_INT)'   \
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
		64BIT_INT='$(64BIT_INT)'   \
		depends; \
		) \
	done

clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		64BIT_INT='$(64BIT_INT)'   \
		clean \
		) \
	done
	
	rm -f *.o *.mod *~ *.par *.diag *.a
