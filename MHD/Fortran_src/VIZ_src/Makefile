#
#
#

LIB_VIZ =       -lkemo_viz
LIB_VIZ_FILE = libkemo_viz.a

SUBDIRS = \
surfacing       \
fieldline        \
volume_rendering \
LIC_PVR          \
\
Fortran2003

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'VIZ_SRCDIR =    $$(F_SRCDIR)/VIZ_src'      >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@for dir in $(SUBDIRS) ; do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKEDIR='$(MAKEDIR)'     \
		MAKENAME='$(MAKENAME)'   \
		dir_list \
		 ) \
	done
	@echo '#' >> $(MAKENAME)

lib_name:
	@echo  'LIB_VIZ =      $(LIB_VIZ)'      >> $(MAKENAME)
	@echo  'LIB_VIZ_FILE = $(LIB_VIZ_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_VIZ_FILE): $$(MOD_VIZ)' >> $(MAKENAME)

lib_archve:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		lib_archve \
		) \
	done;

mod_list:
	@echo MOD_VIZ= \\ >> $(MAKENAME)
	@echo '$$(MOD_SURFACING)'  \\ >> $(MAKENAME)
	@echo '$$(MOD_FIELDLINE)'  \\ >> $(MAKENAME)
	@echo '$$(MOD_PVR)'        \\ >> $(MAKENAME)
	@echo '$$(MOD_LIC)'        \\ >> $(MAKENAME)
	@echo '$$(MOD_VIZ_F2003)'  \\ >> $(MAKENAME)
	@echo >> $(MAKENAME)
	
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
		module \
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
		depends \
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
